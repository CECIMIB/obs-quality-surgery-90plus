# Metaciencia en nonagenarios y centenarios — Manuscrito 1
# Bivariado exploratorio, 30 desenlaces x 343 predictores. Versión 2026-11-14.
# Ejecución: Rscript analisis_bivariado_P1_V7.R "matriz_extendida V7 - TOTAL FINAL.xlsx" "diccionario_variables_V7_ES.csv" "salidas_R"
# Requiere readxl y openxlsx. No usar este script como código ya validado hasta cotejarlo en R.
args <- commandArgs(trailingOnly=TRUE)
infile <- if(length(args)>=1) args[1] else "matriz_extendida V7 - TOTAL FINAL.xlsx"
dfile <- if(length(args)>=2) args[2] else "diccionario_variables_V7_ES.csv"
outdir <- if(length(args)>=3) args[3] else "salidas_R"
if(!file.exists(infile) || !file.exists(dfile)) stop("Faltan matriz o diccionario")
for(p in c("readxl","openxlsx")) if(!requireNamespace(p,quietly=TRUE))stop("Instale ",p)
dir.create(outdir,showWarnings=FALSE,recursive=TRUE)

X <- readxl::read_excel(infile,sheet=1,col_names=FALSE,col_types="text",.name_repair="minimal")
stopifnot(ncol(X)==375L)
heads <- vapply(X,function(a) if(is.na(a[1])) "" else trimws(a[1]),character(1))
D <- X[-1,,drop=FALSE]
D <- D[!is.na(D[[1]]) & nzchar(trimws(D[[1]])),,drop=FALSE]
stopifnot(nrow(D)==128L)
dict <- read.csv(dfile,fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
stopifnot(nrow(dict)==375,all(dict$columna_excel==vapply(1:375,function(j){
  s<-"";while(j>0){a<-(j-1)%%26;s<-paste0(LETTERS[a+1],s);j<-(j-1)%/%26};s
},character(1))))
stopifnot(all(trimws(heads)==trimws(dict$nombre_original)))
# Los códigos originales se preservan, nunca se reemplazan en el archivo de entrada.
textv <- function(x) {z<-trimws(as.character(x));z[is.na(z)]<-"";z}
miss <- c("","no reportado","no aplica","n/a","na","sin dato","sin datos",
          "no disponible","not reported","not available","nr","n.r.",
          "no informado","sin información","sin informacion")
is_missing <- function(v) tolower(textv(v)) %in% miss
numparse <- function(v,nm){
 x<-textv(v);x[is_missing(x)]<-NA_character_
 if(nm %in% c("n_categorias","n_dos_o_mas"))x[x=="1282"]<-NA_character_
 x<-gsub("[[:space:]%]","",x)
 both<-!is.na(x)&grepl(",",x,fixed=TRUE)&grepl(".",x,fixed=TRUE)
 eu<-both & (regexpr(",[0-9]+$",x)>regexpr("\\.[0-9]+$",x))
 eu[is.na(eu)]<-FALSE
 x[eu]<-gsub("\\.","",x[eu]);x[eu]<-sub(",",".",x[eu],fixed=TRUE)
 us<-both & !eu;us[is.na(us)]<-FALSE
 x[us]<-gsub(",","",x[us],fixed=TRUE)
 other<-!both & !is.na(x);other[is.na(other)]<-FALSE
 x[other]<-sub(",",".",x[other],fixed=TRUE)
 z<-suppressWarnings(as.numeric(x));z[!is.finite(z)]<-NA_real_;z
}
values<-vector("list",375);kinds<-character(375);valids<-vector("list",375);recode<-vector("list",375)
for(j in seq_len(375)){
 ty<-dict$tipo[j];a<-textv(D[[j]])
 if(ty=="numérica") {
   a<-numparse(a,heads[j]);ok<-is.finite(a)
 } else {ok<-!is_missing(a)}
 rr<-"Sin reagrupación"
 if(ty=="categórica" || j==12L){ # L: combinaciones de países participantes
   counts<-table(a[ok]);if(length(counts)>2){
     rare<-names(counts)[counts<5]
     if(length(rare)>0){a[ok & a %in% rare]<-"Otras categorías (<5 estudios en el corpus)"
       rr<-sprintf("Reagrupados %d niveles originales con menos de 5 estudios",length(rare))}
   }
   if(j==12L)ty<-"categórica"
 }
 values[[j]]<-a; valids[[j]]<-ok;kinds[j]<-ty;recode[[j]]<-rr
}
cols<-c("NK","NJ","MZ","MY","MM","MN","MA","MB","LM","LL","BP","BH","AX","AY","AZ","BA","AO","AP","AQ","I","K","M","L","N","O","P","Q","R","S","T")
TAR<-match(cols,dict$columna_excel);stopifnot(length(TAR)==30,!anyNA(TAR))
predictor_ids<-setdiff(3:375,TAR)
stopifnot(length(predictor_ids)==343)

circular_reason<-function(oi,pj){
 oc<-dict$columna_excel[oi];pc<-dict$columna_excel[pj]; idx0<-pj-1L
 if(pc=="E")return("Antigüedad derivada exactamente de D: año publicación")
 if(oc %in% c("LL","LM") && (pc %in% c("LL","LM") || idx0>=302 && idx0<=322))return("Puntaje STROBE derivado de sus propios ítems")
 if(oc %in% c("MA","MB") && idx0>=327 && idx0<=339)return("Subescala JBI cohortes derivada de sus ítems")
 if(oc %in% c("MM","MN") && idx0>=340 && idx0<=351)return("Subescala JBI casos y controles derivada de sus ítems")
 if(oc %in% c("MY","MZ") && idx0>=352 && idx0<=363)return("Subescala JBI series derivada de sus ítems")
 if(oc %in% c("NJ","NK") && idx0>=364 && idx0<=374)return("Subescala JBI prevalencia derivada de sus ítems")
 if(oc=="AX" && idx0>=327 && idx0<=374)return("Tipo de checklist JBI determinado estructuralmente por diseño")
 if(oc %in% c("M","L","N","P","Q","R","S","T") && pc %in% c("M","L","N","P","Q","R","S","T","W","X"))return("País/región/ingreso: codificaciones geográficas anidadas")
 if(oc=="K" && pc %in% c("O","T","U","V","W","X"))return("Número de países y composición internacional derivada")
 if(oc=="O" && pc %in% c("K","T","U","V","W","X"))return("Colaboración internacional derivada de países")
 ""
}
results<-vector("list",length(TAR)*length(predictor_ids));counter<-0L
for(oi in TAR){
 for(pj in predictor_ids){
  counter<-counter+1L;oc<-dict$columna_excel[oi];pc<-dict$columna_excel[pj]
  mask<-valids[[oi]] & valids[[pj]];n<-sum(mask)
  status<-"";motivo<-"";test<-"";stattype<-"";stat<-NA_real_;p<-NA_real_
  effecttype<-"";effect<-NA_real_;cil<-NA_real_;ciu<-NA_real_
  x<-values[[oi]][mask];y<-values[[pj]][mask]
  nx<-length(unique(x));ny<-length(unique(y))
  if(kinds[oi]=="texto libre / identificador"){
    status<-"NO_APLICABLE";motivo<-"Identificador o texto libre sin escala común"
  } else if(kinds[pj]=="texto libre / identificador"){
    status<-"EXCLUIDO";motivo<-"Texto libre / identificador"
  } else if(nchar(circular_reason(oi,pj))>0){
    status<-"EXCLUIDO_CIRCULAR";motivo<-circular_reason(oi,pj)
  } else if(n<12) {status<-"NO_ESTIMABLE";motivo<-"Menos de 12 pares completos"
  } else if(nx<2 || ny<2){status<-"NO_ESTIMABLE";motivo<-"Sin variación en pares completos"
  } else if(kinds[oi]=="numérica" && kinds[pj]=="numérica"){
    if(n<15){status<-"NO_ESTIMABLE";motivo<-"Spearman requiere al menos 15 pares completos"
    } else {
      z<-suppressWarnings(cor.test(x,y,method="spearman",exact=FALSE))
      stat<-as.numeric(z$estimate);p<-z$p.value;test<-"Spearman";stattype<-"rho"
      effecttype<-"rho";effect<-stat;status<-"ANALIZADO"
      motivo<-"Correlación monotónica entre dos variables cuantitativas"
    }
  } else if(xor(kinds[oi]=="numérica",kinds[pj]=="numérica")){
    vv<-if(kinds[oi]=="numérica")as.numeric(x) else as.numeric(y)
    gg<-if(kinds[oi]=="numérica")as.character(y) else as.character(x)
    lev<-sort(unique(gg));ct<-as.integer(table(factor(gg,levels=lev)))
    if(length(lev)>10 || min(ct)<3){status<-"NO_ESTIMABLE";motivo<-"Más de 10 categorías o grupo con n<3"
    } else if(length(lev)==2){
      ii<-vv[gg==lev[1]];jj<-vv[gg==lev[2]]
      z<-suppressWarnings(wilcox.test(ii,jj,alternative="two.sided",exact=FALSE,correct=TRUE))
      stat<-as.numeric(z$statistic);p<-z$p.value
      effect<-2*stat/(length(ii)*length(jj))-1
      test<-"Mann–Whitney U";stattype<-"U";effecttype<-"Correlación biserial de rangos"
      status<-"ANALIZADO";motivo<-"Variable numérica entre dos grupos; rangos"
    } else {
      z<-suppressWarnings(kruskal.test(vv~factor(gg,levels=lev)))
      stat<-as.numeric(z$statistic);p<-z$p.value
      effect<-max(0,(stat-length(lev)+1)/(length(vv)-length(lev)))
      test<-"Kruskal–Wallis";stattype<-"H";effecttype<-"Epsilon cuadrado"
      status<-"ANALIZADO";motivo<-"Variable numérica entre grupos nominales; rangos"
    }
  } else {
    lvx<-sort(unique(as.character(x)));lvy<-sort(unique(as.character(y)))
    ct<-table(factor(x,levels=lvx),factor(y,levels=lvy))
    if(length(lvx)>12 || length(lvy)>12){status<-"NO_ESTIMABLE";motivo<-"Categorías nominales >12"
    } else if(min(rowSums(ct),colSums(ct))<2){status<-"NO_ESTIMABLE";motivo<-"Nivel marginal con menos de 2 estudios"
    } else if(nrow(ct)==2 && ncol(ct)==2){
      z<-fisher.test(ct,alternative="two.sided")
      p<-z$p.value;stat<-if(ct[1,2]*ct[2,1]==0)Inf else as.numeric(ct[1,1]*ct[2,2]/(ct[1,2]*ct[2,1]))
      cc<-ct+0.5;effect<-as.numeric(cc[1,1]*cc[2,2]/(cc[1,2]*cc[2,1]))
      se<-sqrt(sum(1/cc));cil<-exp(log(effect)-1.96*se);ciu<-exp(log(effect)+1.96*se)
      test<-"Fisher exacta 2×2";stattype<-"OR muestral";effecttype<-"OR corregida Haldane–Anscombe"
      status<-"ANALIZADO";motivo<-"Tabla binaria exacta; efecto OR e IC aproximado"
    } else {
      z<-suppressWarnings(chisq.test(ct,correct=FALSE));stat<-as.numeric(z$statistic)
      if(all(z$expected>=5)){p<-z$p.value;test<-"Chi-cuadrado de Pearson"
      } else {
        # R genera tablas con márgenes fijos; los valores p pueden variar frente a SciPy.
        set.seed(as.integer((20261114 + oi*1009 + pj*9176) %% 2147483647))
        ymc<-suppressWarnings(chisq.test(ct,simulate.p.value=TRUE,B=1999))
        p<-ymc$p.value;test<-"Chi-cuadrado Monte Carlo (márgenes fijos, B=1999)"
      }
      stattype<-"Chi-cuadrado";effect<-sqrt(max(0,stat)/(sum(ct)*min(nrow(ct)-1,ncol(ct)-1)))
      effecttype<-"V de Cramér";status<-"ANALIZADO";motivo<-"Tabla nominal RxC"
    }
  }
  results[[counter]]<-data.frame(
    desenlace=oc,nombre_desenlace=dict$nombre_final[oi],tipo_desenlace=kinds[oi],
    predictor=pc,nombre_predictor=dict$nombre_final[pj],tipo_predictor=kinds[pj],
    n_pares=n,n_out_valido=sum(valids[[oi]]),n_pred_valido=sum(valids[[pj]]),
    faltantes_pares=128-n,niveles_des=nx,niveles_pred=ny,
    estado=status,prueba=test,estadistico_tipo=stattype,estadistico=stat,p=p,
    tamano_efecto_tipo=effecttype,tamano_efecto=effect,ic95_inf=cil,ic95_sup=ciu,
    justificacion=motivo,recodificacion_des=recode[[oi]],recodificacion_pred=recode[[pj]],
    stringsAsFactors=FALSE)
 }
 cat("Evaluado desenlace",dict$columna_excel[oi],"\n")
}
res<-do.call(rbind,results)
res$q_BH_desenlace<-NA_real_;res$q_BH_global<-NA_real_
valididx<-which(res$estado=="ANALIZADO" & is.finite(res$p))
res$q_BH_global[valididx]<-p.adjust(res$p[valididx],method="BH")
for(code in cols){ix<-which(res$desenlace==code&res$estado=="ANALIZADO"&is.finite(res$p));if(length(ix))res$q_BH_desenlace[ix]<-p.adjust(res$p[ix],method="BH")}
res$signif_p<-!is.na(res$p) & res$p<.05
res$signif_qD<-!is.na(res$q_BH_desenlace)&res$q_BH_desenlace<.05
res$signif_qG<-!is.na(res$q_BH_global)&res$q_BH_global<.05
slist<-lapply(cols,function(code){a<-res[res$desenlace==code,,drop=FALSE]
  data.frame(Desenlace=code,Nombre=dict$nombre_final[match(code,dict$columna_excel)],
   N_valido=sum(valids[[match(code,dict$columna_excel)]]),Contrastes=sum(a$estado=="ANALIZADO"),
   No_estimables=sum(a$estado=="NO_ESTIMABLE"),
   Excluidos=sum(a$estado %in% c("EXCLUIDO","EXCLUIDO_CIRCULAR","NO_APLICABLE")),
   Nominales=sum(a$signif_p),BH_desenlace=sum(a$signif_qD),BH_global=sum(a$signif_qG))})
resumen<-do.call(rbind,slist)
map<-data.frame(Columna=dict$columna_excel,Original=dict$nombre_original,
 Nombre=dict$nombre_final,Tipo_original=dict$tipo,Tipo_analitico=kinds,
 N_valido=vapply(valids,sum,numeric(1)),Regla_reagrupacion=unlist(recode),
 stringsAsFactors=FALSE)

wb<-openxlsx::createWorkbook()
openxlsx::addWorksheet(wb,"Resumen");openxlsx::writeData(wb,"Resumen",resumen)
openxlsx::addWorksheet(wb,"Todos_los_pares");openxlsx::writeData(wb,"Todos_los_pares",res)
openxlsx::addWorksheet(wb,"Evaluables");openxlsx::writeData(wb,"Evaluables",res[res$estado=="ANALIZADO",,drop=FALSE])
openxlsx::addWorksheet(wb,"qGlobal_significativo");openxlsx::writeData(wb,"qGlobal_significativo",res[res$signif_qG,,drop=FALSE])
openxlsx::addWorksheet(wb,"Diccionario");openxlsx::writeData(wb,"Diccionario",map)
openxlsx::saveWorkbook(wb,file.path(outdir,"Resultados_bivariados_P1_R.xlsx"),overwrite=TRUE)
write.csv(res,file.path(outdir,"Resultados_bivariados_P1_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(resumen,file.path(outdir,"Resumen_P1_R.csv"),row.names=FALSE,fileEncoding="UTF-8")

# DOCX reproducible opcional: el motor R requiere paquetes officer y flextable.
if(requireNamespace("officer",quietly=TRUE) && requireNamespace("flextable",quietly=TRUE)){
 fmt<-function(x) ifelse(is.na(x),"—",formatC(x,digits=4,format="fg"))
 doc<-officer::read_docx()
 doc<-officer::body_add_par(doc,"Resultados bivariados — Manuscrito 1",style="heading 1")
 for(code in cols){
  a<-res[res$desenlace==code & res$estado=="ANALIZADO",,drop=FALSE]
  doc<-officer::body_add_par(doc,paste0("Desenlace ",code," — ",dict$nombre_final[match(code,dict$columna_excel)]),style="heading 2")
  if(!nrow(a)){doc<-officer::body_add_par(doc,"Sin contrastes evaluables por n insuficiente o falta de variabilidad.");next}
  sub<-data.frame(Variable=paste0(a$predictor," — ",a$nombre_predictor),
                 Prueba=a$prueba,Estadistico=fmt(a$estadistico),n=a$n_pares,
                 p=fmt(a$p),qD=fmt(a$q_BH_desenlace),qG=fmt(a$q_BH_global),check.names=FALSE)
  ft<-flextable::flextable(sub)
  if(any(a$signif_qG))ft<-flextable::bold(ft,i=which(a$signif_qG),bold=TRUE,part="body")
  ft<-flextable::fontsize(ft,size=8,part="all")
  ft<-flextable::autofit(ft)
  doc<-flextable::body_add_flextable(doc,ft)
 }
 print(doc,target=file.path(outdir,"Tablas_bivariadas_P1_R.docx"))
}
cat("Análisis completado:",nrow(res),"pares,",length(valididx),"contrastes evaluables.\n")
