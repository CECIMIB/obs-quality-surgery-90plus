# Investigación doctoral — Manuscrito 1
# Ampliación de medidas de asociación (RP, OR, diferencia de proporciones, rho y sus IC95%)
# Etapa EXPLORATORIA UNIVARIABLE; NO incluye modelos multivariables ni multinivel.
# Requiere ejecución previa de analisis_bivariado_P1_V7.R
# Ejemplo: Rscript ampliacion_OR_RP_IC95_P1.R "matriz_extendida V7 - TOTAL FINAL.xlsx" "diccionario_variables_V7_ES.csv" "salidas_R"
args <- commandArgs(trailingOnly=TRUE)
infile <- if(length(args)>=1)args[1] else "matriz_extendida V7 - TOTAL FINAL.xlsx"
dfile  <- if(length(args)>=2)args[2] else "diccionario_variables_V7_ES.csv"
outdir <- if(length(args)>=3)args[3] else "salidas_R"
for(pkg in c("readxl","openxlsx","PropCIs"))if(!requireNamespace(pkg,quietly=TRUE))stop("Instale el paquete R: ",pkg)
if(!file.exists(infile) || !file.exists(dfile))stop("Faltan matriz o diccionario")
basefile <- file.path(outdir,"Resultados_bivariados_P1_R.csv")
if(!file.exists(basefile))stop("Ejecute primero analisis_bivariado_P1_V7.R; falta ",basefile)
dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
res <- read.csv(basefile,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
stopifnot(nrow(res)==10290L, sum(res$estado=="ANALIZADO")>3900L)
X<-readxl::read_excel(infile,sheet=1,col_names=FALSE,col_types="text",.name_repair="minimal")
stopifnot(ncol(X)==375L)
headers<-vapply(X,function(z)if(is.na(z[1]))"" else trimws(as.character(z[1])),character(1))
D<-X[-1,,drop=FALSE];D<-D[!is.na(D[[1]])&nzchar(trimws(D[[1]])),,drop=FALSE]
stopifnot(nrow(D)==128L)
DIC<-read.csv(dfile,fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
stopifnot(nrow(DIC)==375L,all(trimws(DIC$nombre_original)==trimws(headers)))
textv<-function(x){z<-trimws(as.character(x));z[is.na(z)]<-"";z}
miss<-c("","no reportado","no aplica","n/a","na","sin dato","sin datos","no disponible","not reported","not available","nr","n.r.","no informado","sin información","sin informacion")
is_missing<-function(z)tolower(textv(z)) %in% miss
numparse<-function(v,nm){
 z<-textv(v);z[is_missing(z)]<-NA_character_
 if(nm %in% c("n_categorias","n_dos_o_mas"))z[z=="1282"]<-NA_character_
 z<-gsub("[[:space:]%]","",z)
 both<-!is.na(z)&grepl(",",z,fixed=TRUE)&grepl(".",z,fixed=TRUE)
 eu<-both & regexpr(",[0-9]+$",z)>regexpr("\\.[0-9]+$",z);eu[is.na(eu)]<-FALSE
 z[eu]<-gsub("\\.","",z[eu]);z[eu]<-sub(",",".",z[eu],fixed=TRUE)
 us<-both & !eu;us[is.na(us)]<-FALSE;z[us]<-gsub(",","",z[us],fixed=TRUE)
 oth<-!both & !is.na(z);oth[is.na(oth)]<-FALSE;z[oth]<-sub(",",".",z[oth],fixed=TRUE)
 v<-suppressWarnings(as.numeric(z));v[!is.finite(v)]<-NA_real_;v
}
values<-vector("list",375);valid<-vector("list",375);kinds<-character(375);counts_all<-vector("list",375)
for(j in seq_len(375)){
 k<-DIC$tipo[j];v<-textv(D[[j]])
 if(k=="numérica"){v<-numparse(v,headers[j]);ok<-is.finite(v)}else ok<-!is_missing(v)
 if(k=="categórica" || j==12L){
  tab<-table(v[ok]);if(length(tab)>2){rare<-names(tab)[tab<5];if(length(rare))v[ok&v %in% rare]<-"Otras categorías (<5 estudios en el corpus)"}
  if(j==12L)k<-"categórica"
 }
 values[[j]]<-v;valid[[j]]<-ok;kinds[j]<-k
 if(k=="categórica")counts_all[[j]]<-table(v[ok])
}
focus<-function(lev,j){ # minority category in full corpus; tie alphabetical
 cnt<-counts_all[[j]];lev<-sort(unique(as.character(lev)))
 lev[order(as.numeric(cnt[lev]),lev)][1]
}
wilson<-function(a,n){
 z<-qnorm(.975);p<-a/n;den<-1+z^2/n
 mid<-(p+z^2/(2*n))/den;h<-z*sqrt(p*(1-p)/n+z^2/(4*n*n))/den
 c(max(0,mid-h),min(1,mid+h))
}
newcombe<-function(a,n1,c,n0){
 p1<-a/n1;p0<-c/n0;w1<-wilson(a,n1);w0<-wilson(c,n0);delta<-p1-p0
 c(delta,delta-sqrt((p1-w1[1])^2+(w0[2]-p0)^2),delta+sqrt((w1[2]-p1)^2+(p0-w0[1])^2))
}
# Mantener exactamente los 3934 contrastes del flujo previo; RP / OR no se calculan en desenlaces continuos.
id<-which(res$estado=="ANALIZADO" & is.finite(res$p))
expanded<-res[id,,drop=FALSE]
expanded$estimando_principal<-"";expanded$efecto_ampliado<-NA_real_
expanded$IC_inf<-NA_real_;expanded$IC_sup<-NA_real_
expanded$metodo_IC<-"";expanded$p_ampliacion<-NA_real_;expanded$q_BH_ampliacion<-NA_real_
expanded$orientacion<-"";expanded$advertencia<-""
ratio<-list();logit<-list();spear<-list();nx<-0L;nl<-0L;ns<-0L
for(ii in seq_len(nrow(expanded))){
 r<-expanded[ii,,drop=FALSE];o<-match(r$desenlace,DIC$columna_excel);p<-match(r$predictor,DIC$columna_excel)
 ok<-valid[[o]]&valid[[p]];y<-values[[o]][ok];x<-values[[p]][ok];n<-length(y)
 if(kinds[o]=="categórica"&&kinds[p]=="categórica"&&length(unique(y))==2&&length(unique(x))==2){
  ev<-focus(unique(y),o);ex<-focus(unique(x),p)
  non<-setdiff(unique(as.character(y)),ev)[1];ref<-setdiff(unique(as.character(x)),ex)[1]
  a<-sum(y==ev&x==ex);b<-sum(y!=ev&x==ex);c<-sum(y==ev&x!=ex);d<-sum(y!=ev&x!=ex)
  tab<-matrix(c(a,b,c,d),nrow=2,byrow=TRUE)
  ft<-fisher.test(tab,alternative="two.sided",conf.int=TRUE,conf.level=.95)
  pr<-if(c==0)if(a>0)Inf else NA_real_ else (a/(a+b))/(c/(c+d))
  ci<-tryCatch(PropCIs::riskscoreci(a,a+b,c,c+d,.95)$conf.int,error=function(e)c(NA_real_,NA_real_))
  dp<-newcombe(a,a+b,c,c+d)
  nx<-nx+1L;ratio[[nx]]<-data.frame(
   desenlace=r$desenlace,nombre_desenlace=r$nombre_desenlace,predictor=r$predictor,nombre_predictor=r$nombre_predictor,
   n_pares=n,evento=ev,no_evento=non,expuesto=ex,referencia=ref,a=a,b=b,c=c,d=d,
   n_exp=a+b,n_ref=c+d,prev_exp=a/(a+b),prev_ref=c/(c+d),RP=pr,
   RP_ic_inf=as.numeric(ci[1]),RP_ic_sup=as.numeric(ci[2]),
   OR_exacto=unname(as.numeric(ft$estimate)),OR_ic_inf=ft$conf.int[1],OR_ic_sup=ft$conf.int[2],
   diferencia_prevalencias=dp[1],DP_ic_inf=dp[2],DP_ic_sup=dp[3],p_exacta=ft$p.value,
   q_BH_global_previo=r$q_BH_global,q_BH_desenlace_previo=r$q_BH_desenlace,
   escasez=if(min(a,b,c,d)<5)"Sí" else "No",stringsAsFactors=FALSE)
  expanded$estimando_principal[ii]<-"RP publicaciones"
  expanded$efecto_ampliado[ii]<-pr;expanded$IC_inf[ii]<-ci[1];expanded$IC_sup[ii]<-ci[2]
  expanded$metodo_IC[ii]<-"Score RP (PropCIs); OR Fisher exacta"
  expanded$p_ampliacion[ii]<-ft$p.value
  expanded$orientacion[ii]<-paste0("Evento=",ev,"; exposición=",ex,"; referencia=",ref)
  expanded$advertencia[ii]<-if(min(a,b,c,d)<5)"Al menos una celda <5" else ""
 } else if(kinds[o]=="categórica"&&length(unique(y))==2&&kinds[p]=="numérica"){
  ev<-focus(unique(y),o);yy<-as.integer(y==ev);xx<-as.numeric(x)
  sdv<-sd(xx);n1<-sum(yy);n0<-n-n1
  est<-NA_real_;low<-NA_real_;up<-NA_real_;pv<-NA_real_;status<-"No estimado";why<-"Insuficiencia de datos o variación"
  if(n>=30&&n1>=10&&n0>=10&&length(unique(xx))>=4&&is.finite(sdv)&&sdv>0){
    zz<-(xx-mean(xx))/sdv
    fit<-tryCatch(suppressWarnings(glm(yy~zz,family=binomial())),error=function(e)NULL)
    if(!is.null(fit)){
     beta<-unname(coef(fit)[2]);se<-sqrt(diag(vcov(fit)))[2]
     if(is.finite(beta)&&is.finite(se)&&abs(beta)<15){
      est<-exp(beta);low<-exp(beta-qnorm(.975)*se);up<-exp(beta+qnorm(.975)*se)
      pv<-2*pnorm(-abs(beta/se));status<-"ESTIMADO";why<-"Logística univariable, OR por 1 DE"
      expanded$estimando_principal[ii]<-"OR/1DE";expanded$efecto_ampliado[ii]<-est
      expanded$IC_inf[ii]<-low;expanded$IC_sup[ii]<-up;expanded$p_ampliacion[ii]<-pv
      expanded$metodo_IC[ii]<-"Wald log(OR)";expanded$orientacion[ii]<-paste0("Evento=",ev,"; predictor por 1 DE")
     }
    }
  }
  nl<-nl+1L;logit[[nl]]<-data.frame(desenlace=r$desenlace,predictor=r$predictor,n_pares=n,
    evento=ev,n_eventos=n1,n_noeventos=n0,DE_predictor=sdv,OR_1DE=est,
    OR_ic_inf=low,OR_ic_sup=up,p_logit=pv,estado=status,razon=why,stringsAsFactors=FALSE)
 } else if(kinds[o]=="numérica"&&kinds[p]=="numérica"){
  rho<-as.numeric(r$tamano_efecto)
  if(is.finite(rho)&&abs(rho)<1&&n>=15){
   z<-atanh(rho);half<-qnorm(.975)/sqrt(n-3);ci<-tanh(c(z-half,z+half))
   expanded$estimando_principal[ii]<-"Rho Spearman";expanded$efecto_ampliado[ii]<-rho
   expanded$IC_inf[ii]<-ci[1];expanded$IC_sup[ii]<-ci[2]
   expanded$metodo_IC[ii]<-"Fisher z aproximada"
   expanded$p_ampliacion[ii]<-r$p
   ns<-ns+1L;spear[[ns]]<-data.frame(desenlace=r$desenlace,predictor=r$predictor,n_pares=n,rho=rho,
     IC_inf=ci[1],IC_sup=ci[2],p=r$p,q_BH_global_previo=r$q_BH_global,stringsAsFactors=FALSE)
  }
 } else{
  expanded$estimando_principal[ii]<-as.character(r$tamano_efecto_tipo)
  expanded$efecto_ampliado[ii]<-r$tamano_efecto
  expanded$metodo_IC[ii]<-"No estimado para estadístico de rangos/ómnibus en esta ampliación"
  expanded$p_ampliacion[ii]<-r$p
 }
}
RP<-do.call(rbind,ratio);LOG<-do.call(rbind,logit);SPEAR<-do.call(rbind,spear)
# BH ampliación: SOLO 2x2 Fisher y logística univariable estimada. NO reetiquetar q global previa.
i_eligible<-which(expanded$estimando_principal %in% c("RP publicaciones","OR/1DE")&is.finite(expanded$p_ampliacion))
expanded$q_BH_ampliacion[i_eligible]<-p.adjust(expanded$p_ampliacion[i_eligible],method="BH")
RP$q_BH_ampliacion<-NA_real_;LOG$q_BH_ampliacion<-NA_real_
for(i in seq_len(nrow(RP))){m<-which(expanded$desenlace==RP$desenlace[i]&expanded$predictor==RP$predictor[i]);RP$q_BH_ampliacion[i]<-expanded$q_BH_ampliacion[m]}
for(i in seq_len(nrow(LOG)))if(LOG$estado[i]=="ESTIMADO"){
 m<-which(expanded$desenlace==LOG$desenlace[i]&expanded$predictor==LOG$predictor[i]);LOG$q_BH_ampliacion[i]<-expanded$q_BH_ampliacion[m]}
stopifnot(nrow(RP)==866L,nrow(SPEAR)==161L,nrow(expanded)==3934L)
cat("2x2:",nrow(RP),"; logística válida:",sum(LOG$estado=="ESTIMADO"),"; Spearman:",nrow(SPEAR),"\n")
wb<-openxlsx::createWorkbook()
for(p in c("RP_OR_2x2","Logit_1DE","Spearman","Todos_3934","Diccionario"))openxlsx::addWorksheet(wb,p)
openxlsx::writeData(wb,"RP_OR_2x2",RP);openxlsx::writeData(wb,"Logit_1DE",LOG)
openxlsx::writeData(wb,"Spearman",SPEAR);openxlsx::writeData(wb,"Todos_3934",expanded)
openxlsx::writeData(wb,"Diccionario",DIC)
for(p in names(wb)){openxlsx::freezePane(wb,p,firstRow=TRUE);openxlsx::setColWidths(wb,p,cols=1:4,widths=c(15,42,15,42))}
openxlsx::saveWorkbook(wb,file.path(outdir,"Ampliacion_R_OR_RP_IC95.xlsx"),overwrite=TRUE)
write.csv(RP,file.path(outdir,"RP_OR_2x2_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(LOG,file.path(outdir,"Logit_1DE_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(SPEAR,file.path(outdir,"Spearman_IC_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(expanded,file.path(outdir,"Todos_3934_ampliados_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
writeLines(capture.output(sessionInfo()),file.path(outdir,"sessionInfo_R.txt"))
# WORD opcional de principales razones de asociación; material exhaustivo en Excel.
if(requireNamespace("officer",quietly=TRUE)&&requireNamespace("flextable",quietly=TRUE)){
 doc<-officer::read_docx();doc<-officer::body_add_par(doc,"Razones de prevalencia y OR — Manuscrito 1",style="heading 1")
 for(oc in unique(res$desenlace)){
  a<-RP[RP$desenlace==oc,,drop=FALSE]
  doc<-officer::body_add_par(doc,paste0("Desenlace ",oc),style="heading 2")
  if(!nrow(a)){doc<-officer::body_add_par(doc,"Sin pares 2×2 estimables.");next}
  sub<-data.frame(Predictor=a$nombre_predictor,Contraste=paste0(a$evento,"; ",a$expuesto," vs ",a$referencia),
    RP=paste0(round(a$RP,2)," (",round(a$RP_ic_inf,2),"–",round(a$RP_ic_sup,2),")"),
    OR=paste0(round(a$OR_exacto,2)," (",round(a$OR_ic_inf,2),"–",round(a$OR_ic_sup,2),")"),
    p=signif(a$p_exacta,3),q=signif(a$q_BH_ampliacion,3),check.names=FALSE)
  ft<-flextable::flextable(sub);ft<-flextable::fontsize(ft,size=8,part="all")
  ft<-flextable::autofit(ft);doc<-flextable::body_add_flextable(doc,ft)
 }
 print(doc,target=file.path(outdir,"Tablas_R_RP_OR.docx"))
}
cat("Salidas guardadas en:",outdir,"\n")
