# Manuscrito 1 | Modelamiento de desenlaces prioritarios | version 2026-11-14
# FUENTE: matriz_extendida V7 - TOTAL FINAL.xlsx, hoja Sheet1 (128 publicaciones)
# No imputa. No agrupa por participantes compartidos por ausencia de identificador.
# IMPORTANTE: las estimaciones en el suplemento adjunto fueron calculadas en Python.
# Este script R es una implementacion independiente preparada para validacion cruzada,
# NO debe anunciarse como validado hasta ejecutarse en R y cotejar las estimaciones.
# Uso: Rscript modelos_prioritarios_P1_reproducible.R "matriz_extendida V7 - TOTAL FINAL.xlsx" "salidas_R"
args <- commandArgs(trailingOnly=TRUE)
input <- if(length(args)>=1) args[1] else "matriz_extendida V7 - TOTAL FINAL.xlsx"
out <- if(length(args)>=2) args[2] else "salidas_R"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
req <- c("readxl", "logistf", "sandwich", "nnet", "MASS", "openxlsx", "officer", "flextable")
missing_packages <- req[!vapply(req, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing_packages)) stop("Instale previamente: ", paste(missing_packages, collapse=", "))
if(!file.exists(input)) stop("Archivo de entrada no encontrado: ", input)
raw <- as.data.frame(readxl::read_excel(input, sheet=1, .name_repair="minimal"), check.names=FALSE)
stopifnot(nrow(raw)==128, ncol(raw)>=375)
clean <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", clean(x), fixed=TRUE)))
coltext <- function(n) clean(raw[[n]])
reg <- coltext(16)
reg[reg == "Middle East and North Africa"] <- "Middle East & North Africa"
mapregion <- c("North America"="Norteamerica", "Europe & Central Asia"="Europa_AsiaCentral",
 "East Asia & Pacific"="AsiaOriental_Pacifico", "Latin America & Caribbean"="Otras",
 "Middle East & North Africa"="Otras")
region <- unname(mapregion[reg]); region[is.na(region)] <- "Otras"
D <- data.frame(publicacion=seq_len(nrow(raw)),titulo=coltext(1),revista=coltext(7),
 year=num(raw[[4]]),sample=num(raw[[61]]),ortho=as.integer(coltext(42)=="Ortopedia y Traumatología"),
 geriatric=as.integer(coltext(60)=="Sí"),cga=as.integer(coltext(68)=="Sí"),
 income_upper=as.integer(coltext(17)=="Upper middle income"),region=region,
 strobe=num(raw[[325]]),jbi_score=num(raw[[339]]),
 cohort=as.integer(coltext(50)=="Estudio de Cohorte"),
 method_specific=as.integer(coltext(50)!="Diseño metodológico inespecífico"),
 stats_level=coltext(53), stringsAsFactors=FALSE)
stopifnot(all(is.finite(D$year)), all(D$sample>0), sum(D$geriatric)==11,
 sum(D$cga)==3, sum(D$income_upper)==13, sum(D$cohort)==55)
D$year5 <- (D$year-2016)/5
D$log10nc <- log10(D$sample)-mean(log10(D$sample))
D$strobe_high <- as.integer(D$strobe>=.75)
D$jbi_prop <- D$jbi_score/11
D$analysis_level <- ordered(D$stats_level,levels=c("Descriptivo",
 "Inferencial sin ajuste multivariable", "Modelamiento multivariable o ajustado"))
stopifnot(!any(is.na(D$analysis_level)),sum(D$strobe_high)==81)

CO <- list(); SI <- list(); AUD <- list(); table_of_models <- list()
add_coef <- function(model,term,b,se,p,lo=b-qnorm(.975)*se,hi=b+qnorm(.975)*se,
                     measure="OR",type="principal",comparison="",n=NA_integer_) {
 CO[[length(CO)+1]] <<- data.frame(model=model,term=term,comparison=comparison,
   beta=b,se=se,effect=measure,estimate=exp(b),low=exp(lo),high=exp(hi),p=p,
   type=type,n=n,stringsAsFactors=FALSE)
}
record_glm <- function(fit,model,n, type="principal",cluster=NULL) {
 V <- if(is.null(cluster)) sandwich::vcovHC(fit,type="HC3") else
   sandwich::vcovCL(fit,cluster=cluster,type="HC1")
 b <- stats::coef(fit); se <- sqrt(diag(V))
 for(i in seq_along(b)) add_coef(model,names(b)[i],b[i],se[i],
   2*pnorm(-abs(b[i]/se[i])),type=type,n=n)
 invisible(fit)
}
record_firth <- function(fit,model,n) {
 for(i in seq_along(fit$coefficients)) add_coef(model,names(fit$coefficients)[i],
    unname(fit$coefficients[i]),sqrt(diag(fit$var))[i],unname(fit$prob[i]),
    unname(fit$ci.lower[i]),unname(fit$ci.upper[i]), type="principal",n=n)
 invisible(fit)
}

# M1. Especialidad y periodo explican participacion geriatra; eventos: 11.
m1 <- logistf::logistf(geriatric ~ year5 + ortho,data=D,pl=TRUE)
record_firth(m1,"M1_Geriatria",nrow(D)); saveRDS(m1,file.path(out,"M1_Geriatria.rds"))
# M2. No hay base para multiples predictores (solo 3 CGA); la regresion es exploratoria.
m2 <- logistf::logistf(cga ~ geriatric,data=D,pl=TRUE)
record_firth(m2,"M2_CGA",nrow(D));saveRDS(m2,file.path(out,"M2_CGA.rds"))
# Exacta de Fisher 3/11 frente a 0/117.
CGAtable <- table(factor(D$geriatric,levels=c(1,0)),factor(D$cga,levels=c(1,0)))
fisher <- fisher.test(CGAtable)
SI[[length(SI)+1]] <- data.frame(analysis="CGA_Geriatra_Fisher",scenario="Exacta 2x2",
 term="geriatric",n=128,estimate=unname(fisher$estimate),low=fisher$conf.int[1],
 high=fisher$conf.int[2],p=fisher$p.value)
# M3. País principal Upper middle (n=13) vs High income (n=115).
m3 <- logistf::logistf(income_upper ~ year5 + ortho,data=D,pl=TRUE)
record_firth(m3,"M3_Ingreso",nrow(D));saveRDS(m3,file.path(out,"M3_Ingreso.rds"))
# M4. Regiones principales, cinco estudios de otras regiones fuera de modelo PRINCIPAL.
Dreg <- subset(D,region!="Otras")
Dreg$region <- factor(Dreg$region,levels=c("Norteamerica","Europa_AsiaCentral","AsiaOriental_Pacifico"))
m4 <- nnet::multinom(region ~ year5 + ortho,data=Dreg,trace=FALSE,maxit=300)
s4 <- summary(m4)
for(i in seq_len(nrow(s4$coefficients))) for(j in seq_len(ncol(s4$coefficients))){
 b <- s4$coefficients[i,j];se <- s4$standard.errors[i,j]
 add_coef("M4_Region",colnames(s4$coefficients)[j],b,se,2*pnorm(-abs(b/se)),
 measure="RRR",comparison=rownames(s4$coefficients)[i],n=nrow(Dreg))
}
saveRDS(m4,file.path(out,"M4_Region.rds"))
# M5. Proporcion continua/fractional de cumplimiento STROBE; coeficientes en odds de media fraccional.
m5 <- glm(strobe ~ year5 + log10nc + ortho,data=D,family=quasibinomial("logit"))
record_glm(m5,"M5_STROBE_fraccion",128);saveRDS(m5,file.path(out,"M5_STROBE_fraccion.rds"))
# M6. Categoria >=75% en cumplimiento STROBE; no oficial JBI/STROBE.
m6 <- glm(strobe_high ~ year5 + log10nc + ortho,data=D,family=binomial("logit"))
record_glm(m6,"M6_STROBE_75",128);saveRDS(m6,file.path(out,"M6_STROBE_75.rds"))
# M7. Puntuacion fraccional JBI cohortes, n=55, denominador 11; categoria JBI no estimable.
Dcoh <- subset(D,cohort==1)
m7 <- glm(jbi_prop ~ year5 + log10nc + ortho,data=Dcoh,family=quasibinomial("logit"))
record_glm(m7,"M7_JBI_fraccion",nrow(Dcoh));saveRDS(m7,file.path(out,"M7_JBI_fraccion.rds"))
# M8. Nivel de complejidad estadistica 3 clases; asuncion proporcional requiere sensibilidad.
Xord <- D[,c("year5","log10nc","ortho")]
m8 <- MASS::polr(analysis_level ~ year5 + log10nc + ortho,data=D,method="logistic",Hess=TRUE)
s8 <- coef(summary(m8))
for(term in rownames(s8)[1:3]){
 b <- s8[term,"Value"];se <- s8[term,"Std. Error"]
 add_coef("M8_Nivel_analitico",term,b,se,2*pnorm(-abs(b/se)),n=128)
}
saveRDS(m8,file.path(out,"M8_Nivel_analitico.rds"))
# M8b sensibilidad robusta al supuesto ordinal: multivariable SI vs el resto.
D$advanced <- as.integer(D$stats_level=="Modelamiento multivariable o ajustado")
m8b <- glm(advanced ~ year5 + log10nc + ortho,data=D,family=binomial("logit"))
record_glm(m8b,"M8b_Multivariable_binario",128,type="sensibilidad")
# M9. Diseño metodologico especificado (66) vs inespecifico (62).
m9 <- glm(method_specific ~ year5 + log10nc + ortho,data=D,family=binomial("logit"))
record_glm(m9,"M9_Diseno_especifico",128);saveRDS(m9,file.path(out,"M9_Diseno_especifico.rds"))

# Ajuste de multiplicidad global de modelos principales, excluye interceptos/sensibilidades.
coef_table <- do.call(rbind,CO)
coef_table$q_BH_global <- NA_real_
index <- which(coef_table$type=="principal" & coef_table$term!="(Intercept)")
coef_table$q_BH_global[index] <- p.adjust(coef_table$p[index],method="BH")

# Sensibilidades 1: >=70%, 75%, 80% sobre STROBE
for(cut in c(.70,.75,.80)) {
 D$target <- as.integer(D$strobe>=cut)
 fi <- glm(target ~ year5 + log10nc + ortho,data=D,family=binomial("logit"))
 V <- sandwich::vcovHC(fi,type="HC3")
 for(term in c("year5","log10nc","ortho")){
  b <- coef(fi)[term];se <- sqrt(diag(V))[term]
  SI[[length(SI)+1]] <- data.frame(analysis="STROBE_Umbral",scenario=paste0(cut*100,"%"),term=term,n=128,
   estimate=exp(b),low=exp(b-qnorm(.975)*se),high=exp(b+qnorm(.975)*se),p=2*pnorm(-abs(b/se)))
 }
}
# Sensibilidad 2: ponderacion alternativa de los 22 criterios (no modifica matriz original)
strobe_block <- raw[,302:323,drop=FALSE]
for(weight in c(.25,.50,.75)){
 new_fraction <- apply(strobe_block,1,function(z){
  a <- clean(z);valid <- a %in% c("Cumple","Cumple parcialmente","No cumple")
  sum(ifelse(a[valid]=="Cumple",1,ifelse(a[valid]=="Cumple parcialmente",weight,0)))/sum(valid)
 })
 D$strobe_alt <- as.numeric(new_fraction)
 fi <- glm(strobe_alt ~ year5 + log10nc + ortho,data=D,family=quasibinomial("logit"))
 V <- sandwich::vcovHC(fi,type="HC3")
 for(term in c("year5","log10nc","ortho")){
  b <- coef(fi)[term];se <- sqrt(diag(V))[term]
  SI[[length(SI)+1]] <- data.frame(analysis="STROBE_Ponderacion",scenario=as.character(weight),term=term,n=128,
   estimate=exp(b),low=exp(b-qnorm(.975)*se),high=exp(b+qnorm(.975)*se),p=2*pnorm(-abs(b/se)))
 }
}
# Sensibilidad 3: revistas (NO participantes ni cohortes compartidas)
Vcluster <- sandwich::vcovCL(m5,cluster=D$revista,type="HC1")
for(term in c("year5","log10nc","ortho")) {
 b <- coef(m5)[term];se <- sqrt(diag(Vcluster))[term]
 SI[[length(SI)+1]] <- data.frame(analysis="STROBE_SE_por_revista",scenario="cluster",
 term=term,n=128,estimate=exp(b),low=exp(b-qnorm(.975)*se),high=exp(b+qnorm(.975)*se),
 p=2*pnorm(-abs(b/se)))
}
# Sensibilidad 4: STROBE solo estudios 2010+
D2010 <- subset(D,year>=2010)
fi <- glm(strobe ~ year5 + log10nc + ortho,data=D2010,family=quasibinomial("logit"))
V <- sandwich::vcovHC(fi,type="HC3")
for(term in c("year5","log10nc","ortho")) {
 b <- coef(fi)[term];se <- sqrt(diag(V))[term]
 SI[[length(SI)+1]] <- data.frame(analysis="STROBE_2010+",scenario="restringir_periodo",
 term=term,n=nrow(D2010),estimate=exp(b),low=exp(b-qnorm(.975)*se),
 high=exp(b+qnorm(.975)*se),p=2*pnorm(-abs(b/se)))
}
# Sensibilidad 5: ajustar adicionalmente por participacion de geriatra
fplus <- glm(strobe ~ year5 + log10nc + ortho + geriatric,data=D,family=quasibinomial("logit"))
Vp <- sandwich::vcovHC(fplus,type="HC3")
for(term in c("year5","log10nc","ortho","geriatric")) {
 b <- coef(fplus)[term];se <- sqrt(diag(Vp))[term]
 SI[[length(SI)+1]] <- data.frame(analysis="STROBE+geriatria",scenario="ampliado",term=term,
 n=128,estimate=exp(b),low=exp(b-qnorm(.975)*se),high=exp(b+qnorm(.975)*se),
 p=2*pnorm(-abs(b/se)))
}
# Sensibilidad 6: diagnostico de proporcionalidad comparando dos logits de corte.
D$yordinal <- as.integer(D$analysis_level)
for(k in c(2,3)){
 D$above <- as.integer(D$yordinal>=k)
 ff <- glm(above ~ year5 + log10nc + ortho,data=D,family=binomial("logit"))
 V <- sandwich::vcovHC(ff,type="HC3")
 for(term in c("year5","log10nc","ortho")){
  b <- coef(ff)[term];se <- sqrt(diag(V))[term]
  SI[[length(SI)+1]] <- data.frame(analysis="Ordinal_umbral",scenario=paste0("nivel>=",k),term=term,
   n=128,estimate=exp(b),low=exp(b-qnorm(.975)*se),high=exp(b+qnorm(.975)*se),
   p=2*pnorm(-abs(b/se)))
 }
}
# Sensibilidad 7: influencia de una publicación a la vez (tendencia temporal STROBE)
LOO <- do.call(rbind,lapply(seq_len(nrow(D)),function(i){
 f <- glm(strobe ~ year5 + log10nc + ortho,data=D[-i,,drop=FALSE],family=quasibinomial("logit"))
 se <- sqrt(diag(sandwich::vcovHC(f,type="HC3")))["year5"]
 b <- coef(f)["year5"]
 data.frame(fila_original=i+1,OR_year5=exp(b),p=2*pnorm(-abs(b/se)))
}))
# Sensibilidad 8: correlacion STROBE vs JBI entre cohortes y bootstrap no parametrico.
set.seed(1743)
rho <- suppressWarnings(cor(Dcoh$strobe,Dcoh$jbi_prop,method="spearman"))
bs <- replicate(2000,{
 id <- sample(seq_len(nrow(Dcoh)),replace=TRUE)
 suppressWarnings(cor(Dcoh$strobe[id],Dcoh$jbi_prop[id],method="spearman"))
})
bs <- bs[is.finite(bs)];CIrho <- as.numeric(quantile(bs,c(.025,.975)))
SI[[length(SI)+1]] <- data.frame(analysis="JBI_STROBE_Spearman",scenario="bootstrap 2000",
 term="rho",n=55,estimate=rho,low=CIrho[1],high=CIrho[2],
 p=suppressWarnings(cor.test(Dcoh$strobe,Dcoh$jbi_prop,method="spearman",exact=FALSE)$p.value))

sensitivity_table <- do.call(rbind,SI)
# Registro de no estimabilidad / advertencias: estudio no es paciente, ingresos y region codificados desde pais.
AUD <- data.frame(incidencia=c("Categoría JBI entre cohortes constante: 0 alta y 55 baja",
 "CGA solo 3 casos: no hay fundamento para regresión convencional con múltiples variables",
 "Tres CGA se concentran en estudios con geriatra; asociación documental y separación",
 "Las regiones son derivadas del país principal; no se deben ajustar simultáneamente región y país",
 "Se excluyen cinco estudios de regiones infrecuentes del M4 principal, pero no de descriptivos",
 "Desconocida la superposición de pacientes entre publicaciones",
 "Odds proporcionales del M8 solo diagnosticadas mediante análisis por umbral, no verificadas formalmente",
 "Las OR de regresión de respuesta fraccional no son OR de eventos clínicos",
 "Las categorías y puntos de corte son operacionales, no instrumentos oficiales JBI/STROBE"))
# Tablas con denominadores y frecuencias
COUNTS <- data.frame(outcome=c("Participación geriátrica", "Evaluación geriátrica integral", "Ingreso medio alto",
 "STROBE >=75%", "Diseño especificado", "Estadística multivariable", "Cohorte JBI"),
 numerator=c(sum(D$geriatric),sum(D$cga),sum(D$income_upper),sum(D$strobe_high),
 sum(D$method_specific),sum(D$advanced),sum(D$cohort)),
 denominator=c(128,128,128,128,128,128,128))
write.csv(coef_table,file.path(out,"coeficientes_modelos_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(sensitivity_table,file.path(out,"sensibilidad_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(D,file.path(out,"datos_derivados_publicacion.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(LOO,file.path(out,"influencia_LOO_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
openxlsx::write.xlsx(list(Modelos=coef_table,Sensibilidad=sensitivity_table,
 Datos_derivados=D,Influencia_LOO=LOO,Conteos=COUNTS,Auditoria=AUD),
 file.path(out,"Suplemento_modelos_prioritarios_R.xlsx"),overwrite=TRUE)
# Word reproducible en estilo sencillo; no agrupar todos los contrastes en una tabla ilegible.
doc <- officer::read_docx()
doc <- officer::body_add_par(doc,"Modelos prioritarios del manuscrito 1",style="heading 1")
doc <- officer::body_add_par(doc,"Unidad de análisis: publicación (N = 128). Asociaciones y estimadores exploratorios; sin interpretación causal.")
for(mid in unique(coef_table$model)){
 tab <- subset(coef_table,model==mid)
 if(nrow(tab)==0) next
 fmt <- tab[,c("term","comparison","estimate","low","high","p","q_BH_global"),drop=FALSE]
 for(z in c("estimate","low","high")) fmt[[z]] <- ifelse(is.finite(fmt[[z]]),formatC(fmt[[z]],digits=3,format="f"),"NA")
 for(z in c("p","q_BH_global")) fmt[[z]] <- ifelse(is.na(fmt[[z]]),"—",formatC(fmt[[z]],digits=4,format="f"))
 doc <- officer::body_add_par(doc,mid,style="heading 2")
 doc <- flextable::body_add_flextable(doc,flextable::autofit(flextable::flextable(fmt)))
}
doc <- officer::body_add_par(doc,"Limitaciones y diagnósticos",style="heading 2")
for(i in seq_len(nrow(AUD)))doc <- officer::body_add_par(doc,paste0("• ",AUD$incidencia[i]))
print(doc,target=file.path(out,"Tablas_modelos_prioritarios_R.docx"))
capture.output(sessionInfo(),file=file.path(out,"sessionInfo_R.txt"))
message("Ejecutado. Verificar convergencia, perfiles, influencias y cotejar con resultados de Python.")
