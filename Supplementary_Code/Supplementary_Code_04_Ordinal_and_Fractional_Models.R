# PROYECTO DOCTORAL | MANUSCRITO 1 | MODELOS MULTIVARIABLES
# Versión 2026-11-14 | LECTURA DIRECTA DE LA MATRIZ V7 | SIN IMPUTACION
# Este archivo es código fuente reproducible NO ejecutado en R en el entorno de entrega.
# Los resultados Python documentados no son necesariamente idénticos a los obtenidos con
# ordinal::clmm (efectos sujetos-específicos) ni al ajuste de GEE marginal (estimandos distintos).
# Ejecución: Rscript analisis_modelos_P1_R.R "matriz_extendida V7 - TOTAL FINAL.xlsx" "salidas_R"

args <- commandArgs(trailingOnly=TRUE)
input <- if (length(args)>=1) args[1] else "matriz_extendida V7 - TOTAL FINAL.xlsx"
out_dir <- if (length(args)>=2) args[2] else "salidas_R"
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
required <- c("readxl","ordinal","geepack","sandwich","lmtest","logistf","lme4")
absent <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(absent)>0) stop("Instale los paquetes necesarios: ",paste(absent,collapse=", "))
if (!file.exists(input)) stop("No se encuentra la matriz de entrada: ",input)
D <- as.data.frame(readxl::read_excel(input, sheet="Sheet1",.name_repair="unique"),check.names=FALSE)
stopifnot(nrow(D)==128,ncol(D)>=375)

clean_text <- function(x) trimws(ifelse(is.na(x), "", as.character(x)))
parse_number <- function(x) suppressWarnings(as.numeric(gsub(",",".",clean_text(x),fixed=TRUE)))
raw_year <- parse_number(D[[4]])
raw_n <- parse_number(D[[61]])
if (any(!is.finite(raw_year)|!is.finite(raw_n)|raw_n<=0)) stop("Año o tamaño muestral no válido: evaluar filas")
log_n_ref <- mean(log10(raw_n))
P <- data.frame(estudio_id=seq_len(nrow(D)),
      titulo=clean_text(D[[1]]),doi=clean_text(D[[2]]),
      revista=clean_text(D[[7]]),diseno=clean_text(D[[50]]),
      ano=raw_year,year5=(raw_year-2016)/5,
      n_pacientes=raw_n,log10n_center=log10(raw_n)-log_n_ref,
      ortho=as.integer(clean_text(D[[42]])=="Ortopedia y Traumatología"),
      cohort=as.integer(clean_text(D[[50]])=="Estudio de Cohorte"),
      geriatric=as.integer(clean_text(D[[60]])=="Sí"),
      stringsAsFactors=FALSE)

lvls <- c("No cumple","Cumple parcialmente","Cumple")
make_long <- function(ids,include_only_cohort=FALSE) {
  blocks <- lapply(ids, function(j) {
    x <- clean_text(D[[j]])
    data.frame(estudio_id=P$estudio_id,item=names(D)[j],
               criterio_columna=j,y_raw=x, stringsAsFactors=FALSE)
  })
  T <- do.call(rbind,blocks)
  valid <- T$y_raw %in% lvls
  if(include_only_cohort) valid <- valid & P$cohort[T$estudio_id]==1L
  T <- T[valid,,drop=FALSE]
  T$y <- ordered(T$y_raw,levels=lvls)
  T$complete <- as.integer(T$y_raw=="Cumple")
  matchP <- match(T$estudio_id,P$estudio_id)
  for (v in c("year5","log10n_center","ortho","cohort","revista")) T[[v]] <- P[[v]][matchP]
  T$item <- factor(T$item)
  T$estudio_id <- as.integer(T$estudio_id)
  rownames(T) <- NULL
  T
}
ST <- make_long(302:323, FALSE)
JB <- make_long(328:338, TRUE)
if(nrow(ST)!=2809 || nrow(JB)!=605) warning("Número de respuestas distinto de auditoría previa; revisar matriz")
write.csv(ST,file.path(out_dir,"datos_STROBE_largos.csv"),row.names=FALSE,fileEncoding="UTF-8")
write.csv(JB,file.path(out_dir,"datos_JBI_cohortes_largos.csv"),row.names=FALSE,fileEncoding="UTF-8")

P$strobe_aplicable <- vapply(P$estudio_id,function(id) sum(ST$estudio_id==id),integer(1))
P$strobe_puntos <- vapply(P$estudio_id,function(id) sum(c(0,.5,1)[as.integer(ST$y[ST$estudio_id==id])]),numeric(1))
P$strobe_frac <- P$strobe_puntos/P$strobe_aplicable
stopifnot(all(is.finite(P$strobe_frac)),all(P$strobe_frac>=0 & P$strobe_frac<=1))
write.csv(P,file.path(out_dir,"datos_modelos_por_publicacion.csv"),row.names=FALSE,fileEncoding="UTF-8")

# Guardar estimadores en una tabla común: coeficientes, EE e IC95%.
coefs <- list();notes <- list()
record <- function(model,component,fit,log_odds=TRUE){
  t <- tryCatch(coef(summary(fit)), error=function(e) NULL)
  if(is.null(t)) return(NULL)
  nm <- rownames(t); ec <- which(tolower(colnames(t)) %in% c("estimate","coef","coefficient","value"))[1]
  sc <- grep("std",tolower(colnames(t)))[1]
  if(is.na(ec)||is.na(sc)) return(NULL)
  for(i in seq_len(nrow(t))){
    b <- as.numeric(t[i,ec]);se <- as.numeric(t[i,sc]);
    q <- data.frame(modelo=model,componente=component,variable=nm[i],
      beta=b,EE=se,LI95=b-qnorm(.975)*se,LS95=b+qnorm(.975)*se,
      OR=if(log_odds) exp(b) else NA_real_,
      OR_LI95=if(log_odds) exp(b-qnorm(.975)*se) else NA_real_,
      OR_LS95=if(log_odds) exp(b+qnorm(.975)*se) else NA_real_,
      p_valor=2*pnorm(-abs(b/se)),stringsAsFactors=FALSE)
    coefs[[length(coefs)+1]] <<- q
  }
}

# MODELO 1 PRINCIPAL PROPUESTO: logit ordinal multinivel de interceptos cruzados.
# OR condicional a los efectos aleatorios, NO intercambiable con OR marginal.
tryCatch({
  m1 <- ordinal::clmm(y ~ year5 + log10n_center + ortho + cohort +
     (1|estudio_id) + (1|item),data=ST,link="logit",Hess=TRUE,nAGQ=1)
  record("M1_STROBE_clmm","principal",m1)
  capture.output(summary(m1),file=file.path(out_dir,"M1_resumen_clmm.txt"))
  saveRDS(m1,file.path(out_dir,"M1_modelo.rds"))
},error=function(e){notes[[length(notes)+1]] <<- paste("M1 no estimado:",conditionMessage(e))})

# MODELO 2: ordinal JBI exclusivamente 55 cohortes, interceptos cruzados.
tryCatch({
  m2 <- ordinal::clmm(y ~ year5 + log10n_center + ortho +
     (1|estudio_id) + (1|item),data=JB,link="logit",Hess=TRUE,nAGQ=1)
  record("M2_JBI_clmm","principal",m2)
  capture.output(summary(m2),file=file.path(out_dir,"M2_resumen_clmm.txt"))
  saveRDS(m2,file.path(out_dir,"M2_modelo.rds"))
},error=function(e){notes[[length(notes)+1]] <<- paste("M2 no estimado:",conditionMessage(e))})

# SENSIBILIDAD M1/M2: respuestas binarias con random intercepts cruzados; evita
# interpretar 2.809/605 filas como observaciones independientes.
for(kind in c("STROBE","JBI")){
 T <- if(kind=="STROBE") ST else JB
 f <- if(kind=="STROBE")
    complete ~ year5 + log10n_center + ortho + cohort + (1|estudio_id)+(1|item)
 else complete ~ year5 + log10n_center + ortho +(1|estudio_id)+(1|item)
 tryCatch({
   fit <- lme4::glmer(f, data=T, family=binomial(link="logit"),
     control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
   record(paste0(kind,"_binaria_cruzada"),"sensibilidad",fit)
   capture.output(summary(fit),file=file.path(out_dir,paste0(kind,"_binaria_cruzada.txt")))
   saveRDS(fit,file.path(out_dir,paste0(kind,"_binaria_cruzada.rds")))
 },error=function(e){notes[[length(notes)+1]] <<- paste("Sensitivity",kind,conditionMessage(e))})
}

# MODELO 3: logit de respuesta fraccional de STROBE (128 publicaciones).
# Uso de quasibinomial por media fraccional, no de una binomial de pacientes.
m3 <- glm(strobe_frac ~ year5 + log10n_center + ortho + cohort,
          data=P,family=quasibinomial(link="logit"))
V3 <- sandwich::vcovHC(m3,type="HC3")
ct3 <- lmtest::coeftest(m3,vcov.=V3)
for(i in seq_len(nrow(ct3))){
 b <- ct3[i,1];se <- ct3[i,2]
 coefs[[length(coefs)+1]] <- data.frame(modelo="M3_fraccional",componente="principal",variable=rownames(ct3)[i],
 beta=b,EE=se,LI95=b-1.96*se,LS95=b+1.96*se,
 OR=exp(b),OR_LI95=exp(b-1.96*se),OR_LS95=exp(b+1.96*se),p_valor=2*pnorm(-abs(b/se)))
}
saveRDS(m3,file.path(out_dir,"M3_modelo.rds"))

# MODELO 4: eventos raros (11/128), Firth con IC de perfil en lugar de Wald.
m4 <- logistf::logistf(geriatric ~ year5 + ortho,data=P,pl=TRUE)
for(i in seq_along(m4$coefficients)){
 b <- unname(m4$coefficients[i])
 coefs[[length(coefs)+1]] <- data.frame(modelo="M4_Firth",componente="principal",variable=names(m4$coefficients)[i],
 beta=b,EE=unname(sqrt(diag(m4$var))[i]),LI95=unname(m4$ci.lower[i]),LS95=unname(m4$ci.upper[i]),
 OR=exp(b),OR_LI95=exp(m4$ci.lower[i]),OR_LS95=exp(m4$ci.upper[i]),p_valor=unname(m4$prob[i]))
}
capture.output(summary(m4),file=file.path(out_dir,"M4_Firth_resumen.txt"))
saveRDS(m4,file.path(out_dir,"M4_modelo.rds"))

# MODELO 5: SENSIBILIDAD por REVISTA (no por cohortes/pacientes compartidos).
# La matriz carece de ID de cohorte/base nacional; esta prueba NO resuelve solapamiento de datos.
V5 <- sandwich::vcovCL(m3,cluster=P$revista,type="HC1")
ct5 <- lmtest::coeftest(m3,vcov.=V5)
for(i in seq_len(nrow(ct5))){
 b <- ct5[i,1];se <- ct5[i,2]
 coefs[[length(coefs)+1]] <- data.frame(modelo="M5_SE_por_revista",componente="sensibilidad",variable=rownames(ct5)[i],
 beta=b,EE=se,LI95=b-1.96*se,LS95=b+1.96*se,
 OR=exp(b),OR_LI95=exp(b-1.96*se),OR_LS95=exp(b+1.96*se),p_valor=2*pnorm(-abs(b/se)))
}

# Exportar con distinción de pruebas y familias; los p no se deben interpretar como confirmatorios.
if(length(coefs)>0) {
 ans <- do.call(rbind,coefs)
 ans$q_BH <- p.adjust(ans$p_valor,method="BH")
 write.csv(ans,file.path(out_dir,"coeficientes_completos_R.csv"),row.names=FALSE,fileEncoding="UTF-8")
}
if(length(notes)>0) writeLines(unlist(notes),file.path(out_dir,"incidencias_de_ajuste.txt"))
capture.output(sessionInfo(),file=file.path(out_dir,"session_info_R.txt"))
message("Fin del análisis. REVISAR ADVERTENCIAS Y DIAGNOSTICOS ANTES DE PUBLICAR: ",out_dir)
