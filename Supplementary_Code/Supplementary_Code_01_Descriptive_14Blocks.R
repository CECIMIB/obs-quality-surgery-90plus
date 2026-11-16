# Reproducibilidad de descriptivos por 14 bloques de la matriz V7
# Unidad: publicación científica (128 registros). Sin imputación ni inferencia.
# Uso: Rscript reproducir_14_tablas_V7.R \
#   "matriz_extendida V7 - TOTAL FINAL.xlsx" \
#   "Diccionario_370_variables_14_bloques.csv" \
#   "resultados_descriptivos"
# Paquetes requeridos: readxl, writexl, officer y flextable.
# La versión original de los resultados fue calculada en Python y el script R
# debe ejecutarse y cotejarse en R antes de su depósito como código validado.

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1L) args[[1L]] else "matriz_extendida V7 - TOTAL FINAL.xlsx"
dict_file  <- if (length(args) >= 2L) args[[2L]] else "Diccionario_370_variables_14_bloques.csv"
out_dir    <- if (length(args) >= 3L) args[[3L]] else "salidas_descriptivas"
if (!file.exists(input_file)) stop("No se encontró la matriz: ", input_file)
if (!file.exists(dict_file)) stop("No se encontró el diccionario: ", dict_file)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
for (p in c("readxl", "writexl", "officer", "flextable")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Se requiere instalar el paquete: ", p)
}

# Leer todas las celdas como texto para evitar inferencia y pérdida de categorías.
x <- readxl::read_excel(input_file, sheet = 1, col_names = FALSE,
                        col_types = "text", .name_repair = "minimal")
if (ncol(x) != 375L) stop("Se esperaban 375 columnas físicas; se hallaron ", ncol(x))
head <- vapply(seq_len(ncol(x)), function(j) {
  v <- x[[j]][1L]
  if (is.na(v)) "" else trimws(as.character(v))
}, character(1))
D <- x[-1L, , drop = FALSE]
valid_row <- !is.na(D[[1L]]) & nzchar(trimws(as.character(D[[1L]])))
D <- D[valid_row, , drop = FALSE]
N <- nrow(D)
if (N != 128L) stop("Se esperaban 128 publicaciones; se hallaron ", N)

# Los nombres científicos provienen de un diccionario trazable de 370 variables.
dict <- utils::read.csv(dict_file, fileEncoding = "UTF-8-BOM",
                        check.names = FALSE, stringsAsFactors = FALSE,
                        na.strings = character(0))
if (nrow(dict) != 370L || length(unique(dict$nombre_final)) != 370L) {
  stop("El diccionario debe contener 370 nombres finales únicos")
}
if (!identical(as.integer(dict$indice_excel), as.integer(sort(dict$indice_excel)))) {
  stop("El diccionario no está ordenado de izquierda a derecha")
}
if (any(head[as.integer(dict$indice_excel)] != dict$nombre_matriz)) {
  stop("Hay discrepancia entre los encabezados de la matriz y el diccionario")
}
if (!identical(sort(unique(as.integer(dict$tabla))), seq_len(14L))) {
  stop("Se requieren 14 bloques")
}

# Excluir exclusivamente códigos explícitos de ausencia de información.
missing_values <- c("", "no reportado", "no aplica", "n/a", "na", "sin dato",
                    "sin datos", "no disponible", "not reported", "not available",
                    "nr", "n.r.", "no informado", "sin información", "sin informacion")
text_value <- function(v) {
  out <- trimws(as.character(v)); out[is.na(v)] <- ""; out
}
is_missing <- function(v) {
  tolower(text_value(v)) %in% missing_values
}
get_column <- function(j) text_value(D[[as.integer(j)]])
num_value <- function(v, original) {
  a <- text_value(v)
  a[is_missing(a)] <- NA_character_
  # Un valor centinela mal codificado NO se trata como medición válida.
  if (original %in% c("n_categorias", "n_dos_o_mas")) a[a == "1282"] <- NA_character_
  a <- gsub("[[:space:]]", "", a)
  a <- gsub("%", "", a, fixed = TRUE)
  both <- !is.na(a) & grepl(",", a, fixed=TRUE) & grepl(".", a, fixed=TRUE)
  eu <- both & regexpr(",[^,]*$", a) > regexpr("\\.[^.]*$", a)
  eu[is.na(eu)] <- FALSE
  a[eu] <- gsub("\\.", "", a[eu])
  a[eu] <- sub(",", ".", a[eu], fixed = TRUE)
  us <- both & !eu
  us[is.na(us)] <- FALSE
  a[us] <- gsub(",", "", a[us], fixed = TRUE)
  other <- !both & !is.na(a)
  other[is.na(other)] <- FALSE
  a[other] <- gsub(",", ".", a[other], fixed = TRUE)
  z <- suppressWarnings(as.numeric(a))
  z[!is.finite(z)] <- NA_real_
  z
}

fmt1 <- function(z) {
  if (is.na(z)) return("—")
  if (z != 0 && abs(z) < 0.005) return(sub("\\.", ",", sprintf("%.4f", z)))
  sub("\\.", ",", sprintf("%.1f", z))
}
pct <- function(n, d) {
  if (d == 0L) return("—")
  paste0(n, "/", d, " (", fmt1(100*n/d), "%)")
}
# Convención descriptiva para decidir media(DE) frente a mediana(Q1-Q3).
num_descrip <- function(z) {
  z <- z[is.finite(z)];n <- length(z)
  if (!n) return(list(n=0L, mean=NA_real_, sd=NA_real_, med=NA_real_,
                      q1=NA_real_, q3=NA_real_, min=NA_real_, max=NA_real_,
                      skew=NA_real_, shapiro=NA_real_, metodo="Sin datos",
                      display=paste0("Sin observaciones cuantitativas válidas; n=0/", N)))
  media <- mean(z)
  desvest <- if (n>1L) stats::sd(z) else NA_real_
  med <- stats::median(z)
  qs <- as.numeric(stats::quantile(z, probs=c(.25,.75), type=7, names=FALSE))
  # Momento central estandarizado sin corrección por sesgo, como scipy.stats.skew(bias=TRUE).
  s0 <- sqrt(mean((z-media)^2))
  asim <- if(n>=3L && s0>0) mean((z-media)^3)/s0^3 else NA_real_
  sw <- if(n>=8L && length(unique(z))>=3L && n<=5000L) {
    unname(stats::shapiro.test(z)$p.value)
  } else NA_real_
  metodo <- if (n>=8L && !is.na(sw) && sw>=.05 && !is.na(asim) && abs(asim)<1) {
    "Media (DE)"
  } else "Mediana (Q1–Q3)"
  display <- if (metodo=="Media (DE)") {
    paste0(fmt1(media), " (",fmt1(desvest), "); n=", n, "/",N)
  } else {
    paste0(fmt1(med), " (",fmt1(qs[1]), "–",fmt1(qs[2]), "); n=", n, "/",N)
  }
  list(n=n,mean=media,sd=desvest,med=med,q1=qs[1],q3=qs[2],
       min=min(z),max=max(z),skew=asim,shapiro=sw,metodo=metodo,display=display)
}

blocks <- list()
all_freq <- list();all_num <- list();audit <- list();issues <- list()
for (b in seq_len(14L)) {
  block <- dict[as.integer(dict$tabla)==b,,drop=FALSE]
  br <- list()
  for (k in seq_len(nrow(block))) {
    r <- block[k,,drop=FALSE]
    j <- as.integer(r$indice_excel)
    original <- as.character(r$nombre_matriz)
    final <- as.character(r$nombre_final)
    kind <- as.character(r$tipo)
    vals <- get_column(j)
    if (kind == "numérica") {
      nn <- num_value(vals,original)
      ns <- num_descrip(nn)
      rows <- data.frame(Variable=final, Medida=ns$display, check.names=FALSE)
      br[[length(br)+1L]] <- rows
      all_num[[length(all_num)+1L]] <- data.frame(
        Tabla=b, Dominio=as.character(r$dominio), Columna=as.character(r$columna),
        Nombre_matriz=original, Nombre_final=final, N_valido=ns$n,
        N_total=N, N_faltante=N-ns$n, Media=ns$mean, DE=ns$sd,
        Mediana=ns$med, Q1=ns$q1, Q3=ns$q3, Minimo=ns$min,
        Maximo=ns$max, Asimetria=ns$skew, Shapiro_p=ns$shapiro,
        Metodo=ns$metodo, Medida=ns$display, check.names=FALSE)
      bad <- vals[!is_missing(vals) & is.na(nn)]
      if(length(bad)) issues[[length(issues)+1L]] <- data.frame(
        Tabla=b, Columna=as.character(r$columna), Variable=original,
        Tipo="Valor numérico no interpretable", N_celdas=length(bad),
        Detalle=paste(unique(bad),collapse=" | "),check.names=FALSE)
      audit[[length(audit)+1L]] <- data.frame(Tabla=b, Columna=as.character(r$columna),
        Nombre_matriz=original, Nombre_final=final, Tipo=kind,
        N_total=N, N_valido=ns$n, N_faltante=N-ns$n,
        Porc_faltante=100*(N-ns$n)/N,check.names=FALSE)
    } else {
      valid <- vals[!is_missing(vals)]
      nn <- length(valid)
      lev <- unique(valid)
      if (kind == "texto libre / identificador") {
        md <- paste0(nn,"/",N," (",fmt1(100*nn/N),
                     "%) con información; ",length(lev)," valores distintos")
        br[[length(br)+1L]] <- data.frame(Variable=final,Medida=md,check.names=FALSE)
      } else if (nn==0L) {
        br[[length(br)+1L]] <- data.frame(Variable=final,
          Medida=paste0("Sin información válida; N=0/",N),check.names=FALSE)
      } else {
        counts <- vapply(lev,function(z)sum(valid==z),integer(1))
        br[[length(br)+1L]] <- data.frame(
          Variable=paste0(final," — ",lev),
          Medida=vapply(counts,function(ct)pct(ct,nn),character(1)),
          check.names=FALSE)
      }
      if(nn>0L) for (z in lev) {
        nlevel <- sum(valid==z)
        all_freq[[length(all_freq)+1L]] <- data.frame(
          Tabla=b, Dominio=as.character(r$dominio), Columna=as.character(r$columna),
          Nombre_matriz=original,Nombre_final=final,Tipo=kind,
          Categoria_original=z,n=nlevel,N_valido=nn,N_total=N,N_faltante=N-nn,
          Porcentaje=100*nlevel/nn,Medida=pct(nlevel,nn),check.names=FALSE)
      }
      audit[[length(audit)+1L]] <- data.frame(Tabla=b, Columna=as.character(r$columna),
        Nombre_matriz=original, Nombre_final=final, Tipo=kind,
        N_total=N, N_valido=nn, N_faltante=N-nn,
        Porc_faltante=100*(N-nn)/N,check.names=FALSE)
    }
  }
  blocks[[b]] <- do.call(rbind,br)
  rownames(blocks[[b]]) <- NULL
}

# Registro completo, numérico y categórico, con N propio en cada columna.
audit_df <- do.call(rbind,audit)
num_df <- do.call(rbind,all_num)
freq_df <- do.call(rbind,all_freq)
issues_df <- if(length(issues)) do.call(rbind,issues) else {
  data.frame(Tabla=integer(),Columna=character(),Variable=character(),
             Tipo=character(),N_celdas=integer(),Detalle=character())
}
block_summary <- do.call(rbind,lapply(seq_len(14L),function(i){
  z <- dict[as.integer(dict$tabla)==i,,drop=FALSE]
  data.frame(Tabla=i,Dominio=z$dominio[1L],Desde=z$columna[1L],
             Hasta=z$columna[nrow(z)],Variables=nrow(z),
             Filas_resultado=nrow(blocks[[i]]),check.names=FALSE)
}))
excluded <- data.frame(Columna=c("AN","LN","LO"),
  Motivo=rep("Fuera de los 14 rangos expresamente definidos",3),
  check.names=FALSE)

# Salida Excel: una hoja por tabla, más trazabilidad completa.
book <- list(
  LEEME=data.frame(Aspecto=c("Unidad","Matriz","Bloques","Faltantes cualitativos",
          "Textos libres","Cuantitativas","Centinela","Fuente"),
          Regla=c("Publicación científica","128 estudios / 375 columnas físicas",
          "14 rangos exactos y 370 variables; AN, LN y LO fuera de los rangos",
          "N propio de cada variable. No reportado y No aplica son faltantes; Sin declaración es válido",
          "Word resume cobertura; categorías textuales completas en FRECUENCIAS",
          "Media(DE) o Mediana(Q1-Q3), con n válido/128",
          "1282 excluido de n_categorias y n_dos_o_mas",
          basename(input_file)), check.names=FALSE),
  RESUMEN_BLOQUES=block_summary
)
for (i in seq_len(14L)) {
  z <- block_summary[i,,drop=FALSE]
  nm <- paste0(sprintf("Tabla_%02d_",i),z$Desde,"_",z$Hasta)
  book[[nm]] <- blocks[[i]]
}
book$DICCIONARIO_370 <- dict
book$AUDITORIA_MISSING <- audit_df
book$FRECUENCIAS_COMPLETAS <- freq_df
book$NUMERICOS_COMPLETOS <- num_df
book$INCIDENCIAS <- issues_df
book$EXCLUSIONES <- excluded
writexl::write_xlsx(book,path=file.path(out_dir,"Suplemento_descriptivos_V7_14_bloques_R.xlsx"))

# Word: 14 tablas, dos columnas, nombres finales únicos, orden exacto de Excel.
doc <- officer::read_docx()
doc <- officer::body_add_par(doc,"Tablas descriptivas de la matriz V7",style="heading 1")
doc <- officer::body_add_par(doc,
   "128 publicaciones. 14 bloques de columnas, 370 variables. Unidad de análisis: publicación científica.")
doc <- officer::body_add_par(doc,
   "Cualitativas: n/N (%) con denominador válido propio de la variable. Cuantitativas: media (DE) o mediana (Q1–Q3) con n válido. Datos faltantes no imputados. Textos libres resumidos en Word y detallados en el suplemento. Se excluyen AN, LN y LO de acuerdo con los rangos indicados.")
for(i in seq_len(14L)) {
  z <- block_summary[i,,drop=FALSE]
  doc <- officer::body_add_par(doc,
     paste0("Tabla ",i,". ",z$Dominio," (",z$Desde,"–",z$Hasta,")"),style="heading 2")
  ft <- flextable::flextable(blocks[[i]])
  ft <- flextable::set_header_labels(ft,Variable="Variable",Medida="Medida descriptiva")
  ft <- flextable::font(ft,fontname="Times New Roman",part="all")
  ft <- flextable::fontsize(ft,size=9,part="all")
  ft <- flextable::bold(ft,part="header")
  ft <- flextable::set_table_properties(ft,layout="fixed")
  ft <- flextable::width(ft,j=1,width=4.22)
  ft <- flextable::width(ft,j=2,width=2.02)
  ft <- flextable::padding(ft,padding=2,part="all")
  doc <- flextable::body_add_flextable(doc,ft)
  doc <- officer::body_add_par(doc,
     paste0("Nota. ",z$Variables," variables. El detalle de faltantes y las frecuencias individuales se presentan en el Excel."))
  if(i<14L) doc <- officer::body_add_break(doc)
}
print(doc,target=file.path(out_dir,"Tablas_descriptivas_V7_C_a_NK_14_bloques_R.docx"))
utils::write.csv(block_summary,file.path(out_dir,"RESUMEN_BLOQUES.csv"),row.names=FALSE,fileEncoding="UTF-8")
utils::write.csv(audit_df,file.path(out_dir,"AUDITORIA_MISSING.csv"),row.names=FALSE,fileEncoding="UTF-8")
utils::write.csv(freq_df,file.path(out_dir,"FRECUENCIAS_COMPLETAS.csv"),row.names=FALSE,fileEncoding="UTF-8")
utils::write.csv(num_df,file.path(out_dir,"NUMERICOS_COMPLETOS.csv"),row.names=FALSE,fileEncoding="UTF-8")
capture.output(sessionInfo(),file=file.path(out_dir,"R_sessionInfo.txt"))
cat("FINALIZADO: 14 tablas; 370 variables; 128 publicaciones.\n")
