# MODEL REPRODUCTION | STROBE and JBI associations | exploratory V7 analysis
# Source: 'matriz_extendida V7 - TOTAL FINAL(1).xlsx', sheet Sheet1 (128 records, 375 columns).
# Prepared for publication-level analyses, NOT patient-level causal inference.
# Recreates: 41 binary factor screens, 6 categorical tests, 8 JBI OLS sensitivities,
# and 2 exploratory joint models, along with BH corrections and audit CSVs.
# Original prior reference results were produced in Python (statsmodels/SciPy).
# Implementation here is independent R. Small numerical differences may arise
# with HC3 covariance conventions, Wald tests, ties, and bootstrap RNG.
# The exploratory p-values are NOT confirmatory and null p does NOT imply equivalence.
# Run: Rscript reproducir_asociaciones_STROBE_JBI_V7.R input.xlsx output_folder
# Or: source('reproducir_asociaciones_STROBE_JBI_V7.R', encoding='UTF-8')

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[[1]] else "matriz_extendida V7 - TOTAL FINAL(1).xlsx"
outdir <- if (length(args) >= 2L) args[[2]] else "Resultados_R_STROBE_JBI_ampliados"
req <- c("readxl", "sandwich", "MASS", "openxlsx")
missing_pkgs <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) stop("Install required R packages: ", paste(missing_pkgs, collapse = ", "))
if (!file.exists(input)) stop("Input V7 workbook not found: ", normalizePath(input, mustWork = FALSE))
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
raw <- as.data.frame(readxl::read_excel(input, sheet = "Sheet1", .name_repair = "minimal"), check.names = FALSE)
if (nrow(raw) != 128L || ncol(raw) < 375L) stop("Expected 128 publications and at least 375 columns in Sheet1.")
expected <- c(`4` = "Ano_Publicacion", `15` = "colab_internacional", `42` = "especialidad_quirurgica",
              `50` = "diseno_metodologico", `61` = "tamano_muestra", `325` = "Cumplimiento_ponderado_estudio_%")
for (jj in names(expected)) {
  if (!identical(names(raw)[as.integer(jj)], unname(expected[jj]))) {
    warning(sprintf("Column %s is named '%s' rather than '%s'; checking numeric layout.", jj,
                    names(raw)[as.integer(jj)], unname(expected[jj])))
  }
}
clean <- function(x) { y <- as.character(x); y[is.na(y)] <- ""; trimws(y) }
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", clean(x), fixed = TRUE)))
value <- function(k) raw[[as.integer(k)]]
N <- nrow(raw)
year <- num(value(4)); samp <- num(value(61));
if (any(!is.finite(year)) || any(!is.finite(samp) | samp <= 0)) stop("Invalid year or sample size in original data.")
cohort <- clean(value(50)) == "Estudio de Cohorte"
ortho <- as.numeric(clean(value(42)) == "Ortopedia y Traumatología")
strobe <- num(value(325)); strobe_high <- as.integer(strobe >= .75)
jbi_raw <- num(value(339)); jbi_prop <- jbi_raw / 11
if (sum(cohort) != 55L || sum(strobe_high) != 81L || sum(1L - strobe_high) != 47L ||
    any(!is.finite(strobe)) || any(!is.finite(jbi_prop[cohort]))) stop("Core V7 outcomes mismatch.")
# Global centering is important: match the original exploratory models.
base <- data.frame(year5 = (year - 2016) / 5,
                   log10nc = log10(samp) - mean(log10(samp)),
                   ortho = ortho)

# Robust HC3 Wald covariance. Heteroskedasticity adjustment is estimation-specific.
robust <- function(mod) {
  V <- sandwich::vcovHC(mod, type = "HC3")
  if (any(!is.finite(V))) stop("Invalid HC3 covariance matrix")
  V
}
walddetails <- function(model, term) {
  b <- unname(stats::coef(model)[term]); V <- robust(model)
  se <- sqrt(unname(diag(V)[term])); zz <- b / se
  if (length(b) != 1L || !is.finite(b + se + zz) || se >= 30) stop("Coefficient not estimable")
  c(est = exp(b), low = exp(b - stats::qnorm(.975) * se),
    high = exp(b + stats::qnorm(.975) * se), p = 2 * stats::pnorm(-abs(zz)))
}
fit_binary_model <- function(y, exposure, mask, remove_ortho = FALSE, fractional = FALSE) {
  df <- base[mask, , drop = FALSE]; df$y <- y[mask]; df$x <- exposure[mask]
  formula <- if (remove_ortho) y ~ year5 + log10nc + ortho else y ~ year5 + log10nc + ortho + x
  term <- if (remove_ortho) "ortho" else "x"
  # Quasi-likelihood fractional logits have the same point estimates as binomial GLM.
  fam <- if (fractional) stats::quasibinomial(link = "logit") else stats::binomial(link = "logit")
  mod <- stats::glm(formula, data = df, family = fam, control = stats::glm.control(maxit = 200))
  out <- walddetails(mod, term)
  c(out, n = nrow(df))
}

# The exact original recoding rules. Unreported/blank ≠ "No".
recode_binary <- function(k, mode) {
  v <- clean(value(k)); x <- rep(NA_real_, N)
  invalid <- v %in% c("", "No reportado", "No aplica", "1282", "nan", "None")
  if (mode == "yes_si") {
    x[v %in% c("Sí", "Si")] <- 1; x[v == "No"] <- 0
  } else if (mode == "yes1") {
    zz <- num(value(k)); x[zz %in% c(0, 1)] <- zz[zz %in% c(0, 1)]
  } else if (mode == "highincome") {
    x[v == "High income"] <- 1; x[v == "Upper middle income"] <- 0
  } else if (mode == "Q1") {
    x[v == "Q1"] <- 1; x[v %in% c("Q2", "Q3", "Q4")] <- 0
  } else if (mode == "prospective") {
    x[v == "Prospectivo"] <- 1; x[v == "Retrospectivo"] <- 0
  } else if (mode == "orthopaedics") {
    x[] <- as.numeric(v == "Ortopedia y Traumatología")
  } else if (mode == "vascular") {
    x[] <- as.numeric(v == "Cirugía Vascular y Endovascular")
  } else if (mode == "geriatrics") {
    x[] <- as.numeric(v == "Geriatria")
  } else if (mode == "hipfx") {
    x[] <- as.numeric(v == "Fractura de cadera")
  } else if (mode == "urgent") {
    x[v %in% c("Urgencia", "Electiva", "Ambas")] <- as.numeric(v[v %in% c("Urgencia", "Electiva", "Ambas")] == "Urgencia")
  } else if (mode == "elective") {
    x[v %in% c("Urgencia", "Electiva", "Ambas")] <- as.numeric(v[v %in% c("Urgencia", "Electiva", "Ambas")] == "Electiva")
  } else if (mode == "centenarian") {
    x[] <- as.numeric(v != "Nonagenarios")
  } else if (mode == "advanced") {
    x[] <- as.numeric(v == "Modelamiento multivariable o ajustado")
  } else if (mode == "designspecified") {
    x[] <- as.numeric(v != "Diseño metodológico inespecífico")
  } else if (mode %in% c("funding", "conflict", "ethics")) {
    x[] <- as.numeric(v != "Sin declaración")
  } else stop("Unknown recoding mode: ", mode)
  x[invalid] <- NA_real_
  x
}

predictors <- data.frame(
  factor = c("International collaboration","High-income country","Open access (article)","SJR quartile Q1","Prospective collection","Orthopaedic specialty","Vascular specialty","Geriatrics medical domain","Hip fracture studied","Urgent surgery only","Elective surgery only","Centenarians included","Advanced adjusted statistics","Mortality primary outcome","Complications primary outcome","Survival primary outcome","Primary functional outcome","Geriatrician involved","Geriatric phenotype defined","Comprehensive geriatric assessment","Frailty assessed","Nutritional status assessed","Cognitive status assessed","Functional independence assessed","Quality of life assessed","Rehabilitation evaluated","Polypharmacy assessed","Comorbidities assessed","ASA scale employed","Charlson index employed","Barthel Index employed","Hospital stay assessed","Reintervention assessed","Complications assessed","Functional loss documented","Primary risk prediction objective","Study design explicitly identified","Abstract structured","Funding reported","Conflict-of-interest declared","Ethical approval declared"),
  domain = c("Editorial/context","Editorial/context","Editorial/context","Editorial/context","Design/context","Clinical/surgical","Clinical/surgical","Clinical/surgical","Clinical/surgical","Clinical/surgical","Clinical/surgical","Clinical/surgical","Study analytical profile","Clinical outcomes","Clinical outcomes","Clinical outcomes","Clinical outcomes","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Geriatric assessment","Risk/geriatric scales","Risk/geriatric scales","Risk/geriatric scales","Clinical outcomes","Clinical outcomes","Clinical outcomes","Clinical outcomes","Clinical outcomes","Reporting-related","Reporting-related","Reporting-related","Reporting-related","Reporting-related"),
  column = c(15,17,56,31,51,42,42,43,44,47,47,41,53,159,161,160,162,60,67,68,69,71,75,73,72,76,78,77,84,85,122,81,82,80,83,177,50,48,54,57,59),
  recode = c("yes1","highincome","yes_si","Q1","prospective","orthopaedics","vascular","geriatrics","hipfx","urgent","elective","centenarian","advanced","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","yes_si","designspecified","yes_si","funding","conflict","ethics"),
  caution = c("Low-frequency category","","","SJR indexed journal categories","Small cohort positive group","Baseline predictor","","","","Exclude unreported urgency; three-level alternative preferred","Exclude unreported urgency; three-level alternative preferred","Small cohort positive group","Potential conceptual overlap with STROBE statistical reporting","","","","Rare functional primary objective","CGA (3) strongly linked to geriatrician involvement","Rare positive group","Extremely rare positive group","All 7 positive studies reach STROBE >=75%","","","","Rare positive group","","Rare positive group","Almost universal","21 without scale coding; excluded","21 without scale coding; excluded","Rare, 21 without scale coding","","","","","Rare positive group","Possible overlap with STROBE item 04; not an independent predictor","Possible overlap with STROBE item 01","Direct overlap with STROBE item 22","Editorial reporting-related proxy","Editorial reporting-related proxy"),
  stringsAsFactors = FALSE
)

# STEP 1 | Binary predictor audit and single-exposure adjusted models.
main <- vector("list", nrow(predictors))
for (i in seq_len(nrow(predictors))) {
  spec <- predictors[i, ]; x <- recode_binary(spec$column, spec$recode)
  v <- is.finite(x); cv <- v & cohort
  n0 <- sum(v & x == 0, na.rm = TRUE); n1 <- sum(v & x == 1, na.rm = TRUE)
  cn0 <- sum(cv & x == 0, na.rm = TRUE); cn1 <- sum(cv & x == 1, na.rm = TRUE)
  h1 <- sum(v & x == 1 & strobe_high == 1, na.rm = TRUE)
  l1 <- sum(v & x == 1 & strobe_high == 0, na.rm = TRUE)
  h0 <- sum(v & x == 0 & strobe_high == 1, na.rm = TRUE)
  l0 <- sum(v & x == 0 & strobe_high == 0, na.rm = TRUE)
  rec <- data.frame(domain = spec$domain, variable = spec$factor,
    source_col = spec$column, source_name = names(raw)[spec$column],
    n_strobe = n0 + n1, n1_strobe = n1, n0_strobe = n0,
    strobe_high_exposed = h1, strobe_low_exposed = l1,
    strobe_high_unexposed = h0, strobe_low_unexposed = l0,
    n_jbi = cn0 + cn1, n1_jbi = cn1, n0_jbi = cn0,
    caution = spec$caution, p_fisher = NA_real_, p_jbi_mw = NA_real_,
    or_strobe = NA_real_, or_strobe_lci = NA_real_, or_strobe_uci = NA_real_, p_adj_strobe = NA_real_,
    frac_odds_jbi = NA_real_, frac_odds_jbi_lci = NA_real_, frac_odds_jbi_uci = NA_real_, p_adj_jbi = NA_real_,
    strobe_eligibility = "Description only", jbi_eligibility = "Description only",
    mean_jbi_exposed = NA_real_, mean_jbi_unexposed = NA_real_, stringsAsFactors = FALSE)
  if (min(n0, n1) > 0L) {
    rec$p_fisher <- stats::fisher.test(matrix(c(h1, l1, h0, l0), nrow = 2, byrow = TRUE))$p.value
    if (min(n0, n1) >= 8L && min(h1, l1, h0, l0) >= 3L) {
      rec$strobe_eligibility <- "HC3-adjusted"
      st_error <- NULL
      tryCatch({
        mm <- fit_binary_model(strobe_high, x, v & is.finite(year) & is.finite(samp),
                               remove_ortho = spec$factor == "Orthopaedic specialty")
        rec$or_strobe <- mm["est"]; rec$or_strobe_lci <- mm["low"]
        rec$or_strobe_uci <- mm["high"]; rec$p_adj_strobe <- mm["p"]
      }, error = function(e) st_error <<- conditionMessage(e))
      if (!is.null(st_error)) rec$caution <- paste0(rec$caution, "; STROBE: ", st_error)
    } else rec$strobe_eligibility <- "Fisher only; insufficient cell counts"
  }
  if (min(cn0, cn1) > 0L) {
    yy1 <- jbi_prop[cv & x == 1]; yy0 <- jbi_prop[cv & x == 0]
    rec$mean_jbi_exposed <- 11 * mean(yy1); rec$mean_jbi_unexposed <- 11 * mean(yy0)
    rec$p_jbi_mw <- suppressWarnings(stats::wilcox.test(yy1, yy0, alternative = "two.sided", exact = FALSE, correct = TRUE)$p.value)
    if (min(cn0, cn1) >= 6L) {
      rec$jbi_eligibility <- "HC3 fractional logit adjusted"
      jbi_error <- NULL
      tryCatch({
        mm <- fit_binary_model(jbi_prop, x, cv & is.finite(year) & is.finite(samp),
                               remove_ortho = spec$factor == "Orthopaedic specialty", fractional = TRUE)
        rec$frac_odds_jbi <- mm["est"]; rec$frac_odds_jbi_lci <- mm["low"]
        rec$frac_odds_jbi_uci <- mm["high"]; rec$p_adj_jbi <- mm["p"]
      }, error = function(e) jbi_error <<- conditionMessage(e))
      if (!is.null(jbi_error)) rec$caution <- paste0(rec$caution, "; JBI: ", jbi_error)
    } else rec$jbi_eligibility <- "Rank test only; insufficient positive count"
  }
  main[[i]] <- rec
}
main <- do.call(rbind, main)
for (pair in list(c("p_fisher", "q_fisher"), c("p_jbi_mw", "q_jbi_mw"),
                  c("p_adj_strobe", "q_adj_strobe"), c("p_adj_jbi", "q_adj_jbi"))) {
  p <- main[[pair[1]]]; q <- rep(NA_real_, length(p)); use <- which(is.finite(p))
  q[use] <- stats::p.adjust(p[use], method = "BH"); main[[pair[2]]] <- q
}
for (pair in list(c("p_adj_strobe", "q_adj_strobe_primary"),
                  c("p_adj_jbi", "q_adj_jbi_primary"))) {
  p <- main[[pair[1]]]; q <- rep(NA_real_, length(p))
  use <- which(is.finite(p) & main$domain != "Reporting-related")
  q[use] <- stats::p.adjust(p[use], method = "BH"); main[[pair[2]]] <- q
}

# STEP 2 | Polytomous factors: robust global Wald tests with explicit references.
catdef <- data.frame(variable = c("Surgical urgency", "Surgical specialty", "Medical domain",
                                     "Geographic region", "SJR quartile", "Statistical complexity"),
  reference = c("Electiva", "Orthopaedic", "Orthopaedic domain", "North America", "Q1",
                "Inferencial sin ajuste multivariable"),
  adjust_ortho = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE), stringsAsFactors = FALSE)
category <- function(name) {
  vals <- switch(name,
    "Surgical urgency" = {v <- clean(value(47)); v[!v %in% c("Urgencia", "Electiva", "Ambas")] <- NA_character_; v},
    "Surgical specialty" = {v <- clean(value(42)); ifelse(v == "Ortopedia y Traumatología", "Orthopaedic",
                                                  ifelse(v == "Cirugía Vascular y Endovascular", "Vascular", "Other surgical"))},
    "Medical domain" = {v <- clean(value(43)); ifelse(v == "Ortopedia y Traumatología", "Orthopaedic domain",
                                           ifelse(v == "Geriatria", "Geriatric medicine",
                                                  ifelse(v == "Cirugía General", "General surgery", "Other domains")))},
    "Geographic region" = {v <- clean(value(16)); v[!v %in% c("North America", "Europe & Central Asia", "East Asia & Pacific")] <- NA_character_; v},
    "SJR quartile" = {v <- clean(value(31)); v[!v %in% c("Q1", "Q2", "Q3")] <- NA_character_; v},
    "Statistical complexity" = {v <- clean(value(53)); v[v == ""] <- NA_character_; v},
    stop("Unknown categorical factor"))
  as.character(vals)
}

multi <- list()
for (i in seq_len(nrow(catdef))) {
  s <- catdef[i, ]; z <- category(s$variable)
  lev <- c(s$reference, sort(setdiff(unique(stats::na.omit(z)), s$reference)))
  row <- data.frame(variable = s$variable, reference = s$reference,
                    levels = paste(lev, collapse = " | "), n128 = sum(!is.na(z)),
                    n55 = sum(cohort & !is.na(z)), stringsAsFactors = FALSE)
  for (kind in c("STROBE", "JBI")) {
    use <- !is.na(z) & (if (kind == "JBI") cohort else TRUE)
    ct <- vapply(lev, function(u) sum(use & z == u), integer(1))
    row[[paste0(kind, "_counts")]] <- paste(paste0(names(ct), ": ", ct), collapse = " | ")
    p <- NA_real_; terms <- "Not estimated: category under 5"
    if (all(ct >= 5L)) {
      df <- base[use, , drop = FALSE]; df$y <- if (kind == "STROBE") strobe_high[use] else jbi_prop[use]
      df$category <- factor(z[use], levels = lev)
      form <- if (s$adjust_ortho) y ~ year5 + log10nc + ortho + category else y ~ year5 + log10nc + category
      tryCatch({
        mod <- stats::glm(form, data = df,
          family = if (kind == "STROBE") stats::binomial(link = "logit") else stats::quasibinomial(link = "logit"),
          control = stats::glm.control(maxit = 200))
        b <- stats::coef(mod); V <- robust(mod)
        ix <- grep("^category", names(b)); beta <- b[ix]
        VV <- V[ix, ix, drop = FALSE]
        wald <- as.numeric(t(beta) %*% MASS::ginv(VV) %*% beta)
        p <- stats::pchisq(wald, df = length(ix), lower.tail = FALSE)
        terms <- paste(vapply(seq_along(ix), function(k) {
          se <- sqrt(VV[k, k]); sprintf("%s: %.2f [%.2f, %.2f]", lev[k + 1L],
            exp(beta[k]), exp(beta[k] - 1.96 * se), exp(beta[k] + 1.96 * se))
        }, character(1)), collapse = "; ")
      }, error = function(e) {
        p <<- NA_real_; terms <<- paste("Model failed:", conditionMessage(e))
      })
    }
    row[[paste0(kind, "_p_global")]] <- p
    row[[paste0(kind, "_terms")]] <- terms
  }
  multi[[i]] <- row
}
multi <- do.call(rbind, multi)

# STEP 3 | OLS HC3 sensitivities for cohort JBI score /11. Bootstrap uses R RNG,
# not NumPy RNG; point estimates and HC3 CI should closely match prior reference,
# while percentile-bootstrap endpoints need not match digit-for-digit.
olsdef <- data.frame(variable = c("Frailty assessed", "Advanced adjusted statistics", "Open access",
                                   "Hip fracture focus", "Charlson index employed", "Geriatrics medical domain",
                                   "Cognitive status assessed", "Functional independence assessed"),
                     column = c(69, 53, 56, 44, 85, 43, 75, 73),
                     mode = c("yes_si", "advanced", "yes_si", "hipfx", "yes_si", "geriatrics", "yes_si", "yes_si"))
set.seed(3293)
ols <- vector("list", nrow(olsdef))
for (i in seq_len(nrow(olsdef))) {
  s <- olsdef[i, ]; x <- recode_binary(s$column, s$mode)
  valid <- cohort & is.finite(x)
  df <- base[valid, , drop = FALSE]; df$y <- jbi_raw[valid]; df$x <- x[valid]
  mod <- stats::lm(y ~ year5 + log10nc + ortho + x, data = df)
  b <- unname(stats::coef(mod)["x"]); V <- robust(mod); se <- sqrt(unname(diag(V)["x"]))
  matrix_X <- stats::model.matrix(mod); y <- df$y; B <- numeric(0)
  for (j in seq_len(1000L)) {
    idx <- sample.int(nrow(df), nrow(df), replace = TRUE); XX <- matrix_X[idx, , drop = FALSE]
    if (qr(XX)$rank < ncol(XX)) next
    vv <- tryCatch(unname(stats::lm.fit(x = XX, y = y[idx])$coefficients["x"]), error = function(e) NA_real_)
    if (is.finite(vv)) B <- c(B, vv)
  }
  ci_boot <- if (length(B) > 100) unname(stats::quantile(B, c(.025, .975))) else c(NA_real_, NA_real_)
  leave <- vapply(seq_len(nrow(df)), function(j) {
    XX <- matrix_X[-j, , drop = FALSE]
    if (qr(XX)$rank < ncol(XX)) return(NA_real_)
    unname(stats::lm.fit(x = XX, y = y[-j])$coefficients["x"])
  }, numeric(1))
  ols[[i]] <- data.frame(Factor = s$variable, n = nrow(df), Exposed = sum(df$x == 1),
    Adjusted_change_11 = b, HC3_95_low = b - 1.96 * se, HC3_95_high = b + 1.96 * se,
    HC3_p = 2 * stats::pnorm(-abs(b / se)), Bootstrap_95_low = ci_boot[1],
    Bootstrap_95_high = ci_boot[2], LOO_min = min(leave, na.rm = TRUE),
    LOO_max = max(leave, na.rm = TRUE), bootstrap_valid = length(B), stringsAsFactors = FALSE)
}
ols <- do.call(rbind, ols)

# STEP 4 | Post-selection exploratory joint models. Not independent confirmation.
run_joint <- function(y, factors, only_cohort = FALSE, method = "binomial") {
  masks <- if (only_cohort) cohort else rep(TRUE, N)
  df <- base; df$y <- y
  for (f in factors) {
    spec <- predictors[predictors$factor == f, ]
    if (nrow(spec) != 1L) stop("Joint factor missing: ", f)
    df[[make.names(f)]] <- recode_binary(spec$column, spec$recode)
  }
  use <- masks & stats::complete.cases(df)
  term_names <- make.names(factors)
  formula <- stats::reformulate(c("year5", "log10nc", "ortho", term_names), response = "y")
  mod <- stats::glm(formula, data = df[use, , drop = FALSE],
                    family = if (method == "fractional") stats::quasibinomial(link = "logit") else stats::binomial(link = "logit"),
                    control = stats::glm.control(maxit = 200))
  r <- lapply(seq_along(term_names), function(i) {
    st <- walddetails(mod, term_names[i]); data.frame(Endpoint = if (only_cohort) "JBI fractional" else "STROBE high vs low",
       Factor = factors[i], n = sum(use), Adjusted_OR = st["est"], CI_low = st["low"], CI_high = st["high"],
       p = st["p"], stringsAsFactors = FALSE)
  })
  do.call(rbind, r)
}
joint <- rbind(run_joint(jbi_prop, c("Frailty assessed", "Advanced adjusted statistics", "Open access"),
                          only_cohort = TRUE, method = "fractional"),
               run_joint(strobe_high, c("Charlson index employed", "Advanced adjusted statistics",
                                       "Open access", "Cognitive status assessed")))

# STEP 5 | Export five analytic sheets corresponding to the reference workbook.
st_out <- data.frame(
 Domain = main$domain, Candidate_variable = main$variable, Source_column = main$source_name,
 N_valid = main$n_strobe, Exposed_N = main$n1_strobe, Unexposed_N = main$n0_strobe,
 High_exposed = main$strobe_high_exposed, Low_exposed = main$strobe_low_exposed,
 High_unexposed = main$strobe_high_unexposed, Low_unexposed = main$strobe_low_unexposed,
 Fisher_p = main$p_fisher, Fisher_q_BH_all = main$q_fisher,
 Adjusted_OR = main$or_strobe, OR_95_low = main$or_strobe_lci,
 OR_95_high = main$or_strobe_uci, Adjusted_p = main$p_adj_strobe,
 Adjusted_q_BH_primary = main$q_adj_strobe_primary,
 Status = main$strobe_eligibility, Caution = main$caution)
jbi_out <- data.frame(
 Domain = main$domain, Candidate_variable = main$variable, Source_column = main$source_name,
 N_cohort_valid = main$n_jbi, Exposed_N = main$n1_jbi, Unexposed_N = main$n0_jbi,
 JBI_mean_exposed_11 = main$mean_jbi_exposed, JBI_mean_unexposed_11 = main$mean_jbi_unexposed,
 Rank_p = main$p_jbi_mw, Rank_q_BH_all = main$q_jbi_mw,
 Adjusted_fractional_OR = main$frac_odds_jbi,
 Fractional_OR_95_low = main$frac_odds_jbi_lci,
 Fractional_OR_95_high = main$frac_odds_jbi_uci, Adjusted_p = main$p_adj_jbi,
 Adjusted_q_BH_primary = main$q_adj_jbi_primary,
 Status = main$jbi_eligibility, Caution = main$caution)
multi_out <- multi[, c("variable", "reference", "levels", "n128", "n55", "STROBE_counts",
                       "STROBE_p_global", "STROBE_terms", "JBI_counts", "JBI_p_global", "JBI_terms")]
readme <- data.frame(Topic = c("Study unit", "Source", "STROBE classification", "JBI outcome",
                                   "Modeling unit", "Covariate adjustment", "Multiplicity",
                                   "Sparse data", "Overlapping constructs", "Joint models", "Bootstrap RNG", "Interpretation"),
  Description = c("One publication, not patient", basename(input),
                  ">= 0.75 on original STROBE fractional score (81 high, 47 low)",
                  "Fractional JBI score /11, cohort studies only (n=55); JBI binary has zero high cases",
                  "One tested candidate in each primary adjusted regression, not all factors simultaneously",
                  "Publication year/5; centered log10(sample size); orthopaedic specialty except orthopaedic tested factor",
                  "BH separately within adjusted non-reporting predictors for each endpoint",
                  "Minimum cell eligibility screens are enforced; blank estimates are not zeros",
                  "Reporting-related factors may be mechanically linked to STROBE/JBI criteria",
                  "Post-selection joint models are exploratory and not confirmatory",
                  "R bootstrap uses R RNG and cannot exactly replicate prior Python bootstrap percentiles",
                  "Non-significant ≠ no association; results are observational, with small cohort n"))
outputs <- list(README = readme, STROBE_128 = st_out, JBI_cohorts_55 = jbi_out,
                Polytomous = multi_out, JBI_OLS_sensitivity = ols,
                Joint_exploratory = joint)
for (nm in names(outputs)) utils::write.csv(outputs[[nm]], file.path(outdir, paste0(nm, ".csv")), row.names = FALSE, na = "")
openxlsx::write.xlsx(outputs, file.path(outdir, "Resultados_R_STROBE_JBI.xlsx"), overwrite = TRUE)
utils::write.csv(main, file.path(outdir, "Auditoria_41_variables_completa.csv"), row.names = FALSE, na = "")

# Optional comparison with prior Python results. Differences are REPORTED, not
# silently overwritten. Interval and bootstrap differences can reflect R/Python
# implementation and random-number generator differences.
reference <- file.path(dirname(input), "Ampliacion_asociaciones_STROBE_JBI_V7_EXPLORATORIA.xlsx")
if (file.exists(reference)) {
  ref_st <- as.data.frame(readxl::read_excel(reference, sheet = "STROBE_128"))
  ref_jbi <- as.data.frame(readxl::read_excel(reference, sheet = "JBI_cohorts_55"))
  make_comparison <- function(endpoint, reference_table, r_table, ref_cols, r_cols, metrics) {
    if (!identical(as.character(reference_table[[2]]), as.character(r_table[[2]])))
      stop("Factor order has changed in reference comparison: ", endpoint)
    out <- vector("list", length(metrics))
    for (k in seq_along(metrics)) {
      a <- suppressWarnings(as.numeric(reference_table[[ref_cols[k]]]))
      b <- suppressWarnings(as.numeric(r_table[[r_cols[k]]]))
      d <- abs(a - b)
      relative <- d / pmax(abs(a), 1e-8)
      # ORs/intervals: 2% relative threshold; p and q: 0.01 absolute.
      ok <- if (metrics[k] %in% c("p", "q")) d <= .01 else relative <= .02
      out[[k]] <- data.frame(Endpoint = endpoint, Factor = as.character(r_table[[2]]),
        Statistic = metrics[k], Reference_Python = a, Recomputed_R = b,
        Absolute_difference = d, Relative_difference = relative,
        Review = ifelse(is.na(d), "Not estimated", ifelse(ok, "Within tolerance", "Review difference")),
        stringsAsFactors = FALSE)
    }
    do.call(rbind, out)
  }
  check <- rbind(
    make_comparison("STROBE", ref_st, st_out, 13:17, 13:17, c("estimate", "lower CI", "upper CI", "p", "q")),
    make_comparison("JBI", ref_jbi, jbi_out, 11:15, 11:15, c("estimate", "lower CI", "upper CI", "p", "q")))
  utils::write.csv(check, file.path(outdir, "Comparacion_R_vs_Python_5_estadisticos.csv"), row.names = FALSE, na = "")
  if (any(check$Review == "Review difference")) warning("Some R results differ from prior Python results. Inspect comparison CSV before manuscript use.")
}

numbers_audit <- data.frame(
  Indicator = c("V7 publications", "STROBE high", "STROBE low", "Cohort studies JBI",
                "Binary candidates", "STROBE adjusted candidate fits", "JBI adjusted candidate fits",
                "Polytomous candidates", "OLS sensitivities", "Joint-model coefficients"),
  Observed = c(N, sum(strobe_high == 1), sum(strobe_high == 0), sum(cohort),
               nrow(main), sum(is.finite(main$p_adj_strobe)), sum(is.finite(main$p_adj_jbi)),
               nrow(multi), nrow(ols), nrow(joint)),
  Reference = c(128, 81, 47, 55, 41, 34, 27, 6, 8, 7))
numbers_audit$Match <- numbers_audit$Observed == numbers_audit$Reference
utils::write.csv(numbers_audit, file.path(outdir, "QC_recuentos_analisis.csv"), row.names = FALSE)
utils::write.csv(predictors, file.path(outdir, "Diccionario_41_predictores_y_reglas.csv"), row.names = FALSE)
utils::capture.output(sessionInfo(), file = file.path(outdir, "R_sessionInfo.txt"))
if (any(!numbers_audit$Match)) warning("Some analytical count checks differ from the reference. Inspect QC_recuentos_analisis.csv")

stopifnot(nrow(main) == 41L, nrow(multi) == 6L, nrow(ols) == 8L, nrow(joint) == 7L)
message("Analysis completed. Output: ", normalizePath(outdir))
message("STROBE high = ", sum(strobe_high == 1), "; low = ", sum(strobe_high == 0),
        "; JBI cohort n = ", sum(cohort))
