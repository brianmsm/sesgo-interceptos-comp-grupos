# Este módulo proporciona funciones para obtener:
#   • Sum score estandarizado
#   • Factor scores vía CFA multigrupo
#   • Pendiente grupo→factor del modelo MIMIC
# Y calcula los correspondientes Cohen d.
# -------------------------------------------------------------------
# 03_estimators.R  •  Ajustes de modelos y métricas derivadas
# Ubicación: project/03_functions/03_estimators.R
# -------------------------------------------------------------------

source(here::here("03_functions/01_helpers.R"))
source(here::here("03_functions/02_generator.R")) 

# ---- 1. MEAN SCORE -------------------------------------------------

mean_score <- function(items_mat) {
  rowMeans(items_mat)
}

# ---- 2. FACTOR SCORES (CFA) -----------------------------

fit_cfa_simple <- function(data_df) {
  model <- paste0("F =~ ", paste(paste0("Item", 1:config$n_items), 
                                 collapse = " + "))
  
  safe_cfa(model, data = data_df)
}

get_factor_scores <- function(fit, n_obs) {
  # Devuelve vector numérico con length == n_obs (orden original de casos)
  if (is.null(fit)) return(rep(NA_real_, n_obs))
  
  fs <- lavPredict(fit, method = "Bartlett")  
  # lavaan devuelve lista por grupo si group.arg=NULL y hay grupos
  if (is.list(fs)) {
    fs <- do.call(rbind, fs)  # concatena 
  }
  if (is.matrix(fs)) fs <- fs[, 1]
  fs <- as.numeric(fs)
  
  # Chequeo defensivo
  if (length(fs) != n_obs) {
    warning("Factor scores length (", length(fs),
            ") != n_obs (", n_obs, ") – se rellenará con NA")
    fs <- rep(NA_real_, n_obs)
  }
  fs
}

# ---- 3. MODELO MIMIC ---------------------------------------------- MODELO MIMIC ----------------------------------------------

fit_mimic <- function(data_df) {
  model <- paste0("F =~ ", paste(paste0("Item", 1:config$n_items), 
                                 collapse = " + "),
                  "\nF ~ grp")
  safe_sem(model, data = data_df)
}

get_mimic_d <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  parameterEstimates(fit) %>%
    dplyr::filter(lhs == "F", op == "~", rhs == "grp") %>%
    dplyr::pull(est) %>%
    dplyr::first()
}

# ---- 4. MÉTRICAS DE COMPARABILIDAD -------------------------------

compute_metrics <- function(data_df, f_scores, mimic_d) {
  items_mat <- as.matrix(data_df[ , grep("^Item", names(data_df))])
  
  d_mean <- cohen_d(z_score(mean_score(items_mat)), data_df$grp)
  d_fscore <- if (all(is.na(f_scores))) NA_real_ else cohen_d(f_scores,
                                                              data_df$grp)
  
  list(d_mean = d_mean,
       d_fscore = d_fscore,
       d_mimic = mimic_d)
}

# ---- 5. FUNCIÓN DE ALTO NIVEL: analyse_data -----------------------

#' analyse_data
#'
#' @param data_df  data.frame generado por generate_data()
#'
#' @return list con: d_true y los d estimados (mean, fscore, mimic)
#' @export
analyse_data <- function(data_df) {
  n_obs <- nrow(data_df)
  
  # d verdadero a partir de theta_true
  d_true <- cohen_d(data_df$theta_true, data_df$grp)
  
  # CFA & factor scores
  fit_cfa <- fit_cfa_simple(data_df)
  f_scores <- get_factor_scores(fit_cfa, n_obs)
  
  # MIMIC
  fit_mim <- fit_mimic(data_df)
  d_mimic <- get_mimic_d(fit_mim)
  
  # Métricas
  metrics <- compute_metrics(data_df, f_scores, d_mimic)
  
  c(list(d_true = d_true), metrics)
}

# ---- 6. TEST INTERACTIVO ------------------------------------------

if (interactive()) {
  log_info("Test rápido de analyse_data() ...")
  tmp <- generate_data(500, 0.5, 0.2)
  print(analyse_data(tmp))
}
