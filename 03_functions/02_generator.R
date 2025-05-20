# -------------------------------------------------------------------
# 02_generator.R  •  Creación de datos poblacionales y simulados
# Ubicación: project/03_functions/02_generator.R
# -------------------------------------------------------------------

# Este módulo provee una única función pública:
#   generate_data()
# que regresa un data.frame con las variables simuladas para una réplica.

source(here::here("03_functions/01_helpers.R"))

# ---- 1. FUNCIÓN AUXILIAR: RESIDUAL VARIANCE ------------------------

compute_resid_var <- function(lambda_vec) {
  1 - lambda_vec^2
}

# ---- 2. GENERADOR MANUAL (más rápido y flexible) -------------------

manual_generator <- function(n_total, Delta_theta, Delta_nu, lambda_vec,
                             bias_idx) {
  n_per_group <- n_total / 2
  stopifnot(n_per_group %% 1 == 0)
  
  # -- Paso 1: generar latentes θ ------------------------------------
  theta <- c(rnorm(n_per_group, 0, 1),
             rnorm(n_per_group, Delta_theta, 1))
  grp   <- rep(c(0, 1), each = n_per_group)
  
  # -- Paso 2: interceptos τ ----------------------------------------
  tau_base <- numeric(config$n_items)
  tau_g1   <- tau_base
  tau_g1[bias_idx] <- tau_g1[bias_idx] + Delta_nu
  
  # -- Paso 3: varianza residual ------------------------------------
  resid_var <- compute_resid_var(lambda_vec)
  
  # -- Paso 4: ensamblar matriz de ítems -----------------------------
  X <- matrix(NA_real_, n_total, config$n_items)
  for (j in seq_len(config$n_items)) {
    current_tau <- ifelse(grp == 1, tau_g1[j], tau_base[j])
    X[, j] <- current_tau + lambda_vec[j] * theta +
      rnorm(n_total, 0, sqrt(resid_var[j]))
  }
  colnames(X) <- paste0("Item", seq_len(config$n_items))
  
  tibble::tibble(grp = factor(grp), theta_true = theta) |>
    dplyr::bind_cols(as.data.frame(X))
}

# ---- 3. GENERADOR CON lavaan::simulateData --------------

lavaan_generator <- function(n_total, Delta_theta, Delta_nu, lambda_vec,
                             bias_idx) {
  n_per_group <- n_total / 2
  
  # *Modelo poblacional*
  model_lines <- c(
    paste0("F =~ ", paste(paste0(lambda_vec, "*Item", seq_len(config$n_items)), collapse = " + ")),
    # interceptos base = 0, luego añadimos sesgo a los 3 ítems
    paste0("Item", bias_idx, " ~ start(0) + ", Delta_nu, "*1"),
    paste0("Item", setdiff(seq_len(config$n_items), bias_idx), " ~ 0*1"),
    "F ~~ 1*F"  # varianza latente = 1
  )
  pop_model <- paste(model_lines, collapse = "\n")
  
  dat <- lavaan::simulateData(model = pop_model,
                              model.type = "cfa",
                              meanstructure = TRUE,
                              group.label = c("0", "1"),
                              sample.nobs = c(n_per_group, n_per_group),
                              group.w.free = FALSE,
                              std.lv = TRUE) |>
    as.data.frame()
  
  # simulateData no permite medias latentes diferentes fácilmente;
  # ajustamos manualmente para el grupo 1
  idx_g1 <- seq_len(n_per_group) + n_per_group
  dat$F[idx_g1] <- dat$F[idx_g1] + Delta_theta
  
  dat$grp <- factor(c(rep(0, n_per_group), rep(1, n_per_group)))
  dat$theta_true <- dat$F
  dat$F <- NULL
  
  dat[c("grp", paste0("Item", seq_len(config$n_items)), "theta_true")]
}

# ---- 4. Función generadora ------------------------------------------

#' generate_data
#'
#' @param n_total     Tamaño muestral total (parejo entre grupos)
#' @param Delta_theta Diferencia de medias latentes (SD units)
#' @param Delta_nu    Sesgo de intercepto aplicado a los ítems sesgados
#' @param lambda_vec  Vector de cargas; se genera con make_lambda() si NULL
#' @param bias_idx    Índices de ítems sesgados (default config$biased_items)
#' @param engine      "manual" (default) o "lavaan" para usar simulateData
#'
#' @return data.frame con: grp (factor 0/1), Item1:Item10, theta_true
#' @export
generate_data <- function(n_total, Delta_theta, Delta_nu,
                          lambda_vec = NULL,
                          bias_idx   = config$biased_items,
                          engine     = c("manual", "lavaan")) {
  engine <- match.arg(engine)
  if (is.null(lambda_vec)) lambda_vec <- make_lambda()
  
  switch(engine,
         manual = manual_generator(n_total, Delta_theta, 
                                   Delta_nu, lambda_vec, bias_idx),
         lavaan = lavaan_generator(n_total, Delta_theta,
                                   Delta_nu, lambda_vec, bias_idx))
}

# ---- 5. PRUEBA RÁPIDA ---------------

if (interactive()) {
  log_info("Test rápido de generate_data...")
  test_df <- generate_data(400, 0.5, 0.2)
  print(head(test_df))
  log_info("OK!")
}
