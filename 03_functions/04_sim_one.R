# -------------------------------------------------------------------
# 04_sim_one.R  •  Ejecuta UNA réplica de la simulación
# Ubicación: project/03_functions/04_sim_one.R
# -------------------------------------------------------------------
# Proporciona la función pública:  sim_one()
# -------------------------------------------------------------------

source(here::here("03_functions/01_helpers.R"))
source(here::here("03_functions/02_generator.R"))
source(here::here("03_functions/03_estimators.R"))

# ---- 1. FUNCIÓN PRINCIPAL -----------------------------------------

#' sim_one
#'
#' Ejecuta una réplica de la simulación Monte‑Carlo.
#'
#' @param seed        Entero semilla para reproducibilidad.
#' @param n_total     Tamaño muestral total (parejo entre grupos).
#' @param Delta_theta Diferencia verdadera de medias latentes (SD).
#' @param Delta_nu    Magnitud del sesgo de intercepto en los ítems sesgados.
#' @param save_raw    Lógico. ¿Guardar los datos simulados en 01_data/raw?
#' @param engine      "manual" (predeterminado) o "lavaan" para generar datos.
#'
#' @return Un tibble con columnas de condiciones + métricas 
#'   (d_true, d_mean, d_fscore, d_mimic).
#' @export
sim_one <- function(seed, n_total, Delta_theta, Delta_nu,
                    save_raw = FALSE,
                    engine = c("manual", "lavaan")) {
  engine <- match.arg(engine)
  set.seed(seed)
  
  # --- Generar datos ------------------------------------------------
  data_df <- generate_data(n_total = n_total,
                           Delta_theta = Delta_theta,
                           Delta_nu = Delta_nu,
                           engine = engine)
  
  # --- Analizar datos ----------------------------------------------
  metrics_list <- analyse_data(data_df)
  
  # --- Resultado ----------------------------------------------------
  res <- tibble::tibble(seed = seed,
                        n_total = n_total,
                        Delta_theta = Delta_theta,
                        Delta_nu = Delta_nu,
                        !!!metrics_list)
  
  # --- Guardado opcional -------------------------------------------
  if (isTRUE(save_raw)) {
    file_tag <- sprintf("n%d_dt%.2f_dn%.2f_seed%09d.rds",
                        n_total, Delta_theta, Delta_nu, seed)
    fpath <- file.path(DIRS$raw, file_tag)
    saveRDS(list(data = data_df, metrics = res), fpath)
  }
  
  res
}

# ---- 2. TEST RÁPIDO -----------------------------------------------

if (interactive()) {
  log_info("Test rápido de sim_one() ...")
  out <- sim_one(seed = 2025, n_total = 400, Delta_theta = 0.5, Delta_nu = 0.2)
  print(out)
}
