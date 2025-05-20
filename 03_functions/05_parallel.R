# -------------------------------------------------------------------
# 05_parallel.R  •  Lanza todas las réplicas en paralelo y resume
# Ubicación: project/03_functions/05_parallel.R
# -------------------------------------------------------------------

source(here::here("03_functions/01_helpers.R"))
source(here::here("03_functions/04_sim_one.R"))

suppressPackageStartupMessages({
  library(future.apply)
  library(progressr)      # barra de progreso compatible con future
  library(tidyverse)
})

# ---- 1. CONFIGURAR PLAN DE FUTURE ---------------------------------

plan(multisession, workers = config$n_cores)
log_info(sprintf("Usando %d núcleos", config$n_cores))

# ---- 2. EXPANDIR GRID × RÉPLICAS ----------------------------------

grid <- config$design_grid
big_grid <- purrr::map_dfr(seq_len(config$n_reps), ~ grid) %>%
  dplyr::mutate(rep = rep(seq_len(config$n_reps), each = nrow(grid)))

# --- Semilla única por fila (estrategia B) --------------------------
set.seed(42)  # reproducibilidad del vector de semillas
big_grid$seed <- sample.int(.Machine$integer.max, nrow(big_grid))

n_jobs <- nrow(big_grid)
log_info(sprintf("Total de réplicas a ejecutar: %d", n_jobs))

# ---- 3. BARRA DE PROGRESO + EJECUCIÓN ------------------------------

handlers("progress")

results <- with_progress({
  p <- progressor(steps = n_jobs)
  future_lapply(seq_len(n_jobs), 
                future.seed=TRUE,
                function(i) {
    row <- big_grid[i, ]
    res <- sim_one(seed = row$seed,
                   n_total = row$n_total,
                   Delta_theta = row$Delta_theta,
                   Delta_nu = row$Delta_nu)
    # Mensaje con la condición actual
    cond_msg <- sprintf("n=%d, Δθ=%.1f, Δν=%.1f", 
                        row$n_total, row$Delta_theta, 
                        row$Delta_nu)
    p(message = cond_msg)
    res
  })
}) %>% dplyr::bind_rows()

# ---- 4. GUARDAR RESULTADOS CRUDOS ---------------------------------

saveRDS(results, file = file.path(DIRS$processed, "sim_results_raw.rds"))

log_info("Simulación completada y guardada.")
