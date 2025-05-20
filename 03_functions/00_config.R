# -------------------------------------------------------------------
# 00_config.R  •  Constantes y rutas del estudio de simulación
# Ubicación: /03_functions/00_config.R
# -------------------------------------------------------------------

# === 1. PARÁMETROS DEL DISEÑO =======================================

n_items        <- 10                          # número de ítems totales
biased_items   <- c(3, 6, 9)                  # ítems con sesgo (30 %)

# Escalas de magnitud (SD de la latente)
Delta_theta_levels <- c(0, 0.2, 0.5, 0.8)     # diferencia de medias latentes
Delta_nu_levels    <- c(0, 0.2, 0.5, 0.8)     # sesgo intercepto en 3 ítems

n_total_levels <- c(400, 800)                 # muestra total (50 %/grupo)

n_reps <- 500                                  # réplicas Monte-Carlo


# === 2. GESTIÓN DE DIRECTORIOS ======================================

suppressPackageStartupMessages(library(here))
base_dir <- here::here()                       # directorio raíz del proyecto

# Estructura definida por el usuario
raw_dir      <- file.path(base_dir, "01_data", "raw")
proc_dir     <- file.path(base_dir, "01_data", "processed")
plots_dir    <- file.path(base_dir, "02_output", "plots")
tables_dir   <- file.path(base_dir, "02_output", "tables")
reports_dir  <- file.path(base_dir, "02_output", "reports")

# === 3. GRADE DESIGN GRID ===========================================

design_grid <- expand.grid(n_total     = n_total_levels,
                           Delta_theta = Delta_theta_levels,
                           Delta_nu    = Delta_nu_levels,
                           KEEP.OUT.ATTRS = FALSE)


# === 4. RECURSOS DE CPU =============================================

n_cores <- max(1, parallel::detectCores())


# === 5. OBJETO CONFIG UNIFICADO =====================================

config <- list(
  # dimensiones y diseño
  n_items      = n_items,
  biased_items = biased_items,
  Delta_theta_levels = Delta_theta_levels,
  Delta_nu_levels    = Delta_nu_levels,
  n_total_levels     = n_total_levels,
  n_reps       = n_reps,
  design_grid  = design_grid,
  # paths
  dirs = list(raw = raw_dir,
              processed = proc_dir,
              plots = plots_dir,
              tables = tables_dir,
              reports = reports_dir),
  # recursos
  n_cores      = n_cores
)

saveRDS(config, file = file.path(base_dir, "config_sim.rds"))

rm(base_dir)
