# -------------------------------------------------------------------
# 01_summary.R  •  Limpia y resume los resultados de la simulación
# Ubicación: project/04_analysis/01_summary.R
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# ---- 1. CARGAR CONFIG Y DATOS --------------------------------------

config <- readRDS(here::here("config_sim.rds"))
DIRS   <- config$dirs
raw_rds <- file.path(DIRS$processed, "sim_results_raw.rds")

if (!file.exists(raw_rds)) stop("No se encontró sim_results_raw.rds. Corre 05_parallel.R primero.")

results <- readRDS(raw_rds)

# ---- 2. RESUMEN ----------------------------------------------------

summary_tbl <- results %>%
  group_by(n_total, Delta_theta, Delta_nu) %>%
  summarise(across(d_mean:d_mimic,
                   list(bias = ~ mean(. - d_true, na.rm = TRUE),
                        rmse = ~ sqrt(mean((. - d_true)^2, na.rm = TRUE))),
                   .names = "{col}_{fn}"),
            .groups = "drop")

# ---- 3. GUARDAR ----------------------------------------------------

tables_path <- DIRS$tables
if (!dir.exists(tables_path)) dir.create(tables_path, recursive = TRUE)

write_csv(summary_tbl, file.path(tables_path, "summary_results.csv"))
