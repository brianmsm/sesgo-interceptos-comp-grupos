# -------------------------------------------------------------------
# 01_helpers.R  •  Funciones genéricas reutilizadas en la simulación
# Ubicación: project/03_functions/01_helpers.R
# -------------------------------------------------------------------

# ---- 1. CARGAR CONFIGURACIÓN ---------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
})

config <- readRDS(here::here("config_sim.rds"))

# Atajo para rutas (por legibilidad):
DIRS <- config$dirs

# ---- 2. GENERADORES AUXILIARES ------------------------------------

# 2.1  make_lambda(): vector de cargas λ ~ U(0.60, 0.80)
make_lambda <- function(n = config$n_items,
                        min_lambda = 0.60,
                        max_lambda = 0.80) {
  runif(n, min_lambda, max_lambda)
}

# 2.2  cohen_d(): diferencia estandarizada entre dos grupos (0/1)
cohen_d <- function(x, g) {
  if (!is.numeric(x)) x <- as.numeric(x)
  g <- as.integer(as.character(g))
  if (!all(sort(unique(g)) == c(0, 1)))
    stop("El vector de grupos debe contener exactamente 0 y 1")
  x1 <- x[g == 0]
  x2 <- x[g == 1]
  s_pooled <- sd(c(x1, x2))
  (mean(x2) - mean(x1)) / s_pooled
}

# 2.3  z_score(): estandariza un vector (media 0, sd 1)
z_score <- function(v) {
  (v - mean(v)) / sd(v)
}

# ---- 3. WRAPPERS PARA AJUSTES LAVAAN -------------------------------

# 3.1  safe_cfa(): CFA multigrupo con control de warnings/errores
safe_cfa <- function(model, data, ...) {
  tryCatch(
    cfa(model, data = data, warn = FALSE, std.lv = TRUE, ...),
    error   = function(e) NULL,
    warning = function(w) invokeRestart("muffleWarning")
  )
}

# 3.2  safe_sem(): para MIMIC
safe_sem <- function(model, data, ...) {
  tryCatch(
    sem(model, data = data, warn = FALSE, std.lv = TRUE, ...),
    error   = function(e) NULL,
    warning = function(w) invokeRestart("muffleWarning")
  )
}

# ---- 4. LOGGING / MENSAJES ----------------------------------------

log_info <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
}
