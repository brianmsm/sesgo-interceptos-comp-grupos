# -------------------------------------------------------------------
# 02_plots.R  •  Visualiza el sesgo y RMSE por condición
# Ubicación: project/04_analysis/02_plots.R
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(ggplot2)
})

# ---- 1. CARGAR DATOS RESUMIDOS -------------------------------------

config <- readRDS(here::here("config_sim.rds"))
DIRS   <- config$dirs
summary_csv <- file.path(DIRS$tables, "summary_results.csv")

if (!file.exists(summary_csv)) stop("Ejecuta 01_summary.R primero para crear summary_results.csv")

summary_tbl <- read_csv(summary_csv, show_col_types = FALSE)

# ---- 2. PLOT: Sesgo medio -----------------------------------------

bias_long <- summary_tbl %>%
  pivot_longer(cols = ends_with("_bias"),
               names_to = "metric", values_to = "bias") %>%
  mutate(method = str_remove(metric, "_bias$"))

p_bias <-
  ggplot(bias_long,
         aes(x = Delta_nu, y = bias,
             colour = method, shape = method)) +
  geom_hline(yintercept = 0, linewidth = .3, colour = "red") +
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_point(size = 3) +
  facet_grid(Delta_theta ~ n_total,
             labeller = labeller(
               Delta_theta = function(x) sprintf("Δθ = %s", x),
               n_total     = function(x) sprintf("N = %s", x)
             )) +
  scale_colour_brewer(palette = "Dark2", name = "Método") +
  scale_shape_manual(values = c(16, 17, 15), name = "Método") +
  labs(title = "Sesgo del estimador d según magnitud del sesgo de intercepto",
       x = expression(Delta*nu),
       y = expression("Sesgo ("*hat(d) - d[verdadero]*")")) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.spacing.y  = unit(.8, "lines"),
        panel.spacing.x  = unit(.6, "lines"))

plots_dir <- DIRS$plots
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

ggsave(file.path(plots_dir, "bias_plot.png"),
       p_bias, width = 9, height = 8, dpi = 300,
       bg = "white")

# ---- 3. PLOT: RMSE -------------------------------------------------

rmse_long <- summary_tbl %>% 
  pivot_longer(cols = ends_with("_rmse"),
               names_to  = "metric",
               values_to = "rmse") %>% 
  mutate(method = str_remove(metric, "_rmse$"),
         method = factor(method,
                         levels = c("d_mean", "d_fscore", "d_mimic"),
                         labels = c("Mean score", "Factor score", "MIMIC")),
         Delta_theta = factor(Delta_theta,
                              levels = sort(unique(Delta_theta)))
  )

p_rmse <- ggplot(rmse_long,
                 aes(x = Delta_nu, y = rmse,
                     colour = method, shape = method)) +
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_point(size = 3) +
  facet_grid(Delta_theta ~ n_total,
             labeller = labeller(
               Delta_theta = function(x) sprintf("Δθ = %s", x),
               n_total     = function(x) sprintf("N = %s", x)
             )) +
  scale_colour_brewer(palette = "Dark2", name = "Método") +
  scale_shape_manual(values = c(16, 17, 15), name = "Método") +
  labs(title = "RMSE del estimador d según magnitud del sesgo de intercepto",
       x = expression(Delta*nu),
       y = "RMSE") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.spacing.y  = unit(.8, "lines"),
        panel.spacing.x  = unit(.6, "lines"))

ggsave(file.path(plots_dir, "rmse_plot.png"), 
       p_rmse, width = 9, height = 8, dpi = 300,
       bg = "white")
