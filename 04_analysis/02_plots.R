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
  pivot_longer(cols = matches("_bias_(mean|low|high)$"),
               names_to  = c("method", "stat"),
               names_pattern = "(d_[^_]+)_bias_(.*)",
               values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(method = factor(method,
                         levels = c("d_mean","d_fscore","d_mimic"),
                         labels = c("Mean score","Factor score","MIMIC")))

p_bias <- ggplot(bias_long,
                 aes(x = Delta_nu, y = mean,
                     colour = method, fill = method,
                     shape = method, group = method)) +
  geom_hline(yintercept = 0, colour = "red", linewidth = .3) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = .15, colour = NA) +
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_point(size = 3) +
  facet_grid(Delta_theta ~ n_total,
             labeller = labeller(
               Delta_theta = \(x) sprintf("Δθ = %s", x),
               n_total     = \(x) sprintf("N = %s", x))) +
  scale_colour_brewer(palette = "Dark2", name = "Método") +
  scale_fill_brewer(palette = "Dark2",  name = "Método") +
  scale_shape_manual(values = c(16, 17, 15), name = "Método") +
  labs(title = "Sesgo del estimador d (media e IC 95 %)",
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
  pivot_longer(cols = matches("_rmse_(mean|low|high)$"),
               names_to  = c("method", "stat"),
               names_pattern = "(d_[^_]+)_rmse_(.*)",
               values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(method = factor(method,
                         levels = c("d_mean","d_fscore","d_mimic"),
                         labels = c("Mean score","Factor score","MIMIC")))

p_rmse <- ggplot(rmse_long,
                 aes(x = Delta_nu, y = mean,
                     colour = method, fill = method,
                     shape = method, group = method)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = .15, colour = NA) +
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_point(size = 3) +
  facet_grid(Delta_theta ~ n_total,
             labeller = labeller(
               Delta_theta = \(x) sprintf("Δθ = %s", x),
               n_total     = \(x) sprintf("N = %s", x))) +
  scale_colour_brewer(palette = "Dark2", name = "Método") +
  scale_fill_brewer(palette = "Dark2",  name = "Método") +
  scale_shape_manual(values = c(16, 17, 15), name = "Método") +
  labs(title = "RMSE del estimador d (media e IC 95 %)",
       x = expression(Delta*nu), y = "RMSE") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.spacing.y  = unit(.8, "lines"),
        panel.spacing.x  = unit(.6, "lines"))

ggsave(file.path(plots_dir, "rmse_plot.png"), 
       p_rmse, width = 9, height = 8, dpi = 300,
       bg = "white")
