################################################################
## 06-plot-SI-fig6.R
## Plot connectivity indicators vs habitat amount and fragmentation
## (SI Fig. 6 style from Oehri et al. 2024), including MCI.
##
## Inputs:  scripts/parameters.R
##          results/binary/connectivity-with-mci.csv
## Outputs: results/binary/figures/SI-Fig6-reconnect.png
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(dplyr)
library(ggplot2)
library(patchwork)
library(npreg)

# ---- Settings ----------
# Set to a subset of dispersal distances to test, or NULL to plot all
FILTER_DISP_DIST <- NULL #c(1, 6.8, 10)

# ---- Load results ----------
results <- read.csv(MCI_CSV_PATH) %>%
  # Convert alpha_label to dispersal distance for plotting
  mutate(disp_dist = round(1 / alpha, 1))

# Filter to single dispersal distance if set
if (!is.null(FILTER_DISP_DIST)) {
  results <- results %>%
    filter(disp_dist %in% FILTER_DISP_DIST)
}

# ---- Data transformations ----------
# 1 cell = 1 m², 10000 cells = 1 ha
results_plot <- results %>%
  mutate(hab_area_ha = hab_area / 10000)

# ---- Indicators and labels ----------
indicators <- c("MPC", "ECA", "ECAAp", "mean_ND", "MCI_mean")

y_labs <- c(
  MPC      = "MPC",
  ECA      = "ECA",
  ECAAp    = "ECAAp",
  mean_ND  = "Average node degree",
  MCI_mean = "MCI (mean)"
)

x_labs <- c(
  hab_area_ha = "Total habitat area (ha)",
  n_patches   = "Number of patches"
)

x_vars <- c("hab_area_ha", "n_patches")

# ---- Colour palette ----------
# Keyed to dispersal distance
disp_dists_present <- sort(unique(results_plot$disp_dist))
palette_cols <- PALETTE_COLS[as.character(disp_dists_present)]

# ---- Smooth spline function ----------
fit_spline <- function(df, x_var, y_var) {
  sub <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  if (nrow(sub) < 5) return(NULL)

  fit    <- npreg::ss(sub[[x_var]], sub[[y_var]], df = 10)
  x_pred <- seq(min(sub[[x_var]]), max(sub[[x_var]]), length.out = 200)
  pred   <- predict(fit, x = x_pred)

  data.frame(x = x_pred, fit = pred$y, se = pred$se)
}

# ---- Panel function ----------
make_panel <- function(df, x_var, y_var, x_lab, y_lab) {
  
  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.2, size = 0.6, colour = "grey60") +
    theme_classic(base_size = 10) +
    theme(aspect.ratio = 2) +
    labs(x = x_lab, y = y_lab)
  
  # Clip axes without removing data
  if (x_var == "n_patches" && y_var == "mean_ND") {
    p <- p + coord_cartesian(xlim = c(0, 1200), ylim = c(0, 1200))
  } else if (x_var == "n_patches") {
    p <- p + coord_cartesian(xlim = c(0, 1200))
  } else if (x_var == "hab_area_ha" && y_var == "mean_ND") {
    p <- p + coord_cartesian(xlim = c(0, 6), ylim = c(0, 1200))
  } else if (x_var == "hab_area_ha") {
    p <- p + coord_cartesian(xlim = c(0, 6))
  }
  
  # Add one spline per dispersal distance
  for (dd in disp_dists_present) {
    sub_df    <- df[df$disp_dist == dd, ]
    spline_df <- fit_spline(sub_df, x_var, y_var)
    if (!is.null(spline_df)) {
      spline_df$disp_dist <- as.character(dd)
      p <- p +
        geom_ribbon(
          data        = spline_df,
          aes(x = x, ymin = fit - se, ymax = fit + se),
          inherit.aes = FALSE,
          fill        = palette_cols[as.character(dd)],
          alpha       = 0.15
        ) +
        geom_line(
          data        = spline_df,
          aes(x = x, y = fit, colour = disp_dist),
          inherit.aes = FALSE,
          linewidth   = 0.9
        )
    }
  }
  
# Add legend — always show all dispersal distances for consistency
  p <- p +
    scale_colour_manual(
      values = PALETTE_COLS,
      name   = "Dispersal\ndistance (cells)",
      breaks = as.character(sort(DISPERSAL_DISTANCES))
    )  
  p
}

# ---- Build panels ----------
panels <- list()

for (yv in indicators) {
  for (xv in x_vars) {
    panels[[paste(yv, xv, sep = "_")]] <- make_panel(
      df    = results_plot,
      x_var = xv,
      y_var = yv,
      x_lab = x_labs[xv],
      y_lab = y_labs[yv]
    )
  }
}

# ---- Assemble figure ----------
fig_si6 <- (
  panels[["MPC_hab_area_ha"]]      + panels[["ECA_hab_area_ha"]]      +
    panels[["ECAAp_hab_area_ha"]]    + panels[["mean_ND_hab_area_ha"]]  +
    panels[["MCI_mean_hab_area_ha"]] +
    panels[["MPC_n_patches"]]        + panels[["ECA_n_patches"]]        +
    panels[["ECAAp_n_patches"]]      + panels[["mean_ND_n_patches"]]    +
    panels[["MCI_mean_n_patches"]]
) +
  patchwork::plot_layout(ncol = 5, guides = "collect") +
  patchwork::plot_annotation(
    title    = "Oehri et al. 2024: Connectivity indicators vs habitat amount and fragmentation",
    subtitle = sprintf(
      "%d habitat amount levels \u00d7 %d clumping levels \u00d7 %d reps | dispersal distance = %s cells",
      length(HAB_AMOUNTS), length(CLUMPING_VALS), N_REP,
      paste(sort(unique(results_plot$disp_dist)), collapse = ", ")
    ),
    theme = theme(legend.position = "right")
  )

# ---- Save ----------
ggsave(FIG_PATH, fig_si6, width = 20, height = 10, dpi = 300)
