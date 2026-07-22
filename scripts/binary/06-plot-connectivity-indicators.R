################################################################
## 06-plot-connectivity-indicators.R
## Plot connectivity indicators vs habitat amount and fragmentation
## (SI Fig. 6 style from Oehri et al. 2024), including MCI.
##
## Inputs:  scripts/parameters.R
##          results/binary/connectivity-with-mci.csv
## Outputs: results/binary/figures/connectivity-indicators.png
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(dplyr)
library(ggplot2)
library(patchwork)
library(npreg)

# ---- Settings ----------
# Set to NULL to include all, or a vector to subset
FILTER_DISP_DIST <- NULL
FILTER_HAB       <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)   # subset of HAB_AMOUNTS
FILTER_CLUMPING  <- c(0.1, 0.2, 0.3, 0.4, 0.5)   # exclude clumping = 0.6

# ---- Load results ----------
results <- read.csv(MCI_CSV_PATH) %>%
  mutate(disp_dist = round(1 / alpha, 1))

# ---- Apply filters ----------
if (!is.null(FILTER_DISP_DIST)) {
  results <- results %>% filter(disp_dist %in% FILTER_DISP_DIST)
}
if (!is.null(FILTER_HAB)) {
  results <- results %>% filter(hab_amount %in% FILTER_HAB)
}
if (!is.null(FILTER_CLUMPING)) {
  results <- results %>% filter(clumping %in% FILTER_CLUMPING)
}

# ---- Data transformations ----------
# True habitat % from actual cell counts (not target hab_amount)
results_plot <- results %>%
  mutate(
    hab_pct      = (hab_area / (NCOL * NROW)) * 100
  )

# ---- Indicators and labels ----------
indicators <- c("MPC", "ECA", "ECAAp", "mean_ND", "MCI_mean")

y_labs <- c(
  MPC          = "MPC",
  ECA          = "ECA",
  ECAAp        = "ECAAp",
  mean_ND      = "Average node degree",
  MCI_mean     = "MCI (mean)"
)

x_labs <- c(
  hab_pct   = "Habitat cover (%)",
  n_patches = "Number of patches"
)

x_vars <- c("hab_pct", "n_patches")

# ---- Colour palette ----------
disp_dists_present <- sort(unique(results_plot$disp_dist))
palette_cols       <- PALETTE_COLS[as.character(disp_dists_present)]

# ---- Smooth spline function ----------
fit_spline <- function(df, x_var, y_var) {
  sub <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  if (nrow(sub) < 5) return(NULL)
  
  fit    <- npreg::ss(sub[[x_var]], sub[[y_var]], df = 5)
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
  } else if (x_var == "hab_pct" && y_var == "mean_ND") {
    p <- p + coord_cartesian(xlim = c(0, 75), ylim = c(0, 1200))
  } else if (x_var == "hab_pct") {
    p <- p + coord_cartesian(xlim = c(0, 75))
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
  
  # Add legend
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
  panels[["MPC_hab_pct"]]           + panels[["ECA_hab_pct"]]           +
    panels[["ECAAp_hab_pct"]]         + panels[["mean_ND_hab_pct"]]       +
    panels[["MCI_mean_hab_pct"]]  +
    panels[["MPC_n_patches"]]         + panels[["ECA_n_patches"]]         +
    panels[["ECAAp_n_patches"]]       + panels[["mean_ND_n_patches"]]     +
    panels[["MCI_mean_n_patches"]]
) +
  patchwork::plot_layout(ncol = 5, guides = "collect") +
  patchwork::plot_annotation(
    title    = "Oehri et al. 2024: Connectivity indicators vs habitat amount and fragmentation",
    subtitle = sprintf(
      "%d habitat amount levels \u00d7 %d clumping levels \u00d7 %d reps | dispersal distance = %s cells",
      length(FILTER_HAB), length(FILTER_CLUMPING), N_REP,
      paste(sort(unique(results_plot$disp_dist)), collapse = ", ")
    ),
    theme = theme(legend.position = "right")
  )

# ---- Save ----------
ggsave(FIG_PATH, fig_si6, width = 20, height = 10, dpi = 300)
