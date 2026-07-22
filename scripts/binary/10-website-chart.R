################################################################
## 11-website-chart.R
## Editorial-style chart for the website: ECAAp vs number of
## protected areas, one smoothed curve per dispersal distance.
## Styled with soft, earthy tones to match the site rather than
## a typical scientific figure.
##
## Inputs:  scripts/parameters.R
##          results/binary/connectivity-reference-indicators.csv
## Outputs: results/binary/figures/website-chart.png
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(dplyr)
library(ggplot2)
library(npreg)

# ---- Settings ----------
FILTER_HAB      <- 0.2                 # matches the map figures
FILTER_CLUMPING <- CLUMPING_VALS       # all clumping levels -> more points
DISP_KEEP       <- c(1, 10, 50, 150, 355)

BG_COL <- "transparent"

EARTHY_COLS <- setNames(
  c("#1F4A32", "#3F9159", "#C9702E", "#1E6E8C", "#16456B"),
  as.character(DISP_KEEP)
)

disp_labels <- setNames(as.character(DISP_KEEP), as.character(DISP_KEEP))
disp_labels["355"] <- "300"

palette_named <- setNames(EARTHY_COLS, disp_labels[names(EARTHY_COLS)])

# ---- Load and filter data ----------
results <- read.csv(REFERENCE_CSV_PATH) %>%
  mutate(disp_dist = round(1 / alpha, 1)) %>%
  filter(
    is.element(disp_dist, DISP_KEEP),
    hab_amount == FILTER_HAB,
    is.element(clumping, FILTER_CLUMPING)
  ) %>%
  mutate(
    disp_dist_lab = factor(
      disp_labels[as.character(disp_dist)],
      levels = disp_labels[as.character(sort(DISP_KEEP))]
    )
  )

# ---- Fit a smooth curve (with SE) per dispersal distance ----------
fit_spline <- function(df) {
  sub <- df[!is.na(df$n_patches) & !is.na(df$ECAAp), ]
  if (nrow(sub) < 5) return(NULL)
  fit    <- npreg::ss(sub$n_patches, sub$ECAAp, df = 5)
  x_pred <- seq(min(sub$n_patches), max(sub$n_patches), length.out = 200)
  pred   <- predict(fit, x = x_pred, se.fit = TRUE)
  se     <- if (!is.null(pred$se.fit)) pred$se.fit else if (!is.null(pred$se)) pred$se else NA_real_
  data.frame(x = x_pred, y = pred$y, se = se)
}

curve_list <- lapply(split(results, results$disp_dist_lab), fit_spline)
curves <- dplyr::bind_rows(curve_list, .id = "disp_dist_lab")
curves$disp_dist_lab <- factor(curves$disp_dist_lab, levels = levels(results$disp_dist_lab))
curves$ymin <- pmax(curves$y - 1.96 * curves$se, 0)
curves$ymax <- pmin(curves$y + 1.96 * curves$se, 1)

# ---- End-of-line labels (replace legend) ----------
label_df <- curves %>%
  group_by(disp_dist_lab) %>%
  filter(x == max(x)) %>%
  ungroup()

# ---- Chart ----------
p_chart <- ggplot(curves, aes(x = x, y = y, colour = disp_dist_lab)) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax, fill = disp_dist_lab),
    colour = NA, alpha = 0.15
  ) +
  geom_line(linewidth = 1.6, lineend = "round") +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = disp_dist_lab, colour = disp_dist_lab),
    hjust = -0.15, size = 4.2, fontface = "bold", family = "sans"
  ) +
  scale_colour_manual(values = palette_named, guide = "none") +
  scale_fill_manual(values = palette_named, guide = "none") +
  scale_x_continuous(limits = c(NA, 1500), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0.03)) +
  labs(x = "Increasing fragmentation", y = "Increasing connectivity") +
  theme_minimal(base_size = 15) +
  theme(
    plot.background   = element_rect(fill = BG_COL, colour = NA),
    panel.background  = element_rect(fill = BG_COL, colour = NA),
    panel.grid        = element_blank(),
    axis.line         = element_line(colour = "#8a8378", linewidth = 0.4),
    axis.ticks        = element_blank(),
    axis.text         = element_blank(),
    axis.title        = element_text(colour = "#6b6459", size = 13),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    plot.margin       = margin(20, 40, 20, 20)
  )

# ---- Save ----------
out_path <- file.path(FIGURES_DIR, "website-chart.png")
ggsave(out_path, p_chart, width = 8.5, height = 6, dpi = 300, bg = "transparent")