################################################################
## 10-website-chart.R
## Editorial-style chart for my website: ECAAp vs number of
## protected areas, one smoothed curve per dispersal distance.
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

BG_COL <- "#f7f5f0"

# Soft earthy palette, ordered short -> long dispersal distance
EARTHY_COLS <- setNames(
  c("#3D5A50", "#7A9E7E", "#C9B896", "#8CA3B8", "#5C7A96"),
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

# ---- Fit a smooth curve per dispersal distance ----------
fit_spline <- function(df) {
  sub <- df[!is.na(df$n_patches) & !is.na(df$ECAAp), ]
  if (nrow(sub) < 5) return(NULL)
  fit    <- npreg::ss(sub$n_patches, sub$ECAAp, df = 5)
  x_pred <- seq(min(sub$n_patches), max(sub$n_patches), length.out = 200)
  pred   <- predict(fit, x = x_pred)
  data.frame(x = x_pred, y = pred$y)
}

curve_list <- lapply(split(results, results$disp_dist_lab), fit_spline)
curves <- dplyr::bind_rows(curve_list, .id = "disp_dist_lab")
curves$disp_dist_lab <- factor(curves$disp_dist_lab, levels = levels(results$disp_dist_lab))

# ---- End-of-line labels (replace legend) ----------
label_df <- curves %>%
  group_by(disp_dist_lab) %>%
  filter(x == max(x)) %>%
  ungroup()

# ---- Chart ----------
p_chart <- ggplot(curves, aes(x = x, y = y, colour = disp_dist_lab)) +
  geom_line(linewidth = 1.6, lineend = "round") +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = disp_dist_lab, colour = disp_dist_lab),
    hjust = -0.15, size = 4.2, fontface = "bold", family = "sans"
  ) +
  scale_colour_manual(values = palette_named, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.16))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0.03)) +
  labs(x = "Increasing fragmentation", y = "Connectivity") +
  theme_minimal(base_size = 15) +
  theme(
    plot.background   = element_rect(fill = BG_COL, colour = NA),
    panel.background  = element_rect(fill = BG_COL, colour = NA),
    panel.grid        = element_blank(),
    axis.line         = element_line(colour = "#8a8378", linewidth = 0.4),
    axis.ticks        = element_blank(),
    axis.text         = element_text(colour = "#8a8378", size = 11),
    axis.title        = element_text(colour = "#6b6459", size = 13),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    plot.margin       = margin(20, 40, 20, 20)
  )

# ---- Save ----------
out_path <- file.path(FIGURES_DIR, "website-chart.png")
ggsave(out_path, p_chart, width = 8.5, height = 6, dpi = 300, bg = BG_COL)

cat(sprintf("Saved %s\n", out_path))