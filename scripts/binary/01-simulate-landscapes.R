################################################################
## 01-simulate-landscapes.R
## Simulate binary landscapes using the random-cluster algorithm
## (Saura & Martínez-Millán 2000) via the NLMR R package.
##
## Inputs:  scripts/parameters.R
## Outputs: landscapes/binary/simulated-landscapes-tif/*.tif
##          (one GeoTIFF per landscape, 540 total)
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(NLMR)
library(terra)
library(ggplot2)

# ---- Simulate landscapes ----------
total_landscapes <- length(HAB_AMOUNTS) * length(CLUMPING_VALS) * N_REP
counter          <- 0

cat(sprintf("Simulating %d landscapes (%d hab x %d clump x %d reps)...\n",
            total_landscapes,
            length(HAB_AMOUNTS), length(CLUMPING_VALS), N_REP))

gen_time <- system.time({

  for (Ai in HAB_AMOUNTS) {
    for (p in CLUMPING_VALS) {
      for (rep in seq_len(N_REP)) {

        counter  <- counter + 1
        if (counter %% 10 == 0) {
          cat(sprintf("  %d / %d\n", counter, total_landscapes))
        }

        key      <- sprintf("clump%.1f_hab%.1f_rep%d", p, Ai, rep)
        out_path <- file.path(NLMR_DIR, paste0(key, ".tif"))

        # Simulate landscape using random-cluster algorithm
        # NLMR returns 1 (non-habitat) and 2 (habitat)
        sim_r <- NLMR::nlm_randomcluster(
          ncol    = NCOL,
          nrow    = NROW,
          p       = p,
          ai      = c(1 - Ai, Ai),
          rescale = FALSE
        )

        # Convert to terra SpatRaster, set non-habitat to NA
        hab_r <- terra::rast(sim_r)
        hab_r[hab_r == 1] <- NA
        
        # Written values: 2 (habitat), NA (non-habitat)
        terra::writeRaster(hab_r, filename = out_path, overwrite = TRUE)
      }
    }
  }
})

cat(sprintf("\nSimulation complete in %.1f seconds (%.1f min).\n",
            gen_time["elapsed"], gen_time["elapsed"] / 60))


# Spot check one landscape
test_key <- sprintf("clump%.1f_hab%.1f_rep%d", CLUMPING_VALS[1], HAB_AMOUNTS[1], 1)
test_r   <- terra::rast(file.path(NLMR_DIR, paste0(test_key, ".tif")))

cat(sprintf("\nSpot check (%s):\n", test_key))
cat(sprintf("  Dimensions: %d x %d cells\n", nrow(test_r), ncol(test_r)))
cat(sprintf("  Habitat cells:          %d\n", terra::global(test_r, "notNA")[1, 1]))
cat(sprintf("  Expected habitat (~%d): %.0f\n",
            round(HAB_AMOUNTS[1] * NCOL * NROW),
            terra::global(test_r, "notNA")[1, 1]))



# ---- Plot QA grid: all hab x clumping combinations (rep 1) ----------
# Adjust these to plot a subset or a different replicate
QA_HAB_VALS   <- c(0.1, 0.6) # HAB_AMOUNTS to plot all      # e.g. c(0.2, 0.4, 0.6) for a subset
QA_CLUMPING   <- c(0.1, 0.5) # CLUMPING_VALS to plot all    # e.g. c(0.1, 0.3, 0.5)
QA_REP        <- 1

grid_combos <- expand.grid(clumping = QA_CLUMPING, hab_amount = QA_HAB_VALS)
grid_combos <- grid_combos[order(grid_combos$clumping, grid_combos$hab_amount), ]

load_panel <- function(clumping, hab_amount, rep = QA_REP) {
  key <- sprintf("clump%.1f_hab%.1f_rep%d", clumping, hab_amount, rep)
  r   <- terra::rast(file.path(NLMR_DIR, paste0(key, ".tif")))
  df  <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df)[3] <- "value"
  df$habitat <- !is.na(df$value)
  df$panel_lab <- sprintf("hab=%.1f | clump=%.1f", hab_amount, clumping)
  df$panel_order <- sprintf("%.1f_%.1f", clumping, hab_amount)
  df
}

grid_df <- do.call(rbind, Map(load_panel, grid_combos$clumping, grid_combos$hab_amount))
grid_df$panel_lab <- factor(grid_df$panel_lab, levels = unique(grid_df$panel_lab))

p_grid <- ggplot(grid_df, aes(x = x, y = y, fill = habitat)) +
  geom_raster() +
  scale_fill_manual(values = c(`TRUE` = "#2c7a3f", `FALSE` = "white"), guide = "none") +
  coord_equal(expand = FALSE) +
  facet_wrap(~ panel_lab, ncol = length(QA_HAB_VALS)) +
  labs(title = "Simulated landscapes : nlmr random cluster algorithm") +
  theme_void(base_size = 12) +
  theme(
    # different plotting parameters if including all combinations
    # strip.text      = element_text(size = 7, colour = "grey20", margin = margin(b = 2)),
    # plot.title      = element_text(face = "bold", size = 12, margin = margin(b = 8)),
    # panel.spacing   = unit(0.4, "lines"),
    strip.text      = element_text(size = 10, colour = "grey20", margin = margin(b = 3)),
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    panel.spacing   = unit(0.5, "lines"),
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", colour = NA)
  )

qa_grid_path <- file.path(FIGURES_DIR, "simulated-landscapes-qa-grid.png")
ggsave(qa_grid_path, p_grid, width = 18, height = 13, dpi = 200)
