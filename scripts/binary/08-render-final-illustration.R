################################################################
## 08-render-final-illustration.R
## Render the chosen candidate as two standalone, presentation-
## ready figures (binary landscape, MCI map), each with a legend.
##
## Inputs:  scripts/parameters.R
##          landscapes/binary/simulated-landscapes-tif/*.tif
##          results/binary/omniscape-output/*.tif
## Outputs: results/binary/figures/illustration-habitat.png
##          results/binary/figures/illustration-mci.png
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(terra)
library(ggplot2)
library(tidyterra)

# ---- Chosen candidate ----------
SEL_HAB       <- 0.3
SEL_CLUMPING  <- 0.6
SEL_REP       <- 1
SEL_DISP_DIST <- 10   # r7

radius <- DISPERSAL_DISTANCES_OMNI[DISPERSAL_DISTANCES == SEL_DISP_DIST]
key    <- sprintf("clump%.1f_hab%.1f_rep%d", SEL_CLUMPING, SEL_HAB, SEL_REP)

hab_path <- file.path(NLMR_DIR, paste0(key, ".tif"))
mci_path <- file.path(OMNISCAPE_DIR, sprintf("%s_r%d.tif", key, radius))

stopifnot(file.exists(hab_path), file.exists(mci_path))

hab_r <- terra::rast(hab_path)
mci_r <- terra::rast(mci_path)

true_pct  <- round(terra::global(hab_r, "notNA")[1, 1] / (NCOL * NROW) * 100, 1)
n_patches <- length(unique(terra::patches(hab_r, directions = 8)[]))

cat(sprintf("Selected: %s | target hab = %.0f%%, true hab = %.1f%%, patches = %d\n",
            key, SEL_HAB * 100, true_pct, n_patches))

# ---- Habitat map ----------
hab_df <- as.data.frame(hab_r, xy = TRUE, na.rm = FALSE)
names(hab_df)[3] <- "value"
hab_df$class <- ifelse(is.na(hab_df$value), "Non-habitat", "Habitat")

p_hab <- ggplot(hab_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  scale_fill_manual(values = c("Habitat" = "#2c7a3f", "Non-habitat" = "white"),
                    name = NULL) +
  coord_equal(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom")

# ---- MCI map ----------
p_mci <- ggplot() +
  tidyterra::geom_spatraster(data = mci_r) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "white",
                       name = "MCI (effective resistance)") +
  coord_equal(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 0.6, title.position = "top"))

# ---- Save individually (identical canvas size, incl. legend) ----------
hab_out <- file.path(FIGURES_DIR, "illustration-habitat.png")
mci_out <- file.path(FIGURES_DIR, "illustration-mci.png")

FIG_WIDTH  <- 6
FIG_HEIGHT <- 6.8

ggsave(hab_out, p_hab, width = FIG_WIDTH, height = FIG_HEIGHT, dpi = 300)
ggsave(mci_out, p_mci, width = FIG_WIDTH, height = FIG_HEIGHT, dpi = 300)

cat(sprintf("Saved:\n  %s\n  %s\n", hab_out, mci_out))