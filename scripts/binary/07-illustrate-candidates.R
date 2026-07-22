################################################################
## 07-illustrate-candidates.R
## Render a grid of candidate binary landscape / MCI map pairs
## for picking a presentation illustration of the MCI metric.
##
## Inputs:  scripts/parameters.R
##          landscapes/binary/simulated-landscapes-tif/*.tif
##          results/binary/omniscape-output/*.tif
## Outputs: results/binary/figures/mci-illustration-candidates.png
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)

# ---- Settings ----------
# Candidates to render: mid-range habitat amount, low-to-mid clumping,
# a few replicates each
CAND_HAB       <- c(0.2)
CAND_CLUMPING  <- c(0.3,0.5,0.6)
CAND_REPS      <- c(2)
CAND_DISP_DIST <- 10   # r7 -- best balance of smoothness / legibility

radius <- DISPERSAL_DISTANCES_OMNI[DISPERSAL_DISTANCES == CAND_DISP_DIST]

# ---- Build candidate keys ----------
candidates <- expand.grid(
  clumping = CAND_CLUMPING,
  rep      = CAND_REPS
)

# ---- Panel function ----------
make_pair_panel <- function(clumping, rep, hab_amount, radius) {
  
  key <- sprintf("clump%.1f_hab%.1f_rep%d", clumping, hab_amount, rep)
  
  hab_path <- file.path(NLMR_DIR, paste0(key, ".tif"))
  mci_path <- file.path(OMNISCAPE_DIR, sprintf("%s_r%d.tif", key, radius))
  
  stopifnot(file.exists(hab_path), file.exists(mci_path))
  
  hab_r <- terra::rast(hab_path)
  mci_r <- terra::rast(mci_path)
  
  n_patches <- length(unique(terra::patches(hab_r, directions = 8)[]))
  true_pct  <- round(terra::global(hab_r, "notNA")[1, 1] / (NCOL * NROW) * 100, 0)
  
  p_hab <- ggplot() +
    tidyterra::geom_spatraster(data = hab_r) +
    scale_fill_gradient(low = "white", high = "#2c7a3f", na.value = "white", guide = "none") +
    theme_void(base_size = 9) +
    coord_equal(expand = FALSE) +
    labs(subtitle = sprintf("clump=%.1f rep=%d (%d%% hab, %d patches)",
                            clumping, rep, true_pct, n_patches))
  
  p_mci <- ggplot() +
    tidyterra::geom_spatraster(data = mci_r) +
    scale_fill_viridis_c(option = "magma", na.value = "white", guide = "none") +
    theme_void(base_size = 9) +
    coord_equal(expand = FALSE)
  
  p_hab + p_mci
}

# ---- Build all panels ----------
panels <- purrr::pmap(
  candidates,
  function(clumping, rep) make_pair_panel(clumping, rep, CAND_HAB, radius)
)

# ---- Assemble grid: one row per candidate ----------
fig_candidates <- patchwork::wrap_plots(panels, ncol = 2) +
  patchwork::plot_annotation(
    title    = "MCI illustration candidates",
    subtitle = sprintf("hab_amount target = %.1f | dispersal distance = %.1f cells (r%d)",
                       CAND_HAB, CAND_DISP_DIST, radius)
  )

# ---- Save ----------
out_path <- file.path(FIGURES_DIR, "mci-illustration-candidates.png")
ggsave(out_path, fig_candidates, width = 8, height = 3.2 * nrow(candidates), dpi = 300, limitsize = FALSE)

cat(sprintf("Saved %d candidate pairs to %s\n", nrow(candidates), out_path))
cat("Pick the pair with the clearest gap-vs-corridor contrast for the slide.\n")