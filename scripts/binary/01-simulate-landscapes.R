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

# ---- Create output directory ----------
dir.create(GEOTIFF_DIR, recursive = TRUE, showWarnings = FALSE)

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
        out_path <- file.path(GEOTIFF_DIR, paste0(key, ".tif"))

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
test_r   <- terra::rast(file.path(GEOTIFF_DIR, paste0(test_key, ".tif")))

cat(sprintf("\nSpot check (%s):\n", test_key))
cat(sprintf("  Dimensions: %d x %d cells\n", nrow(test_r), ncol(test_r)))
cat(sprintf("  Habitat cells:          %d\n", terra::global(test_r, "notNA")[1, 1]))
cat(sprintf("  Expected habitat (~%d): %.0f\n",
            round(HAB_AMOUNTS[1] * NCOL * NROW),
            terra::global(test_r, "notNA")[1, 1]))
