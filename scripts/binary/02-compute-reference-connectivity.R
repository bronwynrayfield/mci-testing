################################################################
## 02-compute-connectivity.R
## Compute landscape connectivity indicators (MPC, ECA, ECAAp,
## mean node degree) for all simulated landscapes and alpha values.
##
## Inputs:  scripts/parameters.R
##          scripts/functions/compute-connectivity.R
##          landscapes/binary/simulated-landscapes-tif/*.tif
## Outputs: results/binary/connectivity.csv
################################################################

# ---- Source parameters and functions ----------
source("scripts/parameters.R")
source("scripts/functions/compute-connectivity.R")

# ---- Load libraries ----------
library(terra)
library(igraph)
library(dplyr)

# ---- Settings ----------
# Set TRUE to append new alpha results to existing CSV
# Set FALSE to overwrite (e.g. starting fresh)
APPEND_RESULTS <- TRUE

# ---- Load landscapes ----------
tif_files      <- list.files(NLMR_DIR, pattern = "\\.tif$", full.names = TRUE)
landscape_list <- lapply(tif_files, terra::rast)
names(landscape_list) <- gsub("\\.tif$", "", basename(tif_files))

# ---- Compute connectivity ----------
conn_time <- system.time({

  all_results <- list()

  for (a_idx in seq_along(ALPHA_VALS)) {

    alpha     <- ALPHA_VALS[a_idx]
    disp_dist <- DISPERSAL_DISTANCES[a_idx]

    results_list <- list()
    counter      <- 0

    for (key in names(landscape_list)) {

      counter <- counter + 1

      # Parse parameters from key name
      parts      <- strsplit(key, "_")[[1]]
      clumping   <- as.numeric(sub("clump", "", parts[1]))
      hab_amount <- as.numeric(sub("hab",   "", parts[2]))
      rep        <- as.numeric(sub("rep",   "", parts[3]))

      conn_df <- tryCatch(
        compute_connectivity(landscape_list[[key]],
                             alpha          = alpha,
                             min_patch_area = MIN_PATCH_AREA),
        error = function(e) {
          data.frame(n_patches=NA, hab_area=NA, mean_pa=NA,
                     MPC=NA, ECA=NA, ECAAp=NA, mean_ND=NA)
        }
      )

      results_list[[counter]] <- cbind(
        data.frame(
          hab_amount = hab_amount,
          clumping   = clumping,
          rep        = rep,
          disp_dist  = disp_dist
        ),
        conn_df
      )
    }

    all_results[[as.character(disp_dist)]] <- dplyr::bind_rows(results_list)
  }
})

results_df <- dplyr::bind_rows(all_results)
cat(sprintf("Connectivity analysis complete in %.1f seconds (%.1f min).\n",
            conn_time["elapsed"], conn_time["elapsed"] / 60))

# ---- Save results ----------
if (APPEND_RESULTS && file.exists(CSV_PATH)) {
  results_existing <- read.csv(CSV_PATH)
  results_combined <- dplyr::bind_rows(results_existing, results_df)
  write.csv(results_combined, CSV_PATH, row.names = FALSE)
} else {
  write.csv(results_df, CSV_PATH, row.names = FALSE)
}
