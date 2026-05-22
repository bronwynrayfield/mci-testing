################################################################
## 05-summarise-mci.R
## Summarise MCI output rasters to landscape-level values
## (mean and sum) and join to connectivity results.
##
## Inputs:  scripts/parameters.R
##          results/binary/omniscape-output/*.tif
##          results/binary/connectivity-reference-indicators.csv
## Outputs: results/binary/connectivity-with-mci.csv
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(terra)
library(dplyr)

# ---- Load existing connectivity results ----------
results <- read.csv(REFERENCE_CSV_PATH)

# ---- Get MCI rasters ----------
tif_files <- list.files(OMNISCAPE_DIR, pattern = "\\.tif$", full.names = TRUE)

# ---- Summarise each MCI raster ----------
mci_summary <- data.frame()

for (f in tif_files) {

  # Parse key and radius from filename e.g. clump0.1_hab0.1_rep1_r7.tif
  fname  <- gsub("\\.tif$", "", basename(f))
  # Extract radius from end of filename
  radius <- as.numeric(sub(".*_r([0-9]+)$", "\\1", fname))
  # Extract landscape key by removing _r<radius> suffix
  key    <- sub("_r[0-9]+$", "", fname)

  # Parse landscape parameters from key
  parts      <- strsplit(key, "_")[[1]]
  clumping   <- as.numeric(sub("clump", "", parts[1]))
  hab_amount <- as.numeric(sub("hab",   "", parts[2]))
  rep        <- as.numeric(sub("rep",   "", parts[3]))

  # Match integer radius back to dispersal distance (e.g. 7 -> 6.8)
  disp_dist  <- DISPERSAL_DISTANCES[DISPERSAL_DISTANCES_OMNI == radius]

  # Convert to alpha_label to match existing connectivity.csv format
  alpha_label <- sprintf("1/%.1f", disp_dist)

  # Read raster and compute summary statistics
  r    <- terra::rast(f)
  vals <- terra::values(r, na.rm = TRUE)

  mci_summary <- rbind(mci_summary, data.frame(
    hab_amount  = hab_amount,
    clumping    = clumping,
    rep         = rep,
    alpha_label = alpha_label,
    MCI_mean    = mean(vals),
    MCI_sum     = sum(vals)
  ))
}

# ---- Join MCI summary to connectivity results ----------
results_combined <- results %>%
  left_join(mci_summary, by = c("hab_amount", "clumping", "rep", "alpha_label"))

# ---- Save ----------
write.csv(results_combined, MCI_CSV_PATH, row.names = FALSE)