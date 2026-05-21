################################################################
## 03-convert-to-resistance.R
## Convert binary habitat rasters to resistance rasters for
## use in Omniscape / MCI analysis.
##
## Conversion:
##   2   (habitat)     -> 1   (low resistance)
##   NA  (non-habitat) -> 100 (high resistance)
##
## Inputs:  scripts/parameters.R
##          landscapes/binary/simulated-landscapes-tif/*.tif
## Outputs: landscapes/binary/resistance-tif/*.tif
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(terra)

# ---- Convert rasters ----------
tif_files <- list.files(NLMR_DIR, pattern = "\\.tif$", full.names = TRUE)

conv_time <- system.time({
  for (f in tif_files) {

    key   <- gsub("\\.tif$", "", basename(f))
    hab_r <- terra::rast(f)

    # Recode: 2 (habitat) -> RESISTANCE_HABITAT
    res_r <- terra::classify(hab_r,
                             rcl    = matrix(c(2, RESISTANCE_HABITAT), ncol = 2),
                             others = NA)

    # Fill remaining NAs (non-habitat) -> RESISTANCE_NON_HABITAT
    res_r <- terra::cover(res_r,
                          terra::setValues(res_r, RESISTANCE_NON_HABITAT))

    # Written values: 1 (habitat), 100 (non-habitat)
    terra::writeRaster(
      res_r,
      filename  = file.path(RESISTANCE_DIR, paste0(key, ".tif")),
      overwrite = TRUE
    )
  }
})

cat(sprintf("Conversion complete in %.1f seconds (%.1f min).\n",
            conv_time["elapsed"], conv_time["elapsed"] / 60))

# ---- Spot check ----------
test_key <- gsub("\\.tif$", "", basename(tif_files[1]))
test_r   <- terra::rast(file.path(RESISTANCE_DIR, paste0(test_key, ".tif")))

cat(sprintf("\nSpot check (%s):\n", test_key))
print(table(terra::values(test_r), useNA = "always"))
