################################################################
## Shared parameters for the connectivity analysis pipeline.
## Sourced by all scripts in scripts/binary/
###############################################################


# ---- Landscape simulation ----------

set.seed(42)

# Landscape size (cells)
NCOL <- 250
NROW <- 250

# Habitat amount gradient (Ai parameter in Saura & Martínez-Millán 2000)
HAB_AMOUNTS <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# Fragmentation / clumping gradient
# Lower = more fragmented, higher = more clumped
CLUMPING_VALS <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

# Replicates per combination
N_REP <- 10


# ---- Connectivity analysis ----------

# Minimum patch area threshold (cells)
MIN_PATCH_AREA <- 1

# Dispersal distances in cell units
# Note: 6.8 rounded to 7 for Omniscape integer radius requirement
DISPERSAL_DISTANCES      <- c(1, 6.8, 10, 50, 150, 355)
DISPERSAL_DISTANCES_OMNI <- round(DISPERSAL_DISTANCES)

# Alpha values derived from dispersal distances
ALPHA_VALS <- 1 / DISPERSAL_DISTANCES


# ---- Omniscape / MCI ----------

# Resistance values for habitat conversion
RESISTANCE_HABITAT     <- 1    # habitat cells (value 2 in NLMR output)
RESISTANCE_NON_HABITAT <- 100  # non-habitat cells (NA in NLMR output)

# Fixed Omniscape parameters
OMNISCAPE_MODE             <- "pairwise-direct"
OMNISCAPE_NUM_SPOKES       <- 16
OMNISCAPE_BLOCK_SIZE       <- 1
OMNISCAPE_SOURCE_FROM_RES  <- "true"
OMNISCAPE_SPOKE_AGGREGATION <- "mean"


# ---- Paths ----------
# All paths relative to project root
LANDSCAPE_TYPE <- "binary"

LANDSCAPES_DIR <- file.path("landscapes", LANDSCAPE_TYPE)
RESULTS_DIR    <- file.path("results", LANDSCAPE_TYPE)

# Landscape inputs
GEOTIFF_DIR    <- file.path(LANDSCAPES_DIR, "simulated-landscapes-tif")
RESISTANCE_DIR <- file.path(LANDSCAPES_DIR, "resistance-tif")

# Results outputs
OMNISCAPE_DIR  <- file.path(RESULTS_DIR, "omniscape-output")
FIGURES_DIR    <- file.path(RESULTS_DIR, "figures")
CSV_PATH       <- file.path(RESULTS_DIR, "connectivity.csv")
FIG_PATH       <- file.path(FIGURES_DIR, "Oehri_SI_Fig6_reproduce.png")


# ---- Plotting ----------

# One colour per dispersal distance, consistent across all plots
PALETTE_COLS <- setNames(
  c("#3b528b", "#440154", "#E91E8C", "#FF6347", "#f89540", "#fde725"),
  as.character(DISPERSAL_DISTANCES)
)