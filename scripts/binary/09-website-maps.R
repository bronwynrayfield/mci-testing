################################################################
## 09-website-maps.R
## Render selected simulated landscapes as individual,
## identically-sized figures for use in an external layout
## (e.g. PowerPoint).
##
## Inputs:  scripts/parameters.R
##          landscapes/binary/simulated-landscapes-tif/*.tif
## Outputs: results/binary/figures/website-map-clump0.3.png
##          results/binary/figures/website-map-clump0.5.png
##          results/binary/figures/website-map-clump0.6.png
################################################################

# ---- Source parameters ----------
source("scripts/parameters.R")

# ---- Load libraries ----------
library(terra)
library(ggplot2)
library(purrr)

# ---- Selected landscapes ----------
selected <- tibble::tibble(
  clumping   = c(0.3, 0.5, 0.6),
  rep        = c(2, 2, 1),
  hab_amount = 0.2
)

# ---- Fixed output size (identical across all maps) ----------
FIG_SIZE <- 5   # inches, square
BG_COL   <- "#f7f5f0"
HAB_COL  <- "#2c7a3f"

# ---- Map function ----------
make_map <- function(clumping, rep, hab_amount) {
  key <- sprintf("clump%.1f_hab%.1f_rep%d", clumping, hab_amount, rep)
  r   <- terra::rast(file.path(NLMR_DIR, paste0(key, ".tif")))
  df  <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df)[3] <- "value"
  df$habitat <- !is.na(df$value)
  
  p <- ggplot(df, aes(x = x, y = y, fill = habitat)) +
    geom_raster() +
    scale_fill_manual(values = c(`TRUE` = HAB_COL, `FALSE` = BG_COL), guide = "none") +
    coord_equal(expand = FALSE) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = BG_COL, colour = NA),
      panel.background = element_rect(fill = BG_COL, colour = NA),
      plot.margin      = margin(4, 4, 4, 4)
    )
  
  out_path <- file.path(FIGURES_DIR, sprintf("website-map-clump%.1f.png", clumping))
  ggsave(out_path, p, width = FIG_SIZE, height = FIG_SIZE, dpi = 300)
  cat(sprintf("Saved %s\n", out_path))
  out_path
}

# ---- Render all ----------
out_paths <- purrr::pmap_chr(selected, make_map)