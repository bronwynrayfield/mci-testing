################################################################
## 10-website-maps.R
## Render the selected simulated landscapes as individual,
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
library(magick)
library(grid)

# ---- Selected landscapes ----------
selected <- tibble::tibble(
  clumping   = c(0.3, 0.5, 0.6),
  rep        = c(2, 2, 1),
  hab_amount = 0.2
)

# ---- Fixed output size (identical across all maps) ----------
FIG_SIZE      <- 5   # inches, square
BG_COL        <- "white"
HAB_COL       <- "#2c7a3f"
CORNER_RADIUS <- 0.08   # fraction of tile size; increase for more rounding

# ---- Rounded-corner clip (post-process, since ggplot can't clip a raster to a shape) ----------
# Build a mask that is a genuinely transparent image (alpha=0) outside a rounded
# rectangle and opaque (alpha=1) inside it, then use "DstIn" to multiply that
# mask's alpha into the existing image's alpha -- this composites two real
# alpha channels together natively, rather than hand-extracting/inverting
# grayscale channels (which is ambiguous across ImageMagick versions and was
# causing the fill to invert).
round_corners <- function(path, radius_frac = CORNER_RADIUS) {
  img  <- magick::image_read(path)
  info <- magick::image_info(img)
  w <- info$width; h <- info$height
  
  mask <- magick::image_graph(width = w, height = h, bg = "transparent")
  grid::grid.roundrect(
    width = unit(1, "npc"), height = unit(1, "npc"),
    r = unit(radius_frac, "npc"),
    gp = gpar(fill = "white", col = NA)
  )
  dev.off()
  
  out <- magick::image_composite(img, mask, operator = "DstIn")
  magick::image_write(out, path)
}

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
  ggsave(out_path, p, width = FIG_SIZE, height = FIG_SIZE, dpi = 300, bg = "white")
  round_corners(out_path)
  out_path
}

# ---- Render all ----------
out_paths <- purrr::pmap_chr(selected, make_map)