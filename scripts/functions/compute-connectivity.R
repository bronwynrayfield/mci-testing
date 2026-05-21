################################################################
## compute-connectivity.R
## Computes landscape connectivity indicators for a single
## binary habitat raster.
##
## Indicators:
##   n_patches  number of habitat patches
##   hab_area   total habitat area (cells)
##   mean_pa    mean patch area (cells)
##   MPC        metapopulation capacity (Hanski & Ovaskainen 2000)
##   ECA        equivalent connected area (Saura et al. 2011)
##   ECAAp      ECA as fraction of total habitat area
##   mean_ND    mean node degree
##
## Called by: scripts/binary/02-compute-connectivity.R
################################################################

compute_connectivity <- function(hab_raster, alpha, min_patch_area = 1) {

  # ---- Identify habitat patches ----------
  patches_r <- terra::patches(hab_raster, directions = 8, zeroAsNA = TRUE)

  patch_areas_tbl <- terra::freq(patches_r, bylayer = FALSE)
  patch_areas_tbl <- patch_areas_tbl[!is.na(patch_areas_tbl$value), ]
  patch_areas_tbl <- patch_areas_tbl[patch_areas_tbl$count >= min_patch_area, ]

  # ---- Single or zero patch: return early ----------
  if (nrow(patch_areas_tbl) < 2) {
    a_single <- patch_areas_tbl$count[1]
    return(data.frame(
      n_patches = nrow(patch_areas_tbl),
      hab_area  = sum(patch_areas_tbl$count),
      mean_pa   = ifelse(nrow(patch_areas_tbl) > 0, mean(patch_areas_tbl$count), NA),
      MPC       = ifelse(nrow(patch_areas_tbl) == 1, a_single, NA),
      ECA       = ifelse(nrow(patch_areas_tbl) == 1, a_single, NA),
      ECAAp     = ifelse(nrow(patch_areas_tbl) == 1, 1.0, NA),
      mean_ND   = ifelse(nrow(patch_areas_tbl) == 1, 0, NA)
    ))
  }

  patch_ids   <- patch_areas_tbl$value
  patch_areas <- patch_areas_tbl$count
  n           <- length(patch_ids)

  # ---- Compute patch centroids ----------
  patches_poly <- terra::as.polygons(patches_r, dissolve = TRUE)
  poly_ids     <- patches_poly$patches
  match_order  <- match(patch_ids, poly_ids)
  patches_poly <- patches_poly[match_order, ]
  cents        <- terra::centroids(patches_poly)
  coords       <- terra::crds(cents)

  # ---- Dispersal probability matrix ----------
  # p_ij = exp(-alpha * d_ij)  [Eq. 1 in Oehri et al. 2024]
  dist_mat <- as.matrix(dist(coords))
  prob_mat <- exp(-alpha * dist_mat)
  diag(prob_mat) <- 0

  # ---- Metapopulation capacity (MPC) ----------
  # Leading eigenvalue of landscape matrix M
  # M_ij = f(d_ij) * a_i^x * a_j^x, x = 0.5
  # (Hanski & Ovaskainen 2000)
  x <- 0.5
  a <- patch_areas
  M <- outer(a^x, a^x) * prob_mat
  diag(M) <- a^(2 * x)
  eig     <- eigen(M, symmetric = FALSE, only.values = TRUE)
  MPC_val <- max(Re(eig$values))

  # ---- Equivalent connected area (ECA) ----------
  # Via igraph max-product dispersal paths
  # (Saura et al. 2011)
  edge_list <- which(prob_mat > 0 & upper.tri(prob_mat), arr.ind = TRUE)

  if (nrow(edge_list) == 0) {
    ECA_val   <- sqrt(sum(a^2))
    ECAAp_val <- ECA_val / sum(a)
    mean_ND   <- 0
  } else {
    weights <- -log(prob_mat[edge_list])
    g <- igraph::graph_from_edgelist(edge_list, directed = FALSE)
    igraph::E(g)$weight <- weights
    sp     <- igraph::distances(g, weights = igraph::E(g)$weight)
    p_star <- exp(-sp)
    diag(p_star) <- 1

    ECA_val   <- sqrt(sum(outer(a, a) * p_star))
    ECAAp_val <- ECA_val / sum(a)

    # ---- Mean node degree (ND) ----------
    # Number of patches connected above threshold p* > 0.01
    adj_mat       <- (p_star > 0.01)
    diag(adj_mat) <- FALSE
    mean_ND       <- mean(rowSums(adj_mat))
  }

  data.frame(
    n_patches = n,
    hab_area  = sum(a),
    mean_pa   = mean(a),
    MPC       = MPC_val,
    ECA       = ECA_val,
    ECAAp     = ECAAp_val,
    mean_ND   = mean_ND
  )
}
