geox_relate_pattern_mat <- function(shp, pattern) {
  shp <- geos::as_geos_geometry(shp)
  nby <- geos::geos_strtree_query(
    geos::geos_strtree(shp),
    shp
  )
  lapply(seq_len(length(shp)), function(i) {
    x <- geos::geos_relate(shp[[i]], shp[[nby[[i]]]])
    nby[[i]][geos::geos_relate_pattern_match(x, 'F***T****')]
  })
}

geox_distance_mat <- function(x) {
  lx <- length(x)
    out <- matrix(0, lx, lx)
  y <- geos::geos_make_collection(x)

  for (i in seq_len(lx - 1)) {
    out[i, i:lx] <- geos::geos_distance(x[[i]], geos::geos_geometry_n(y, i:lx))
  }

  out + t(out)
}

geox_union <- function(x) {
  geos::geos_unary_union(geos::geos_make_collection(x))
}

geox_coordinates <- function(x) {
  cbind(geos::geos_x(x), geos::geos_y(x))
}


geox_sub_centroid <- function(coordinates, areas, idx) {
  c(stats::weighted.mean(coordinates[idx, 1], areas[idx]),
    stats::weighted.mean(coordinates[idx, 2], areas[idx]))
}

# geox_point_list <- function(x) {
#   x <- geos::as_geos_geometry(x)
#   pts <- lapply(seq_along(x), function(i) {
#     geos::geos_unnest(geos::geos_boundary(x[[i]]), keep_multi = FALSE)
#   })
#   n_pts <- lapply(pts, function(y) geos::geos_num_coordinates(y))
#   lapply(seq_along(pts), function(i){
#     lapply(seq_along(n_pts[[i]]), function(n) {
#       inner_pt <- geos::geos_point_n(pts[[i]][n], 1:n_pts[[i]][n])
#       cbind(geos::geos_x(inner_pt), geos::geos_y(inner_pt))
#     })
#   })
# }
