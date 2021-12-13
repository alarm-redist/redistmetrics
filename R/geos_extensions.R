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
  c(geos::geos_x(x), geos::geos_y(x))
}
