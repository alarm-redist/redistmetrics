geox_relate_pattern_mat <- function(shp, pattern) {
  shp <- geos::as_geos_geometry(shp)
  nby <- geos::geos_strtree_query(geos::geos_strtree(shp),
                                  shp)
  lapply(seq_len(length(shp)), function(i) {
    x <- geos::geos_relate(shp[[i]], shp[[nby[[i]]]])
    nby[[i]][geos::geos_relate_pattern_match(x, "F***T****")]
  })
}
