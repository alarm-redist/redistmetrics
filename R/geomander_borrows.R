# will work out a cleaner way to use these, just self-stealing for now

geo_filter <- function(from, to) {
  ints <- geos::geos_intersects(from,
                                geos::geos_unary_union(geos::geos_make_collection(to)))

  from[ints, ]
}

geo_trim <- function(from, to, thresh = 0.01) {
  ints <- geos::geos_intersection(from, geos::geos_unary_union(geos::geos_make_collection(to)))
  area <- geos::geos_area(from)
  areaints <- rep(0, nrow(from))
  areaints <- geos::geos_area(geos::geos_make_valid(ints)) # , NA_on_exception = TRUE in valid
  keep <- as.numeric(areaints / area) > thresh

  keep[is.na(keep)] <- FALSE

  from[keep, ]
}
