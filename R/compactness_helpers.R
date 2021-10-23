# use planarize as a function with argument epsg for clarity
planarize <- function(shp, epsg = 3857) {
  if (isTRUE(sf::st_is_longlat(sf::st_geometry(shp)))) {
    if (!is.null(sf::st_crs(shp)) & !is.null(epsg) & !isFALSE(epsg)) {
      shp <- sf::st_transform(shp, epsg)
    } else {
      cli::cli_warn('Planarizing skipped.')
    }
  }

  shp
}

# new backend for redist.prep.polsbypopper
prep_polsby <- function(shp, planarize = 3857, perim_path, ncores = 1) {

}
