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

#' Prep Polsby Popper Perimeter Tibble
#'
#' Replaces `redist.prep.polsbypopper`
#'
#' @param shp todo inherits
#' @param epsg todo inherits
#' @param perim_path A path to save an rds
#' @param ncores todo inherits
#'
#' @return tibble of perimeters and lengths
#' @export
#'
#' @examples
#' # todo example
prep_perims <- function(shp, epsg = 3857, perim_path, ncores = 1) {

  if(missing(shp)){
    cli::cli_abort('Please provide an argument to `shp`.')
  }

  shp <- planarize(shp, epsg)

  suppressMessages(alist <- sf::st_relate(shp, pattern = "F***T****"))


  #alist <- lapply(adj, function(x){x+1L})
  invalid <- which(!sf::st_is_valid(shp))
  sf::st_geometry(shp[invalid,]) <- sf::st_geometry(sf::st_buffer(shp[invalid,],0))

  if (sf::st_is_longlat(shp)) {
    perim <- sum(sf::st_length(sf::st_cast(united, "MULTILINESTRING")))
  } else {
    suppressWarnings(perims <- lwgeom::st_perimeter(shp))
  }


  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  perim_adj_df <- foreach::foreach(from = 1:length(alist), .combine = 'rbind', .packages = c('sf', 'rict')) %oper% {
    suppressWarnings(lines <- sf::st_intersection(shp[from,], shp[alist[[from]],]))
    l_lines <- sf::st_length(lines)
    if(length(alist[[from]] > 0)){
      data.frame(origin = from,
                 touching = alist[[from]],
                 edge = as.vector(l_lines))
    } else {
      data.frame(origin = from, touching = NA, edge = -1)
    }

  }

  perim_adj_island <- perim_adj_df %>%
    dplyr::filter(edge == -1) %>%
    dplyr::mutate(edge = 0)
  perim_adj_df <- perim_adj_df %>%
    dplyr::filter(edge > 0) %>%
    rbind(perim_adj_island)


  adj_boundary_lengths <- perim_adj_df %>%
    dplyr::group_by(origin) %>%
    dplyr::summarize(perim_adj = sum(edge)) %>%
    dplyr::mutate(perim_full = as.numeric(perims),
           perim_boundary = perim_full - perim_adj,
           X1 = -1) %>%
    dplyr::filter(perim_boundary > .001) %>%
    dplyr::select(X1, origin, perim_boundary) %>%
    dplyr::rename(origin = X1, touching = origin, edge = perim_boundary)

  perim_df <- dplyr::bind_rows(perim_adj_df, adj_boundary_lengths) %>%
    dplyr::arrange(origin, touching) %>%
    dplyr::filter(!is.na(touching))# %>% filter(touching > origin)

  if(!missing(perim_path)){
    try(expr = { saveRDS(object = perim_df, file = perim_path) }, silent = TRUE)
  }

  perim_df
}
