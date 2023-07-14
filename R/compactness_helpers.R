# use planarize as a function with argument epsg for clarity
planarize <- function(shp, epsg = 3857) {
  if (isTRUE(sf::st_is_longlat(sf::st_geometry(shp)))) {
    if (!is.null(sf::st_crs(shp)) & !is.null(epsg) & !isFALSE(epsg)) {
      shp <- sf::st_transform(shp, epsg)
    } else {
      if (!isFALSE(epsg)) {
        cli::cli_warn('Planarizing skipped.')
      }
    }
  }

  shp
}

#' Prep Polsby Popper Perimeter Tibble
#'
#' Replaces `redist.prep.polsbypopper`
#'
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @param perim_path A path to save an rds
#' @templateVar ncores TRUE
#' @template template
#'
#' @return tibble of perimeters and lengths
#' @export
#'
#' @examples
#' data(nh)
#' prep_perims(nh)
#'
prep_perims <- function(shp, epsg = 3857, perim_path, ncores = 1) {
  if (missing(shp)) {
    cli::cli_abort('Please provide an argument to {.arg shp}.')
  }

  shp <- dplyr::ungroup(shp)
  shp <- planarize(shp, epsg)
  shp <- geos::as_geos_geometry(shp)
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }

  alist <- geox_relate_pattern_mat(shp, pattern = 'F***T****')

  invalid <- which(!geos::geos_is_valid(shp))
  for (i in seq_along(invalid)) {
    shp[[invalid[i]]] <- geos::geos_buffer(shp[[invalid[i]]], 0)
  }

  perims <- geos::geos_length(shp)

  nc <- min(ncores, length(shp))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  perim_adj_df <- foreach::foreach(from = seq_along(alist), .combine = 'rbind',
                                   .packages = c('sf', 'redistmetrics')) %oper% {
                                     cat(i)
    x <- geos::geos_geometry_n(shp_col, from)
    y <- geos::geos_geometry_n(shp_col, alist[[from]])
    l <- geos::geos_intersects_matrix(x, y) %>% unlist() %>% sort()
    l_lines <- sapply(seq_along(l), function(i) {
      geos::geos_length(geos::geos_intersection(x, y[[l[[i]]]]))
    })

    if (length(alist[[from]] > 0)) {
      data.frame(
        origin = from,
        touching = alist[[from]],
        edge = l_lines
      )
    } else {
      data.frame(origin = from, touching = NA, edge = -1)
    }
  }

  perim_adj_island <- perim_adj_df %>%
    dplyr::filter(edge == -1) %>%
    dplyr::mutate(edge = 0)

  perim_adj_df_out <- perim_adj_df %>%
    dplyr::filter(edge > 0) %>%
    rbind(perim_adj_island)
  queen_bug <- which(!(seq_along(alist) %in% unique(perim_adj_df_out$origin)))
  if (length(queen_bug) > 0) {
    perim_adj_queens <- perim_adj_df %>%
      dplyr::filter(edge == 0, origin %in% queen_bug)
    perim_adj_df_out <- dplyr::bind_rows(perim_adj_df_out, perim_adj_queens)
  }


  adj_boundary_lengths <- perim_adj_df_out %>%
    dplyr::group_by(origin) %>%
    dplyr::summarize(perim_adj = sum(edge)) %>%
    dplyr::mutate(
      perim_full = as.numeric(perims),
      perim_boundary = perim_full - perim_adj,
      X1 = -1
    ) %>%
    dplyr::filter(perim_boundary > .001) %>%
    dplyr::select(X1, origin, perim_boundary) %>%
    dplyr::rename(origin = X1, touching = origin, edge = perim_boundary)

  perim_df <- dplyr::bind_rows(perim_adj_df_out, adj_boundary_lengths) %>%
    dplyr::arrange(origin, touching) %>%
    dplyr::filter(!is.na(touching)) # %>% filter(touching > origin)

  if (!missing(perim_path)) {
    try(expr = {
      saveRDS(object = perim_df, file = perim_path)
    }, silent = TRUE)
  }

  perim_df
}
