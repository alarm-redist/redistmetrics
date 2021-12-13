#' Calculate Polsby Popper Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param use_Rcpp Logical. Use Rcpp?
#' @param perim_path path to perimeter tibble saved by `prep_perims()`
#' @param perim_df tibble of perimeters from `prep_perims()`
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_polsby(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' comp_polsby(plans = nh_m[, 3:5], shp = nh)
#'
comp_polsby <- function(plans, shp, use_Rcpp, perim_path, perim_df, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)
  V <- nrow(plans)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  # Rcpp ----
  if (missing(use_Rcpp)) {
    use_Rcpp <- ncol(plans) > 8 || !missing(perim_path) || !missing(perim_df)
  }
  if (missing(perim_df)) {
    if (missing(perim_path)) {
      perim_df <- prep_perims(shp = shp, epsg = epsg, ncores = ncores)
    } else {
      perim_df <- readRDS(perim_path)
    }
  }

  # calculate ----
  areas <- geos::geos_area(shp)
  if (use_Rcpp) {
    splits <- split(x = plans, rep(1:nc, each = ceiling(n_plans / nc) * V)[1:(n_plans * V)]) %>%
      lapply(., FUN = function(x, r = V) matrix(data = x, nrow = r))

    result <- foreach::foreach(map = 1:nc, .combine = 'cbind', .packages = c('redistmetrics')) %oper% {
      polsbypopper(
        from = perim_df$origin, to = perim_df$touching, area = areas,
        perimeter = perim_df$edge, dm = splits[[map]], nd = nd
      )
    }
  } else {
    result <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('geos')) %oper% {
      ret <- vector('numeric', nd)

      for (i in 1:nd) {
        united <- geox_union(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
        area <- sum(areas[plans[, map] == dists[i]])

          perim <- sum(geos::geos_length(united))

        ret[i] <- 4 * pi * (area) / (perim)^2
      }

      ret
    }
  }

  c(result)
}

#' Calculate Schwartzberg Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param use_Rcpp Logical. Use Rcpp?
#' @param perim_path path to perimeter tibble saved by `prep_perims()`
#' @param perim_df tibble of perimeters from `prep_perims()`
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_schwartz(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' comp_schwartz(plans = nh_m[, 3:5], shp = nh)
#'
comp_schwartz <- function(plans, shp, use_Rcpp, perim_path, perim_df, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)
  V <- nrow(plans)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  # Rcpp ----
  if (missing(use_Rcpp)) {
    use_Rcpp <- ncol(plans) > 8 || !missing(perim_path) || !missing(perim_df)
  }
  if (missing(perim_df)) {
    if (missing(perim_path)) {
      perim_df <- prep_perims(shp = shp, epsg = epsg, ncores = ncores)
    } else {
      perim_df <- readRDS(perim_path)
    }
  }

  # calculate ----
  areas <- geos::geos_area(shp)
  if (use_Rcpp) {
    splits <- split(x = plans, rep(1:nc, each = ceiling(n_plans / nc) * V)[1:(n_plans * V)]) %>%
      lapply(., FUN = function(x, r = V) matrix(data = x, nrow = r))

    result <- foreach::foreach(map = 1:nc, .combine = 'cbind', .packages = c('redistmetrics')) %oper% {
      schwartzberg(
        from = perim_df$origin, to = perim_df$touching, area = areas,
        perimeter = perim_df$edge, dm = splits[[map]], nd = nd
      )
    }
  } else {
    result <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('geos')) %oper% {
      ret <- vector('numeric', nd)

      for (i in 1:nd) {
        united <- geox_union(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
        area <- sum(areas[plans[, map] == dists[i]])

        perim <- sum(geos::geos_length(united))

        ret[i] <- 1 / (perim / (2 * pi * sqrt(area / pi)))
      }

      ret
    }
  }

  c(result)
}

#' Calculate Reock Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_reock(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' comp_reock(plans = nh_m[, 3:5], shp = nh)
#'
comp_reock <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  # compute ----
  areas <- geos::geos_area(shp)
  out <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('geos')) %oper% {
    ret <- vector('numeric', nd)

    for (i in 1:nd) {
      united <- geox_union(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
      area <- sum(areas[plans[, map] == dists[i]])

      mbc <- geos::geos_area(geos::geos_minimum_bounding_circle(united))
      ret[i] <- area / mbc
    }

    ret
  }

  out
}

#' Calculate Convex Hull Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_ch(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' comp_ch(plans = nh_m[, 3:5], shp = nh)
#'
comp_ch <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  # compute ----
  areas <- geos::geos_area(shp)
  out <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('geos')) %oper% {
    ret <- vector('numeric', nd)

    for (i in 1:nd) {
      united <- geox_union(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
      area <- sum(areas[plans[, map] == dists[i]])

      cvh <- geos::geos_area(geos::geos_convex_hull(united))
      ret[i] <- area / cvh
    }

    ret
  }

  out
}

#' Calculate Length Width Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_lw(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' \donttest{
#' # slower, beware!
#' comp_lw(plans = nh_m[, 3:5], shp = nh)
#' }
#'
comp_lw <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  shp <- geos::as_geos_geometry(shp)
  # compute ----
  bboxes <- as.matrix(geos::geos_envelope_rct(nh))
  result <- foreach::foreach(map = 1:n_plans, .combine = 'cbind') %oper% {
    out <- numeric(nd)
    for (i in 1:nd) {
      idx <- plans[, map] == dists[i]
      xdiff <- max(bboxes[idx, 3]) - min(bboxes[idx, 1])
      ydiff <- max(bboxes[idx, 4]) - min(bboxes[idx, 2])
      out[i] <- if (xdiff < ydiff) xdiff / ydiff else ydiff / xdiff
    }
    out
  }

  c(result)
}

#' Calculate Boyce Clark Ratio
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_bc(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' \donttest{
#' # slower, beware!
#' comp_bc(plans = nh_m[, 3:5], shp = nh)
#' }
#'
comp_bc <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  epsg <- sf::st_crs(shp)$epsg
  shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1) {
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods = FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  # compute ----
  result <- foreach::foreach(map = 1:n_plans, .combine = 'cbind', .packages = c('sf')) %oper% {
    out <- numeric(nd)

    for (i in 1:nd) {
      united <- geox_union(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
      center <- geos::geos_centroid(united)
      if (!geos::geos_within(united, center)) {
        center <- geos::geos_point_on_surface(united)
      }
      center <- geox_coordinates(center)
      bbox <- as.numeric(geos::geos_envelope_rct(united))
      max_dist <- sqrt((bbox[4] - bbox[2])^2 + (bbox[3] - bbox[1])^2)

      x_list <- center[1] + max_dist * cos(seq(0, 15) * pi / 8)
      y_list <- center[2] + max_dist * sin(seq(0, 15) * pi / 8)
      radials <- rep(NA_real_, 16)
      for (angle in 1:16) {
        line <- geos::geos_make_linestring(x = c(x_list[angle], center[1]), c(y_list[angle], center[2]),
                                           crs = epsg)
        radials[angle] <- max(0, geos::geos_length(geos::geos_intersection(line, united)))
      }
      out[i] <- 1 - (sum(abs(radials / sum(radials) * 100 - 6.25)) / 200)
    }

    out
  }

  c(result)
}

#' Calulate Fryer Holden Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param total_pop A numeric vector with the population for every observation.
#' @templateVar epsg TRUE
#' @param ncores TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_fh(plans = nh$r_2020, shp = nh, total_pop = pop)
#'
#' # Or many plans:
#' comp_fh(plans = nh_m[, 3:5], shp = nh, pop)
#'
comp_fh <- function(plans, shp, total_pop, epsg = 3857, ncores = 1) {
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  total_pop <- rlang::eval_tidy(rlang::enquo(total_pop), shp)

  shp <- geos::as_geos_geometry(shp)

  centroids <- geos::geos_centroid(shp)
  dist_sqr <- geox_distance_mat(centroids)^2
  pop <- total_pop * t(matrix(rep(total_pop, length(shp)), length(shp)))
  fh <- pop * dist_sqr
  out <- apply(plans, 2, function(x) {
    sum(vapply(1:nd, function(i) {
      ind <- x == i
      sum(fh[ind, ind])
    },
    FUN.VALUE = 0
    ))
  })

  rep(out, each = nd)
}

#' Calulate Edges Removed Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar adj TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_edges_rem(plans = nh$r_2020, shp = nh, nh$adj)
#'
#' # Or many plans:
#' comp_edges_rem(plans = nh_m[, 3:5], shp = nh, nh$adj)
#'
comp_edges_rem <- function(plans, shp, adj) {
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  if (missing(adj) & inherits(shp, 'redist_map')) {
    adj <- shp[[attr(shp, 'adj_col')]]
  } else if (missing(adj)) {
    cli::cli_abort('`adj` missing and `shp` is not of class `redist_map`.')
  }

  n_removed(g = adj, districts = plans, n_distr = nd) %>%
    rep(each = nd)
}

#' Calulate Fraction Kept Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar adj TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_frac_kept(plans = nh$r_2020, shp = nh, nh$adj)
#'
#' # Or many plans:
#' comp_frac_kept(plans = nh_m[, 3:5], shp = nh, nh$adj)
#'
comp_frac_kept <- function(plans, shp, adj) {
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  if (missing(adj) & inherits(shp, 'redist_map')) {
    adj <- shp[[attr(shp, 'adj_col')]]
  } else if (missing(adj)) {
    cli::cli_abort('`adj` missing and `shp` is not of class `redist_map`.')
  }
  n_edge <- length(unlist(adj))

  (1 - (n_removed(g = adj, districts = plans, n_distr = nd) / n_edge)) %>%
    rep(each = nd)
}

#' Calulate Log Spanning Tree Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param counties column name in shp containing counties
#' @templateVar adj TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_log_st(plans = nh$r_2020, shp = nh, counties = county, adj = nh$adj)
#'
#' # Or many plans:
#' comp_log_st(plans = nh_m[, 3:5], shp = nh, counties = county, adj = nh$adj)
#'
comp_log_st <- function(plans, shp, counties = NULL, adj) {
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  counties <- rlang::eval_tidy(rlang::enquo(counties), shp)
  if (is.null(counties)) {
    counties <- rep(1L, nrow(shp))
  } else {
    counties <- make_id(counties)
  }

  if (missing(adj) & inherits(shp, 'redist_map')) {
    adj <- shp[[attr(shp, 'adj_col')]]
  } else if (missing(adj)) {
    cli::cli_abort('`adj` missing and `shp` is not of class `redist_map`.')
  }

  log_st_map(g = adj, districts = plans, counties = counties, n_distr = nd) %>%
    rep(each = nd)
}
