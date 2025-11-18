#' Calculate Polsby Popper Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param use_Rcpp If `TRUE` (the default for more than 8 plans), precompute boundaries
#'   shared by each pair of units and use them to quickly compute the compactness score.
#' @param perim_path Path to perimeter tibble saved by `prep_perims()`
#' @param perim_df Tibble of perimeters from `prep_perims()`
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Cox, E. 1927. A Method of Assigning Numerical and Percentage Values to the
#' Degree of Roundness of Sand Grains. Journal of Paleontology, 1(3), 179-183.
#'
#' Polsby, Daniel D., and Robert D. Popper. 1991. “The Third Criterion:
#' Compactness as a procedural safeguard against partisan gerrymandering.”
#' Yale Law & Policy Review 9 (2): 301–353.
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
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  if (use_Rcpp) {
    if (missing(perim_df)) {
      if (missing(perim_path)) {
        perim_df <- prep_perims(shp = shp, epsg = epsg, ncores = ncores)
      } else {
        perim_df <- readRDS(perim_path)
      }
    }
  }

  # calculate ----
  areas <- geos::geos_area(shp)
  if (use_Rcpp) {
    splits <- split(x = plans, rep(seq_len(nc), each = ceiling(n_plans / nc) * V)[seq_len(n_plans * V)]) %>%
      lapply(., FUN = function(x, r = V) matrix(data = x, nrow = r))

    result <- foreach::foreach(map = seq_len(nc), .combine = 'cbind', .packages = c('redistmetrics'),
                               .export = 'polsbypopper') %oper% {
      polsbypopper(
        from = perim_df$origin, to = perim_df$touching, area = areas,
        perimeter = perim_df$edge, dm = splits[[map]], nd = nd
      )
    }
  } else {
    result <- foreach::foreach(map = seq_len(n_plans), .combine = 'c', .packages = c('geos', 'redistmetrics'),
                               .export = 'geox_union') %oper% {
      ret <- vector('numeric', nd)

      for (i in seq_len(nd)) {
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
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Schwartzberg, Joseph E. 1966. Reapportionment, Gerrymanders, and the Notion
#' of Compactness. Minnesota Law Review. 1701.
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
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  if (use_Rcpp) {
    if (missing(perim_df)) {
      if (missing(perim_path)) {
        perim_df <- prep_perims(shp = shp, epsg = epsg, ncores = ncores)
      } else {
        perim_df <- readRDS(perim_path)
      }
    }
  }

  # calculate ----
  areas <- geos::geos_area(shp)
  if (use_Rcpp) {
    splits <- split(x = plans, rep(seq_len(nc), each = ceiling(n_plans / nc) * V)[seq_len(n_plans * V)]) %>%
      lapply(., FUN = function(x, r = V) matrix(data = x, nrow = r))

    result <- foreach::foreach(map = seq_len(nc), .combine = 'cbind', .packages = c('redistmetrics'),
                               .export = 'schwartzberg') %oper% {
      schwartzberg(
        from = perim_df$origin, to = perim_df$touching, area = areas,
        perimeter = perim_df$edge, dm = splits[[map]], nd = nd
      )
    }
  } else {
    result <- foreach::foreach(map = seq_len(n_plans), .combine = 'c', .packages = c('geos'),
                               .export = 'geox_union') %oper% {
      ret <- vector('numeric', nd)

      for (i in seq_len(nd)) {
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
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Reock, E. 1961. A Note: Measuring Compactness as a Requirement of Legislative
#' Apportionment. Midwest Journal of Political Science, 5(1), 70-74.
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
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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

  area_mat <- agg_p2d(plans, vote = areas, nd = nd)
  out <- foreach::foreach(
    map = seq_len(n_plans), .combine = 'c', .packages = c('geos')
  ) %oper% {
    ret <- vector('numeric', nd)

    for (i in seq_len(nd)) {
      united <- geos::geos_make_collection(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
      mbc <- geos::geos_area(geos::geos_minimum_bounding_circle(united))
      ret[i] <- mbc
    }

    ret
  }

  c(area_mat) / out
}

comp_reock_cpp <- function(plans, shp, epsg = 3857, ncores = 1) {
  # process objects ----
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  if (ncol(plans) == 0) {
    return(numeric(0))
  }
  nd <- vctrs::vec_unique_count(plans[, 1])

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
  area_mat <- agg_p2d(plans, vote = areas, nd = nd)

  if (nc == 1) {
    chunks <- rep(1L, ncol(plans))
  } else {
    chunks <- cut(seq_len(ncol(plans)), nc, labels = FALSE)
  }

  plan_chunks <- lapply(seq_len(max(chunks)), function(x) {
    plans[, chunks == x, drop = FALSE]
  })

  shp_col_wkt <- geos::as_geos_geometry(shp) |>
    geos::geos_make_collection() |>
    geos::geos_write_wkt()

  out <- foreach::foreach(
    map = seq_along(plan_chunks), .packages = c('redistmetrics'),
    .export = 'compute_mbc_area', .combine = 'c'
  ) %oper% {
    c(compute_mbc_area(shp_col_wkt, plan_chunks[[map]], nd))
  }

  c(area_mat) / c(out)
}

#' Calculate Convex Hull Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
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
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  out <- foreach::foreach(map = seq_len(n_plans), .combine = 'c', .packages = c('geos'),
                          .export = 'geox_union') %oper% {
    ret <- vector('numeric', nd)

    for (i in seq_len(nd)) {
      united <- geos::geos_make_collection(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
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
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Harris, Curtis C. 1964. “A scientific method of districting”.
#' Behavioral Science 3(9), 219–225.
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
  bboxes <- as.matrix(geos::geos_envelope_rct(shp))
  result <- foreach::foreach(map = seq_len(n_plans), .combine = 'cbind') %oper% {
    out <- numeric(nd)
    for (i in seq_len(nd)) {
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
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Boyce, R., & Clark, W. 1964. The Concept of Shape in Geography.
#' Geographical Review, 54(4), 561-572.
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
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  result <- foreach::foreach(map = seq_len(n_plans), .combine = 'cbind', .packages = c('geos'),
                             .export = c('geox_union', 'geox_coordinates')) %oper% {
    out <- numeric(nd)

    for (i in seq_len(nd)) {
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

#' Calculate Fryer Holden Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param total_pop A numeric vector with the population for every observation.
#' @templateVar epsg TRUE
#' @param ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Fryer R, Holden R. 2011. Measuring the Compactness of Political Districting Plans.
#' Journal of Law and Economics.
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
    sum(vapply(seq_len(nd), function(i) {
      ind <- x == i
      sum(fh[ind, ind])
    },
    FUN.VALUE = 0
    ))
  })

  rep(out, each = nd)
}

#' Calculate Edges Removed Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar adj TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Matthew P. Dube and Jesse Tyler Clark. 2016.
#' Beyond the circle: Measuring district compactness using graph theory. In
#' Annual Meeting of the Northeastern Political Science Association
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
    cli::cli_abort('{.arg adj} missing and {.arg shp} is not a {.cls redist_map}.')
  }

  rep(n_removed(g = adj, districts = plans, n_distr = nd), each=nd)
}

#' Calculate Fraction Kept Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar adj TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Matthew P. Dube and Jesse Tyler Clark. 2016.
#' Beyond the circle: Measuring district compactness using graph theory. In
#' Annual Meeting of the Northeastern Political Science Association
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
    cli::cli_abort('{.arg adj} missing and {.arg shp} is not a {.cls redist_map}.')
  }
  n_edge <- length(unlist(adj))

  rep(1 - (n_removed(g = adj, districts = plans, n_distr = nd) / n_edge), each=nd)
}

#' Calculate Log Spanning Tree Compactness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param counties column name in shp containing counties
#' @templateVar adj TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Cory McCartan and Kosuke Imai. 2020.
#' Sequential Monte Carlo for Sampling Balanced and Compact Redistricting Plans.
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
    cli::cli_abort('{.arg adj} missing and {.arg shp} is not a {.cls redist_map}.')
  }

  rep(log_st_map(g = adj, districts = plans, counties = counties, n_distr = nd), each=nd)
}

#' Calculate Skew Compactness
#'
#' Skew is defined as the ratio of the radii of the largest inscribed circle with
#' the smallest bounding circle. Scores are bounded between 0 and 1, where 1 is
#' most compact.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' S.N. Schumm. 1963. Sinuosity of alluvial rivers on the Great Plains.
#' Bulletin of the Geological Society of America, 74. 1089-1100.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_skew(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' \donttest{
#' # slower, beware!
#' comp_skew(plans = nh_m[, 3:5], shp = nh)
#' }
comp_skew <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  result <- foreach::foreach(map = seq_len(n_plans), .combine = 'cbind', .packages = c('geos'),
                             .export = 'geox_union') %oper% {
    out <- numeric(nd)

    for (i in seq_len(nd)) {
      united <- geox_union(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
      w <- sqrt(geos::geos_area(geos::geos_maximum_inscribed_crc(united, tolerance = 0.01)))
      l <- sqrt(geos::geos_area(geos::geos_minimum_bounding_circle(united)))
      out[i] <- w / l
    }

    out
  }

  c(result)
}

#' Calculate Box Reock Compactness
#'
#' Box reock is the ratio of the area of the district by the area of the minimum
#' bounding box (of any rotation). Scores are bounded between 0 and 1, where 1 is
#' most compact.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @examples
#' #' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_box_reock(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' \donttest{
#' # slower, beware!
#' comp_box_reock(plans = nh_m[, 3:5], shp = nh)
#' }
comp_box_reock <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  out <- foreach::foreach(map = seq_len(n_plans), .combine = 'c', .packages = c('geos'),
                          .export = 'geox_union') %oper% {
    ret <- vector('numeric', nd)

    for (i in seq_len(nd)) {
      united <- geos::geos_make_collection(geos::geos_geometry_n(shp_col, which(plans[, map] == dists[i])))
      area <- sum(areas[plans[, map] == dists[i]])

      mbc <- geos::geos_area(geos::geos_minimum_rotated_rectangle(united))
      ret[i] <- area / mbc
    }

    ret
  }

  out
}

#' Calculate Bounding Box Reock Compactness
#'
#' Box reock is the ratio of the area of the district by the area of the minimum
#' bounding box (of fixed rotation). Scores are bounded between 0 and 1, where 1 is
#' most compact.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @examples
#' #' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_bbox_reock(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' comp_bbox_reock(plans = nh_m[, 1:5], shp = nh)
comp_bbox_reock <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  if (ncores > 1) {
    shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
  } else {
    shp_col <- geos::geos_make_collection(geos::as_geos_geometry(shp))
  }
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
  extents <- geos::geos_extent(shp) |>
    as.matrix()
  if (nc == 1) {
    chunks <- rep(1L, ncol(plans))
  } else {
    chunks <- cut(seq_len(ncol(plans)), nc, labels = FALSE)
  }

  plan_chunks <- lapply(seq_len(max(chunks)), function(x) {
    plans[, chunks == x, drop = FALSE]
  })
  out <- foreach::foreach(
    map = seq_along(plan_chunks),
    .combine = 'cbind', .packages = c('redistmetrics'),
    .export = 'bbox_reock'
  ) %oper% {
    bbox_reock(dm = plan_chunks[[map]], areas = areas, extents = extents, nd = nd)
  }

  c(out)
}

#' Calculate Y Symmetry Compactness
#'
#' Y symmetry is the overlapping area of a shape and its projection over the
#' y-axis.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Aaron Kaufman, Gary King, and Mayya Komisarchik. 2021.
#' How to Measure Legislative District Compactness If You Only Know it When You See It.
#' American Journal of Political Science. 65, 3. Pp. 533-550.
#'
#' @examples
#' #' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_y_sym(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' \donttest{
#' # slower, beware!
#' comp_y_sym(plans = nh_m[, 3:5], shp = nh)
#' }
#'
comp_y_sym <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg) %>% sf::st_geometry()
  epsg <- sf::st_crs(shp)$epsg
  # shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
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
  coords <- geox_coordinates(geos::geos_centroid(shp))
  out <- foreach::foreach(map = seq_len(n_plans), .combine = 'c', .packages = c('geos'),
                          .export = c('geox_union', 'geox_sub_centroid')) %oper% {
    ret <- vector('numeric', nd)

    for (i in seq_len(nd)) {
      w_prec <- which(plans[, map] == dists[i])
      # find centroid
      cent <- geox_sub_centroid(coords, areas, w_prec)

      # center at 0, 0
      #coords_ctr <- coords[w_prec, ] - cent
      # need to do affine transformations in sf for now?
      shp_ctr <- sf::st_union(shp[w_prec] - cent)
      coords_ctr <- sf::st_coordinates(shp_ctr)
      shp_refl <- coords_ctr[, 1:2]

      if (!all(shp_refl[1, ] == shp_refl[nrow(shp_refl), ])) {
        shp_refl[, 1] <- shp_refl[, 1] * -1
        shp_refl <- sf::st_sfc(sf::st_polygon(
          lapply(unique(coords_ctr[, 3]), function(x) shp_refl[coords_ctr[, 3] == x, ])
        ))
      } else {
        shp_refl[, 1] <- shp_refl[, 1] * -1
        shp_refl <- sf::st_sfc(sf::st_polygon(list(shp_refl)))
      }

      # Reflect over x = 0 and make shapes
      # the idea, but needs to be the actual shapes, not the centers:
      #refl_united <- geox_union(geos::geos_make_polygon(x = coords_ctr[, 1] * -1,
      #                                                  y = coords_ctr[, 2],
      #                                                  crs = epsg))
      # united <- geox_union(geos::geos_make_polygon(x = coords_ctr[, 1],
      #                                                  y = coords_ctr[, 2],
      #                                                  crs = epsg))

      # Intersect
      #ovlap <- geos::geos_intersection(united, refl_united)
      ovlap <- geos::geos_intersection(shp_ctr, shp_refl)

      # Compute area
      ret[i] <- sum(geos::geos_area(ovlap)) / sum(areas[w_prec])
    }

    ret
  }

  out
}

#' Calculate X Symmetry Compactness
#'
#' X symmetry is the overlapping area of a shape and its projection over the
#' x-axis.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Aaron Kaufman, Gary King, and Mayya Komisarchik. 2021.
#' How to Measure Legislative District Compactness If You Only Know it When You See It.
#' American Journal of Political Science. 65, 3. Pp. 533-550.
#'
#' @examples
#' #' data(nh)
#' data(nh_m)
#' # For a single plan:
#' comp_x_sym(plans = nh$r_2020, shp = nh)
#'
#' # Or many plans:
#' \donttest{
#' # slower, beware!
#' comp_x_sym(plans = nh_m[, 3:5], shp = nh)
#' }
#'
comp_x_sym <- function(plans, shp, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg) %>% sf::st_geometry()
  epsg <- sf::st_crs(shp)$epsg
  # shp_col <- wk::as_wkt(geos::geos_make_collection(geos::as_geos_geometry(shp)))
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
  coords <- geox_coordinates(geos::geos_centroid(shp))

  ## experimental:
  #all_pts <- wk::wk_coords(shp)

  out <- foreach::foreach(map = seq_len(n_plans), .combine = 'c', .packages = c('geos'),
                          .export = c('geox_union', 'geox_sub_centroid')) %oper% {
    ret <- vector('numeric', nd)

    for (i in seq_len(nd)) {
      w_prec <- which(plans[, map] == dists[i])

      # # experimental:
      # w_feat <- which(all_pts$feature_id %in% w_prec)
      # find centroid
      cent <- geox_sub_centroid(coords, areas, w_prec)

      # center at 0, 0
      shp_ctr <- sf::st_union(shp[w_prec] - cent)
      coords_ctr <- sf::st_coordinates(shp_ctr)
      shp_refl <- coords_ctr[, 1:2]

      if (!all(shp_refl[1, ] == shp_refl[nrow(shp_refl), ])) {
        shp_refl[, 2] <- shp_refl[, 2] * -1
        shp_refl <- sf::st_sfc(sf::st_polygon(
          lapply(unique(coords_ctr[, 3]), function(x) shp_refl[coords_ctr[, 3] == x, ])
        ))
      } else {
        shp_refl[, 2] <- shp_refl[, 2] * -1
        shp_refl <- sf::st_sfc(sf::st_polygon(list(shp_refl)))
      }

      # # experimental:
      # # center at 0,0
      # shp_ctr <- geox_union(geos::geos_make_valid(geos::geos_make_polygon(
      #   x = all_pts$x[w_feat] - cent[1], y = all_pts$y[w_feat] - cent[2],
      #   feature_id = all_pts$feature_id[w_feat], ring_id = all_pts$ring_id[w_feat],
      #   crs = epsg
      # )))
      #
      # # center at 0,0 and reflect
      # shp_refl <- geox_union(geos::geos_make_valid(geos::geos_make_polygon(
      #   x = all_pts$x[w_feat] - cent[1], y = -1 * (all_pts$y[w_feat] - cent[2]),
      #   feature_id = all_pts$feature_id[w_feat], ring_id = all_pts$ring_id[w_feat],
      #   crs = epsg
      # )))

      # Intersect
      ovlap <- geos::geos_intersection(shp_ctr, shp_refl)

      # Compute area
      ret[i] <- sum(geos::geos_area(ovlap)) / sum(areas[w_prec])
    }

    ret
  }

  out
}
