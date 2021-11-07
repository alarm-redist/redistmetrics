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
#' # todo example
comp_polsby <- function(plans, shp, use_Rcpp, perim_path, perim_df, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
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
  if(missing(perim_path) & missing(perim_df)) {
    perim_df <- prep_perims(shp = shp, epsg = epsg, ncores = ncores)
  }

  # calculate ----
  areas <- sf::st_area(shp)
  if (use_Rcpp) {
    splits <- split(x = plans, rep(1:nc, each = ceiling(n_plans/nc)*V)[1:(n_plans*V)]) %>%
      lapply(., FUN = function(x, r = V) matrix(data = x, nrow = r))

    result <- foreach::foreach(map = 1:nc, .combine = 'cbind', .packages = c('sf', 'rict')) %oper% {
      polsbypopper(from = perim_df$origin, to = perim_df$touching, area = areas,
                   perimeter = perim_df$edge, dm = splits[[map]], nd = nd)

    }
  } else {
    result <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('sf', 'lwgeom')) %oper% {
      ret <- vector('numeric', nd)

      for (i in 1:nd) {
        united <- suppressMessages(sf::st_union(shp[plans[, map] == dists[i],]))
        area <- sum(areas[plans[, map] == dists[i]])

        if(is.null(sf::st_crs(united$EPSG)) || is.na(sf::st_is_longlat(united))){
          perim <- sum(sf::st_length(sf::st_cast(sf::st_cast(united, 'POLYGON'),'LINESTRING')))
        } else {
          perim <- sum(sf::st_length(sf::st_cast(united, "MULTILINESTRING")))
        }

        ret[i] <- 4*pi*(area)/(perim)^2
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
#' # todo example
comp_schwartz <- function(plans, shp, use_Rcpp, perim_path, perim_df, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
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
  if(missing(perim_path) & missing(perim_df)) {
    perim_df <- prep_perims(shp = shp, epsg = epsg, ncores = ncores)
  }

  # calculate ----
  areas <- sf::st_area(shp)
  if (use_Rcpp) {
    splits <- split(x = plans, rep(1:nc, each = ceiling(n_plans/nc)*V)[1:(n_plans*V)]) %>%
      lapply(., FUN = function(x, r = V) matrix(data = x, nrow = r))

    result <- foreach::foreach(map = 1:nc, .combine = 'cbind', .packages = c('sf', 'rict')) %oper% {
      schwartzberg(from = perim_df$origin, to = perim_df$touching, area = areas,
                   perimeter = perim_df$edge, dm = splits[[map]], nd = nd)

    }
  } else {
    result <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('sf', 'lwgeom')) %oper% {
      ret <- vector('numeric', nd)

      for (i in 1:nd) {
        united <- suppressMessages(sf::st_union(shp[plans[, map] == dists[i],]))
        area <- sum(areas[plans[, map] == dists[i]])

        if(is.null(sf::st_crs(united$EPSG)) || is.na(sf::st_is_longlat(united))){
          perim <- sum(sf::st_length(sf::st_cast(sf::st_cast(united, 'POLYGON'),'LINESTRING')))
        } else {
          perim <- sum(sf::st_length(sf::st_cast(united, "MULTILINESTRING")))
        }

        ret[i] <- perim/(2*pi*sqrt(area/pi))
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
#' # todo example
comp_reock <- function(plans, shp, epsg = 3857, ncores = 1) {

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

  # compute ----
  areas <- sf::st_area(shp)
  out <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('sf', 'lwgeom')) %oper% {
    ret <- vector('numeric', nd)

    for (i in 1:nd) {
      united <- sf::st_union(shp[plans[, map] == dists[i], ])
      area <- sum(areas[plans[, map] == dists[i]])

      mbc <- sf::st_area(lwgeom::st_minimum_bounding_circle(united))
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
#' # todo example
comp_ch <- function(plans, shp, epsg = 3857, ncores = 1) {

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

  # compute ----
  areas <- sf::st_area(shp)
  out <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('sf', 'lwgeom')) %oper% {
    ret <- vector('numeric', nd)

    for (i in 1:nd) {
      united <- sf::st_union(shp[plans[, map] == dists[i], ])
      area <- sum(areas[plans[, map] == dists[i]])

      cvh <- sf::st_area(sf::st_convex_hull(united))
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
#' # todo example
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

  # compute ----
  bboxes <- as.data.frame(do.call(rbind, lapply(sf::st_geometry(shp), sf::st_bbox)))
  result <- foreach::foreach(map = 1:n_plans, .combine = 'cbind', .packages = c('sf')) %oper% {
    out <- numeric(nd)
    for (i in 1:nd) {
      idx <- plans[, map] == dists[i]
      xdiff <- max(bboxes$xmax[idx]) - min(bboxes$xmin[idx])
      ydiff <- max(bboxes$ymax[idx]) - min(bboxes$ymin[idx])
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
#' # todo example
comp_bc <- function(plans, shp, epsg = 3857, ncores = 1) {

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

  # compute ----
  result <- foreach::foreach(map = 1:n_plans, .combine = 'cbind', .packages = c('sf')) %oper% {
    out <- numeric(nd)

    for (i in 1:nd) {
      united <- sf::st_union(shp[plans[, map] == dists[i], ])
      suppressWarnings(center <- sf::st_centroid(united))
      suppressMessages(suppressWarnings(if(!sf::st_within(united,center,sparse=FALSE)[[1]]){
        suppressWarnings(center <- sf::st_point_on_surface(united))
      }))
      center <- sf::st_coordinates(center)
      bbox <- sf::st_bbox(united)
      max_dist <- sqrt((bbox$ymax-bbox$ymin)^2+(bbox$xmax-bbox$xmin)^2)
      sf::st_crs(united) <- NA

      x_list <- center[1] + max_dist*cos(seq(0,15)*pi/8)
      y_list <- center[2] + max_dist*sin(seq(0,15)*pi/8)
      radials <- rep(NA_real_, 16)
      for(angle in 1:16){
        line <- data.frame(x = c(x_list[angle],center[1]), y = c(y_list[angle], center[2])) %>%
          sf::st_as_sf(coords = c('x','y'))  %>% sf::st_coordinates() %>% sf::st_linestring()
        radials[angle] <- max(0, stats::dist(sf::st_intersection(line, united)))
      }
      out[i] <- 1 - (sum(abs(radials/sum(radials)*100-6.25))/200)
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
#' # todo example
comp_fh <- function(plans, shp, total_pop, epsg = 3857, ncores = 1) {
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)


  centroids <- sf::st_geometry(sf::st_centroid(shp))
  dist_sqr <- sf::st_distance(centroids, centroids)^2
  pop <- total_pop * t(matrix(rep(total_pop, nrow(shp)), nrow(shp)))
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
#' # todo example
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
#' # todo example
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

  (1 - (n_removed(g = adj, districts = plans, n_distr = nd)/n_edge)) %>%
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
#' # todo example
comp_log_st <- function(plans, shp, counties, adj) {

  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  counties <- rlang::eval_tidy(rlang::enquo(counties), shp)

  if (missing(adj) & inherits(shp, 'redist_map')) {
    adj <- shp[[attr(shp, 'adj_col')]]
  } else if (missing(adj)) {
    cli::cli_abort('`adj` missing and `shp` is not of class `redist_map`.')
  }

  log_st_map(g = adj, districts = plans, counties = counties, n_distr = nd) %>%
    rep(each = nd)
}


