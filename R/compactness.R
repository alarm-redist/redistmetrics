#' Calculate Reock Compactness
#'
#' @param shp todo inherits
#' @param plans todo inherits
#' @param epsg todo inherits
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' # todo example
comp_reock <- function(shp, plans, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <-  length(dists)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1){
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods=FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  # compute ----
  areas <- sf::st_area(shp)
  out <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('sf', 'lwgeom')) %oper% {
    ret <- vector('numeric', nd)

    for (i in 1:nd) {
      united <- sf::st_union(shp[plans[, map] == dists[i],])
      area <- sum(areas[plans[, map] == dists[i]])

      mbc <- sf::st_area(lwgeom::st_minimum_bounding_circle(united))
      ret[i] <- area/mbc
    }

    ret
  }

  out
}

#' Calculate Convex Hull Compactness
#'
#' @param shp todo inherits
#' @param plans todo inherits
#' @param epsg todo inherits
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' # todo example
comp_ch <- function(shp, plans, epsg = 3857, ncores = 1) {

  # process objects ----
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  n_plans <- ncol(plans)
  dists <- sort(unique(c(plans)))
  nd <-  length(dists)

  # set up parallel ----
  nc <- min(ncores, ncol(plans))
  if (nc == 1){
    `%oper%` <- foreach::`%do%`
  } else {
    `%oper%` <- foreach::`%dopar%`
    cl <- parallel::makeCluster(nc, setup_strategy = 'sequential', methods=FALSE)
    doParallel::registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }

  # compute ----
  areas <- sf::st_area(shp)
  out <- foreach::foreach(map = 1:n_plans, .combine = 'c', .packages = c('sf', 'lwgeom')) %oper% {
    ret <- vector('numeric', nd)

    for (i in 1:nd) {
      united <- sf::st_union(shp[plans[, map] == dists[i],])
      area <- sum(areas[plans[, map] == dists[i]])

      cvh <- sf::st_area(sf::st_convex_hull(united))
      ret[i] <- area/cvh
    }

    ret
  }

  out
}

#' Calulate Fryer Holden Compactness
#'
#' @param shp todo inherits
#' @param plans todo inherits
#' @param total_pop A numeric vector with the population for every observation.
#' @param epsg todo inherits
#'
#' @return numeric vector
#' @export
#' @concept compactness
#'
#' @examples
#' # todo example
comp_fh <- function(shp, plans, total_pop, epsg = 3857) {
  shp <- planarize(shp, epsg)
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <-  length(dists)


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
