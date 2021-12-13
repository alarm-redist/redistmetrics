#' Calculate Variation of Information Distances
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar total_pop TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return matrix of plan distances
#' @export
#' @concept distances
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan (distance is trivial, 0):
#' dist_info(plans = nh$r_2020, shp = nh, total_pop = pop)
#'
#' # Or many plans:
#' dist_info(plans = nh_m[, 3:5], shp = nh, total_pop = pop)
#'
dist_info <- function(plans, shp, total_pop, ncores = 1) {

  # process objects ----
  plans <- process_plans(plans)
  total_pop <- rlang::eval_tidy(rlang::enquo(total_pop), shp)

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

  if (is.null(total_pop)) {
    cli::cli_warn('{.arg total_pop} not provided, using default of equal population.')
    total_pop <- rep(1, nrow(plans))
  }
  if (length(total_pop) != nrow(plans)) {
    cli::cli_abort('Length of {.arg total_pop} does not match the number of rows in {.arg plans}.')
  }

  vi <- foreach::foreach(map = 1:ncol(plans), .combine = 'cbind') %oper% {
    var_info_mat(plans, map - 1, total_pop)
  }
  colnames(vi) <- NULL
  # copy over other half of matrix; we only computed upper triangle
  vi[lower.tri(vi)] <- t(vi)[lower.tri(vi)]

  vi
}

#' Calculate Hamming Distances
#'
#' @templateVar plans TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return matrix of plan distances
#' @export
#' @concept distances
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan (distance is trivial, 0):
#' dist_ham(plans = nh$r_2020)
#'
#' # Or many plans:
#' dist_ham(plans = nh_m[, 3:5])
#'
dist_ham <- function(plans, ncores = 1) {

  # process objects ----
  plans <- process_plans(plans)

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


  ham <- foreach::foreach(map = 1:ncol(plans), .combine = 'cbind') %oper% {
    hamming(v = plans[, map], m = plans)
  }
  colnames(ham) <- NULL

  ham
}

#' Calculate Euclidean Distances
#'
#' @templateVar plans TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return matrix of plan distances
#' @export
#' @concept distances
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan (distance is trivial, 0):
#' dist_euc(plans = nh$r_2020)
#'
#' # Or many plans:
#' dist_euc(plans = nh_m[, 3:5])
#'
dist_euc <- function(plans, ncores = 1) {

  # process objects ----
  plans <- process_plans(plans)

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


  euc <- foreach::foreach(map = 1:ncol(plans), .combine = 'cbind') %oper% {
    minkowski(v = plans[, map], m = plans, p = 2)
  }
  colnames(euc) <- NULL

  euc
}

#' Calculate Manhattan Distances
#'
#' @templateVar plans TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return matrix of plan distances
#' @export
#' @concept distances
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan (distance is trivial, 0):
#' dist_man(plans = nh$r_2020)
#'
#' # Or many plans:
#' dist_man(plans = nh_m[, 3:5])
#'
dist_man <- function(plans, ncores = 1) {

  # process objects ----
  plans <- process_plans(plans)

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


  man <- foreach::foreach(map = 1:ncol(plans), .combine = 'cbind') %oper% {
    minkowski(v = plans[, map], m = plans, p = 1)
  }
  colnames(man) <- NULL

  man
}
