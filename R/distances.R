#' Calculate Variation of Information Distances
#'
#' @templateVar plans TRUE
#' @templateVar total_pop TRUE
#' @templateVar ncores TRUE
#' @template template
#'
#' @return matrix of plan distances
#' @export
#' @concept distances
#'
#' @examples
#' # todo example
dist_info <- function(plans, total_pop, ncores = 1) {

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

  if (is.null(total_pop)) {
    cli::cli_warn('`total_pop` not provided, using default of equal population.')
    total_pop <- rep(1, nrow(plans))
  }
  if (length(total_pop) != nrow(plans)) {
    cli::cli_abort('Length of `total_pop` does not match the number of rows in `plans`.')
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
#' # todo example
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
#' # todo example
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
#' # todo example
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
