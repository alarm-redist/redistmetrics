## usethis namespace: start
#' @useDynLib redistmetrics, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @importFrom libgeos libgeos_version
## usethis namespace: end
NULL

globalVariables(c(
  'map', '.', 'united', 'plans', 'from', 'edge',
  'origin', 'perim_full', 'perim_adj', 'perim_boundary',
  'X1', 'touching'
))

#' @importFrom rlang .data
NULL
