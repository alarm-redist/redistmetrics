#' Count Incumbent Pairings
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar inc TRUE
#' @template template
#'
#' @return vector of number of incumbents paired
#' @export
#' @concept incumbent
#'
#' @examples
#' # todo example
inc_pairs <- function(plans, shp, inc) {
  # process objects ----
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  inc <- rlang::eval_tidy(rlang::enquo(inc), shp)

  m <- plans[inc, , drop = FALSE]

  rep(sum(inc) - apply(m, 2, function(x) length(unique(x))), each = nd)
}
