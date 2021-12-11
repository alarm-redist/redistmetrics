#' Count Incumbent Pairings
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar inc TRUE
#' @template template
#'
#' @return
#' @export
#' @concept incumbent
#'
#' @examples
#' # todo example
inc_pairs <- function(plans, shp, inc) {
  plans <- process_plans(plans)

  m <- plans[inc, ]

  sum(inc) - apply(m, 2, \(x) length(unique(x)))
}
