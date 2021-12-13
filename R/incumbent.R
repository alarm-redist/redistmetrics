#' Count Incumbent Pairings
#'
#' Count the number of incumbents paired with at least one other incumbent.
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
#' data(nh)
#' data(nh_m)
#' # Use incumbent data:
#' fake_inc <- rep(FALSE, nrow(nh))
#' fake_inc[3:4] <- TRUE
#'
#' # For a single plan:
#' inc_pairs(plans = nh$r_2020, shp = nh, inc = fake_inc)
#'
#' # Or many plans:
#' inc_pairs(plans = nh_m[, 3:5], shp = nh, inc = fake_inc)
#'
inc_pairs <- function(plans, shp, inc) {
  # process objects ----
  plans <- process_plans(plans)
  dists <- sort(unique(c(plans)))
  nd <- length(dists)

  inc <- rlang::eval_tidy(rlang::enquo(inc), shp)

  m <- plans[inc, , drop = FALSE]

  rep(sum(inc) - apply(m, 2, function(x) length(unique(x))), each = nd)
}
