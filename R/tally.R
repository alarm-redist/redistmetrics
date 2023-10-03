#' Tally a Column by District
#'
#' Helper function to aggregate a vector by district. Can be used to calculate
#' total population, group percentages, and more.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param x The numeric vector to tally.
#' @template template_nosf
#'
#' @return A numeric vector with the tallies. Can be shaped into a
#'   district-by-plan matrix.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#'
#' tally(nh_m, nh, pop) # total population
#' tally(nh_m, nh, vap_hisp) / tally(nh_m, nh, vap) # HVAP
#'
#' @export
tally <- function(plans, shp, x) {
  plans <- process_plans(plans)
  nd <- dplyr::n_distinct(plans[, 1])
  if (max(plans[, 1]) != nd) {
    plans = reindex(plans)
  }

  # prep admin ----
  x <- rlang::eval_tidy(rlang::enquo(x), data = shp)
  if (is.null(x)) {
    cli::cli_abort("{.arg x} not found in {.arg shp}.")
  }
  if (!is.numeric(x) && !is.logical(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }
  x = as.numeric(x)

  as.numeric(tally_var(plans, x, nd))
}
