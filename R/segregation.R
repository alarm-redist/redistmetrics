#' Compute Dissimilarity Index
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar group_pop TRUE
#' @templateVar total_pop TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept segregation
#'
#' @references
#' Douglas Massey and Nancy Denton. 1987.
#' The Dimensions of Social Segregation. Social Forces.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' seg_dissim(plans = nh$r_2020, shp = nh, group_pop = vap_hisp, total_pop = vap)
#'
#' # Or many plans:
#' seg_dissim(plans = nh_m[, 3:5], shp = nh, group_pop = vap_hisp, total_pop = vap)
#'
seg_dissim <- function(plans, shp, group_pop, total_pop) {
  # process inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))
  group_pop <- rlang::eval_tidy(rlang::enquo(group_pop), data = shp)
  total_pop <- rlang::eval_tidy(rlang::enquo(total_pop), data = shp)
  if (is.null(group_pop)) {
    cli::cli_abort('{.arg group_pop} not found in {.arg shp}.')
  }
  if (is.null(total_pop)) {
    cli::cli_abort('{.arg total_pop} not found in {.arg shp}.')
  }

  segregationcalc(plans, group_pop, total_pop) %>%
    rep(each = nd)
}
