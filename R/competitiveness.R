#' Compute Talismanic Redistricting Competitiveness Metric
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar rvote TRUE
#' @templateVar dvote TRUE
#' @param alpha Numeric scaling value
#' @param beta Numeric scaling value
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept competitiveness
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' compet_talisman(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' compet_talisman(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
compet_talisman <- function(plans, shp, rvote, dvote, alpha = 1, beta = 1) {
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)

  # process objects ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # aggregate ---
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  talisman(dvs = dvs, nd = nd, alpha = alpha, beta = beta) %>%
    rep(each = nd)
}
