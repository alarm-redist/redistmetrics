#' Compute Talismanic Redistricting Competitiveness Metric
#'
#' @param plans todo inherits
#' @param rvote todo inherits
#' @param dvote todo inherits
#' @param alpha Numeric scaling value
#' @param beta Numeric scaling value
#'
#' @return numeric vector
#' @export
#' @concept competitiveness
#'
#' @examples
#' # todo examples
compet_talisman <- function(plans, rvote, dvote, alpha = 1, beta = 1) {
  # process objects ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # aggregate ---
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  talisman(dvs = dvs, nd = nd, alpha = alpha, beta = beta)
}
