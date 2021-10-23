#' Calculate Partisan Bias
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#' @param v vote share to calculate bias at. Numeric. Default is 0.5.
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_bias <- function(plans, dvote, rvote, v = 0.5) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }


  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  erp(biasatv(dvs = dvs, v = v, nd = nd), each = nd)
}

#' Calculate Democratic Seats
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_dseats <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }


  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)

  rep(dseat_vec, each = nd)
}


#' Calculate Democratic Vote Share
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_dvs <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  c(DVS(dcounts = dcounts, rcounts = rcounts))
}


#' Calculate Efficiency Gap
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_egap <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  totvote <- sum(rvote) + sum(dvote)
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)


  rep(effgap(dcounts = dcounts, rcounts = rcounts, totvote = totvote), each = nd)
}

#' Calculate Efficiency Gap (Equal Population Assumption)
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_egap_ep <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(effgapEP(dvs = dvs, dseat_vec = dseat_vec, nd = nd), each = nd)
}

#' Calculate Tau Gap
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#' @param tau A non-negative numeric for calculating Tau Gap. Defaults to 1.
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_tau_gap <- function(plans, dvote, rvote, tau = 1) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(taugap(tau = tau, dvs = dvs, dseat_vec = dseat_vec, nd = nd), each = nd)
}

#' Calculate Mean Median Score
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_mean_median <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(meanmedian(dvs = dvs), each = nd)
}

#' Calculate Declination
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#' @param normalize Default is FALSE. Translate score to an angle?
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_decl <- function(plans, dvote, rvote, normalize = FALSE) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  dec <- rep(declination(dvs = dvs, dseat_vec = dseat_vec, nd = nd), each = nd)

  if (normalize) {
    dec <- atan(dec)/log(nd)
  }

  dec
}

#' Calculate Responsiveness
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#' @param bandwidth Defaults to 0.01. A value between 0 and 1 for the step size to estimate the slope.
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_resp <- function(plans, dvote, rvote, bandwidth = FALSE) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(responsiveness(dvs = dvs, v = respV, nd = nd, bandwidth = bandwidth), each = nd)
}

#' Calculate Lopsided Wins
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_lopsided_wins <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(lopsidedwins(dvs = dvs, dseat_vec = dseat_vec, nd = nd), each = nd)
}

#' Calculate Ranked Margin Deviation
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_rmd <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(RankedMarginalDev(dvs = dvs), each = nd)
}

#' Calculate Smoothed Seat Count Deviation
#'
#' @param plans todo inherits
#' @param dvote todo inherits
#' @param rvote todo inherits
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_sscd <- function(plans, dvote, rvote) {

  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if(!is.numeric(plans)){
    stop('`plans` should be a numeric vector or matrix.')
  }
  if(!is.matrix(plans)){
    plans <- as.matrix(plans)
  }
  if(any(is.na(plans))){
    stop('NA value in argument to `plans`.')
  }
  if(any(is.na(dvote))){
    stop('NA value in argument to `dvote`.')
  }
  if(length(rvote) != nrow(plans)){
    stop('`rvote` length and `plans` rows are not equal.')
  }
  if(length(dvote) != nrow(plans)){
    stop('`dvote` length and `plans` rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(smoothseat(dvs = dvs, nd = nd), each = nd)
}
