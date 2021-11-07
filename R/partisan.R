#' Calculate Partisan Bias
#'
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param v vote share to calculate bias at. Numeric. Default is 0.5.
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_bias <- function(plans, dvote, rvote, v = 0.5) {

  plans <- process_plans(plans)
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

  rep(biasatv(dvs = dvs, v = v, nd = nd), each = nd)
}

#' Calculate Democratic Seats
#'
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_dseats <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_dvs <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_egap <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_egap_ep <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param tau A non-negative numeric for calculating Tau Gap. Defaults to 1.
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_tau_gap <- function(plans, dvote, rvote, tau = 1) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_mean_median <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param normalize Default is FALSE. Translate score to an angle?
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_decl <- function(plans, dvote, rvote, normalize = FALSE) {

  plans <- process_plans(plans)
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

  dec <- declination(dvs = dvs, dseat_vec = dseat_vec, nd = nd)

  if (normalize) {
    dec <- atan(dec)/log(nd)
  }

  rep(dec, each = nd)
}

#' Calculate Responsiveness
#'
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param v vote share to calculate bias at. Numeric. Default is 0.5.
#' @param bandwidth Defaults to 0.01. A value between 0 and 1 for the step size to estimate the slope.
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_resp <- function(plans, dvote, rvote, v, bandwidth = FALSE) {

  plans <- process_plans(plans)
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

  rep(responsiveness(dvs = dvs, v = v, nd = nd, bandwidth = bandwidth), each = nd)
}

#' Calculate Lopsided Wins
#'
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_lopsided_wins <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_rmd <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
#' @templateVar plans TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' # todo example
part_sscd <- function(plans, dvote, rvote) {

  plans <- process_plans(plans)
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
