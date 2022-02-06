#' Calculate Partisan Bias
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param v vote share to calculate bias at. Numeric. Default is 0.5.
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Jonathan N. Katz, Gary King, and Elizabeth Rosenblatt. 2020.
#' Theoretical Foundations and Empirical Evaluations of Partisan Fairness in District-Based Democracies.
#' American Political Science Review, 114, 1, Pp. 164-178.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_bias(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_bias(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_bias <- function(plans, shp, dvote, rvote, v = 0.5) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
  }


  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  cli::cli_inform(
    '{.pkg redistmetrics} 1.0.0 has partisan bias direction reversed. Positive is now pro-Republican bias.',
    .frequency = 'once', .frequency_id = 'part_bias'
  )

  rep(biasatv(dvs = dvs, v = v, nd = nd), each = nd)
}

#' Calculate Democratic Seats
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_dseats(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_dseats(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_dseats <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_dvs(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_dvs(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_dvs <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  c(DVS(dcounts = dcounts, rcounts = rcounts))
}


#' Calculate Efficiency Gap
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Nicholas O. Stephanopoulos. 2015.
#' Partisan Gerrymandering and the Efficiency Gap.
#' The University of Chicago Law Review, 82, Pp. 831-900.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_egap(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_egap(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_egap <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Nicholas O. Stephanopoulos. 2015.
#' Partisan Gerrymandering and the Efficiency Gap.
#' The University of Chicago Law Review, 82, Pp. 831-900.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_egap_ep(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_egap_ep(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_egap_ep <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param tau A non-negative numeric for calculating Tau Gap. Defaults to 1.
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Gregory S. Warrington. 2018. "Quantifying Gerrymandering Using the Vote Distribution."
#' Election Law Journal: Rules, Politics, and Policy. Pp. 39-57.http://doi.org/10.1089/elj.2017.0447
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_tau_gap(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_tau_gap(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_tau_gap <- function(plans, shp, dvote, rvote, tau = 1) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Michael D. McDonald and Robin E. Best. 2015.
#' Unfair Partisan Gerrymanders in Politics and Law: A Diagnostic Applied to Six Cases.
#' Election Law Journal: Rules, Politics, and Policy. 14. 4. Pp. 312-330.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # zero for the two district case:
#' # For a single plan:
#' part_mean_median(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_mean_median(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_mean_median <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param normalize Default is TRUE Translate score to an angle?
#' @param adjust Default is TRUE. Applies a correction to increase cross-size comparison.
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Gregory S. Warrington. 2018. "Quantifying Gerrymandering Using the Vote Distribution."
#' Election Law Journal: Rules, Politics, and Policy. Pp. 39-57.http://doi.org/10.1089/elj.2017.0447
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_decl(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_decl(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_decl <- function(plans, shp, dvote, rvote, normalize = TRUE, adjust = TRUE) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  dec <- declination_angle(dvs = dvs, dseat_vec = dseat_vec, nd = nd)

  if (normalize) {
    dec <- 2 * dec / pi
  }

  if (adjust) {
    dec <- dec * log(nd) / 2
  }

  rep(dec, each = nd)
}

#' Calculate Simplified Declination
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Jonathan N. Katz, Gary King, and Elizabeth Rosenblatt. 2020.
#' Theoretical Foundations and Empirical Evaluations of Partisan Fairness in District-Based Democracies.
#' American Political Science Review, 114, 1, Pp. 164-178.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_decl_simple(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_decl_simple(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_decl_simple <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  declination_simple(dvs = dvs, dseat_vec = dseat_vec, nd = nd) %>%
    rep(each = nd)
}


#' Calculate Responsiveness
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @param v vote share to calculate bias at. Numeric. Default is 0.5.
#' @param bandwidth Defaults to 0.01. A value between 0 and 1 for the step size to estimate the slope.
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Jonathan N. Katz, Gary King, and Elizabeth Rosenblatt. 2020.
#' Theoretical Foundations and Empirical Evaluations of Partisan Fairness in District-Based Democracies.
#' American Political Science Review, 114, 1, Pp. 164-178.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_resp(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_resp(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_resp <- function(plans, shp, dvote, rvote, v = 0.5, bandwidth = 0.01) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Samuel S.-H. Wang. 2016. "Three Tests for Practical Evaluation of Partisan Gerrymandering."
#' Stanford Law Review, 68, Pp. 1263 - 1321.
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_lop_wins(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_lop_wins(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_lop_wins <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dseat_vec <- dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(lopsidedwins(dvs = dvs, dseat_vec = dseat_vec, nd = nd), each = nd)
}

#' Calculate Ranked Marginal Deviation
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Gregory Herschlag, Han Sung Kang, Justin Luo, Christy Vaughn Graves, Sachet Bangia,
#' Robert Ravier & Jonathan C. Mattingly (2020) Quantifying Gerrymandering in North Carolina,
#' Statistics and Public Policy, 7:1, 30-38, DOI: 10.1080/2330443X.2020.1796400
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_rmd(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_rmd(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_rmd <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
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
#' @templateVar shp TRUE
#' @templateVar dvote TRUE
#' @templateVar rvote TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept partisan
#'
#' @references
#' Gregory Herschlag, Han Sung Kang, Justin Luo, Christy Vaughn Graves, Sachet Bangia,
#' Robert Ravier & Jonathan C. Mattingly (2020) Quantifying Gerrymandering in North Carolina,
#' Statistics and Public Policy, 7:1, 30-38, DOI: 10.1080/2330443X.2020.1796400
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' part_sscd(plans = nh$r_2020, shp = nh, rvote = nrv, dvote = ndv)
#'
#' # Or many plans:
#' part_sscd(plans = nh_m[, 3:5], shp = nh, rvote = nrv, dvote = ndv)
#'
part_sscd <- function(plans, shp, dvote, rvote) {

  plans <- process_plans(plans)
  dvote <- rlang::eval_tidy(rlang::enquo(dvote), shp)
  rvote <- rlang::eval_tidy(rlang::enquo(rvote), shp)

  if (any(is.na(dvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg dvote}.')
  }
  if (any(is.na(rvote))) {
    cli::cli_abort('{.val NA} in argument to {.arg rvote}.')
  }
  if (length(rvote) != nrow(plans)) {
    cli::cli_abort('{.arg rvote} length and {.arg plans} rows are not equal.')
  }
  if (length(dvote) != nrow(plans)) {
    cli::cli_abort('{.arg dvote} length and {.arg plans} rows are not equal.')
  }

  nd <- length(unique(plans[, 1]))
  rcounts <- agg_p2d(vote = rvote, dm = plans, nd = nd)
  dcounts <- agg_p2d(vote = dvote, dm = plans, nd = nd)
  dvs <- DVS(dcounts = dcounts, rcounts = rcounts)

  rep(smoothseat(dvs = dvs, nd = nd), each = nd)
}
