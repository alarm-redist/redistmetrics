#' Compute Number of Administrative Units Split
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_admin(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_admin(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_admin <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)

  # run splits with max_split = 1 ----
  splits(reindex(plans, nd) - 1, community = admin - 1, nd, 1) %>%
    rep(each = nd)
}

#' Compute Number of Sub-Administrative Units Split
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar sub_admin TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_sub_admin(plans = nh$r_2020, shp = nh, sub_admin = county)
#'
#' # Or many plans:
#' splits_sub_admin(plans = nh_m[, 3:5], shp = nh, sub_admin = county)
#'
splits_sub_admin <- function(plans, shp, sub_admin) {

  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  sub_admin <- rlang::eval_tidy(rlang::enquo(sub_admin), data = shp)
  if (is.null(sub_admin)) {
    cli::cli_abort('{.arg sub_admin} not found in {.arg shp}.')
  }

  plans <- plans[!is.na(sub_admin), , drop = FALSE]
  sub_admin <- sub_admin[!is.na(sub_admin)]

  sub_admin <- make_id(sub_admin)

  # run splits with max_split = 2 ----
  splits(reindex(plans, nd) - 1, community = sub_admin - 1, nd, 1) %>%
    rep(each = nd)
}

#' Compute Number of Administrative Units Split More than Once
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @return numeric vector
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_multi(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_multi(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_multi <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)

  # run splits with max_split = 2 ----
  splits(reindex(plans, nd) - 1, community = admin - 1, nd, 2) %>%
    rep(each = nd)
}

#' Count the Number of Splits in Each Administrative Unit
#'
#' Tallies the number of unique administrative unit-districts. An unsplit administrative
#' unit will return an entry of 1, while each additional administrative unit-district
#' adds 1.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @return numeric matrix
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_count(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_count(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_count <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)

  admin_splits_count(plans, admin - 1)
}

#' Count the Total Splits in Each Plan
#'
#' Counts the total number of administrative splits.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @return numeric matrix
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_total(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_total(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_total <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)

  colSums(admin_splits_count(plans, admin - 1) - 1L) %>%
    rep(each = nd)
}
