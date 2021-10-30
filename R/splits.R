#' Compute Number of Administrative Units Split
#'
#' @param plans todo inherits
#' @param shp todo inherits
#' @param admin todo inherits
#'
#' @return numeric vector
#' @export
#' @concept splits
#'
#' @examples
#' # todo examples
splits_admin <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('`admin` not found in `shp`.')
  }
  admin <- make_id(admin)

  # run splits with max_split = 1 ----
  splits(plans - 1, community = admin - 1, length(unique(plans[, 1])), 1)
}

#' Compute Number of Sub-Administrative Units Split More than Once
#'
#' @param plans todo inherits
#' @param shp todo inherits
#' @param sub_admin todo inherits
#'
#' @return numeric vector
#' @export
#' @concept splits
#'
#' @examples
#' # todo examples
splits_sub_admin <- function(plans, shp, sub_admin) {

  # prep inputs ----
  plans <- process_plans(plans)

  # prep admin ----
  sub_admin <- rlang::eval_tidy(rlang::enquo(sub_admin), data = shp)
  if (is.null(sub_admin)) {
    cli::cli_abort('`admin` not found in `shp`.')
  }

  plans <- plans[!is.na(sub_admin), ]
  sub_admin <- sub_admin[!is.na(sub_admin)]

  sub_admin <- make_id(sub_admin)

  # run splits with max_split = 2 ----
  splits(plans - 1, community = sub_admin - 1, length(unique(plans[, 1])), 2)
}

#' Compute Number of Administrative Units Split More than Once
#'
#' @param plans todo inherits
#' @param shp todo inherits
#' @param admin todo inherits
#'
#' @return numeric vector
#' @export
#' @concept splits
#'
#' @examples
#' # todo examples
splits_multi <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('`admin` not found in `shp`.')
  }
  admin <- make_id(admin)

  # run splits with max_split = 2 ----
  splits(plans - 1, community = admin - 1, length(unique(plans[, 1])), 2)
}
