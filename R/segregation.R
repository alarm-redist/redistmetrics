#' Compute Dissimilarity Index
#'
#' @param plans todo inherits
#' @param shp todo inherits
#' @param group_pop todo inherits
#' @param total_pop todo inherits
#'
#' @return numeric vector
#' @export
#' @concept segregation
#'
#' @examples
#' # todo examples
seg_dissim <- function(plans, shp, group_pop, total_pop) {
  # process inputs ----
  plans <- process_plans(plans)
  group_pop <- rlang::eval_tidy(rlang::enquo(group_pop), data = shp)
  total_pop <- rlang::eval_tidy(rlang::enquo(total_pop), data = shp)
  if (is.null(group_pop)) {
    cli::cli_abort('`group_pop` not found in `shp`.')
  }
  if (is.null(total_pop)) {
    cli::cli_abort('`total_pop` not found in `shp`.')
  }

  segregationcalc(plans, group_pop, total_pop)
}
