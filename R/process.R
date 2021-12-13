process_plans <- function(plans) {
  if (inherits(plans, 'redist_plans')) {
    plans <- attr(plans, 'plans')
  }
  if (!is.numeric(plans)) {
    cli::cli_abort('{.arg plans} should be a numeric vector or matrix.')
  }
  if (!is.matrix(plans)) {
    plans <- as.matrix(plans)
  }
  if (any(is.na(plans))) {
    cli::cli_abort('{.val NA} in argument to {.arg plans}.')
  }
  plans
}
