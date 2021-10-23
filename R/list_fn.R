#' Return Functions Matching a Prefix
#'
#' This package uses prefixes for each function that correspond to the type of measure.
#' This function returns the functions
#'
#' @param prefix character prefix of functions to return
#'
#' @return character vector of functions
#' @export
#'
#' @examples
#' list_fn('part')
list_fn <- function(prefix) {
  fns <- utils::lsf.str('package:rict')
  fns[startsWith(fns, prefix)]
}

list_args <- function(fn) {
  names(formals(fn))
}

