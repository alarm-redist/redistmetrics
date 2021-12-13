#' Shorten District by Plan vector
#'
#' If `x` is repeated for each district, it returns a plan level value. Otherwise
#' it returns `x`.
#'
#' @param x summary statistic at the district level
#'
#' @return x or plan level subset of x
#' @export
#'
#' @examples
#' by_plan(letters)
#' by_plan(rep(letters, each = 2))
#'
by_plan <- function(x) {
  reps <- unique(rle(x)$lengths)
  if (length(reps) == 1) {
    x[reps * seq_len(length(x) / reps)]
  } else {
    x
  }
}
