#' Shorten District by Plan vector
#'
#' If `x` is repeated for each district, it returns a plan level value. Otherwise
#' it returns `x`.
#'
#' @param x summary statistic at the district level
#' @param ndists numeric. Number of districts. Estimated as the gcd of the unique
#' run length encodings if missing.
#'
#' @return x or plan level subset of x
#' @export
#'
#' @examples
#' by_plan(letters)
#' by_plan(rep(letters, each = 2))
#'
by_plan <- function(x, ndists) {
  if (missing(ndists)) {
    reps <- unique(rle(x)$lengths)
    ndists <- ifelse(length(reps) == 1, reps, min(apply(utils::combn(reps, 2), 2, gcd)))
  }

  x[ndists * seq_len(length(x) / ndists)]
}


gcd <- function(x) {
  big <- max(x)
  small <- min(x)
  rem <- big %% small

  if (rem == 0) {
    return(small)
  }

  gcd(c(small, rem))
}
