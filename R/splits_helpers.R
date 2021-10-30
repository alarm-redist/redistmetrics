#' Make IDs from a vector
#'
#' Simpler `redist::redist.county.id()` that handles NAs more gracefully
#'
#' @param x vector to match
#'
#' @return
#' @noRd
#'
#' @examples
#' x <- sample(letters[1:3], 10, replace = TRUE)
#' x[10] <- NA
#' make_id(x)
make_id <- function(x) {
  match(x,  unique(sort(x, na.last = TRUE)))
}
