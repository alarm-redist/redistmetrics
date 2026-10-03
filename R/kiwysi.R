#' Calculate KIWSI Compactness
#'
#' Predicts the Kaufman, King, and Komisarchik "know it when you see it"
#' compactness score by combining 14 `redistmetrics` compactness measures in a
#' linear model. Scores are bounded between 0 and 100, where 0 is least compact
#' and 100 is most compact.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
#' @param return_all If `TRUE`, return a tibble containing the score and all
#'   features used by the model.
#' @param ... Precomputed feature vectors. These are tidy-evaluated in `shp`.
#'   Use the corresponding function name, such as `comp_polsby = polsby`.
#' @template template
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept compactness
#'
#' @references
#' Aaron Kaufman, Gary King, and Mayya Komisarchik. 2021.
#' How to Measure Legislative District Compactness If You Only Know it When You See It.
#' American Journal of Political Science. 65, 3. Pp. 533-550.
#'
#' @examples
#' data(nh)
#' comp_kiwysi(plans = nh$r_2020, shp = nh)
comp_kiwysi <- function(
  plans,
  shp,
  epsg = 3857,
  ncores = 1,
  return_all = FALSE,
  ...
) {
  features <- kiwysi_features(plans, shp, epsg = epsg, ncores = ncores, ...)
  pred <- as.numeric(stats::predict(kiwysi_model, newdata = features))
  pred <- pmin(100, pmax(0, pred))
  pred <- 100 - pred

  if (return_all) {
    return(dplyr::as_tibble(cbind(kiwysi = pred, features)))
  }

  pred
}

kiwysi_features <- function(plans, shp, epsg = 3857, ncores = 1, ...) {
  plans <- process_plans(plans)
  dots <- rlang::enquos(...)
  if (length(dots) > 0 && any(names(dots) == '')) {
    cli::cli_abort(
      'All precomputed KIWSI features in {.arg ...} must be named.'
    )
  }

  features <- c(
    'comp_polsby',
    'comp_ch',
    'comp_reock',
    'comp_bbox_reock',
    'comp_box_reock',
    'comp_lw',
    'comp_bc',
    'comp_x_sym',
    'comp_y_sym',
    'comp_skew',
    'comp_corners',
    'comp_jagged',
    'comp_components',
    'comp_holes'
  )
  unknown <- setdiff(names(dots), features)
  if (length(unknown) > 0) {
    cli::cli_abort(
      'Unknown KIWSI feature{?s} in {.arg ...}: {.val {unknown}}.'
    )
  }

  expected <- length(unique(c(plans))) * ncol(plans)
  dots <- lapply(dots, function(dot) as.numeric(rlang::eval_tidy(dot, data = shp)))
  wrong_length <- names(dots)[lengths(dots) != expected]
  if (length(wrong_length) > 0) {
    cli::cli_abort(
      'KIWSI feature vectors {.val {wrong_length}} must have length {expected} (one value per district per plan).'
    )
  }

  if ('comp_polsby' %in% names(dots)) {
    polsby <- dots$comp_polsby
  } else {
    polsby <- comp_polsby(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_ch' %in% names(dots)) {
    hull <- dots$comp_ch
  } else {
    hull <- comp_ch(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_reock' %in% names(dots)) {
    reock <- dots$comp_reock
  } else {
    reock <- comp_reock(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_bbox_reock' %in% names(dots)) {
    bbox <- dots$comp_bbox_reock
  } else {
    bbox <- comp_bbox_reock(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_box_reock' %in% names(dots)) {
    box_reock <- dots$comp_box_reock
  } else {
    box_reock <- comp_box_reock(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_lw' %in% names(dots)) {
    lenwid <- dots$comp_lw
  } else {
    lenwid <- comp_lw(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_bc' %in% names(dots)) {
    boyce <- dots$comp_bc
  } else {
    boyce <- comp_bc(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_x_sym' %in% names(dots)) {
    sym_x <- dots$comp_x_sym
  } else {
    sym_x <- comp_x_sym(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_y_sym' %in% names(dots)) {
    sym_y <- dots$comp_y_sym
  } else {
    sym_y <- comp_y_sym(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_skew' %in% names(dots)) {
    skew <- dots$comp_skew
  } else {
    skew <- comp_skew(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_corners' %in% names(dots)) {
    corners <- dots$comp_corners
  } else {
    corners <- comp_corners(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_jagged' %in% names(dots)) {
    jagged <- dots$comp_jagged
  } else {
    jagged <- comp_jagged(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_components' %in% names(dots)) {
    components <- dots$comp_components
  } else {
    components <- comp_components(plans, shp, epsg = epsg, ncores = ncores)
  }

  if ('comp_holes' %in% names(dots)) {
    holes <- dots$comp_holes
  } else {
    holes <- comp_holes(plans, shp, epsg = epsg, ncores = ncores)
  }

  data.frame(
    polsby = polsby,
    hull = hull,
    reock = reock,
    bbox = bbox,
    box_reock = box_reock,
    lenwid = lenwid,
    boyce = boyce,
    sym_x = sym_x,
    sym_y = sym_y,
    skew = skew,
    corners = corners,
    jagged = jagged,
    components = components,
    holes = holes
  )
}
