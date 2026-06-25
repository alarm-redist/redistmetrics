#' Calculate KIWSI Compactness
#'
#' Predicts the Kaufman, King, and Komisarchik "know it when you see it"
#' compactness score from `redistmetrics` compactness measures. Scores are
#' bounded between 0 and 100, where larger values indicate less compact
#' districts.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar epsg TRUE
#' @templateVar ncores TRUE
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
comp_kiwysi <- function(plans, shp, epsg = 3857, ncores = 1) {
  features <- kiwysi_features(plans, shp, epsg = epsg, ncores = ncores)
  pred <- as.numeric(stats::predict(kiwysi_model, newdata = features))
  pmin(100, pmax(0, pred))
}

kiwysi_features <- function(plans, shp, epsg = 3857, ncores = 1) {
  data.frame(
    polsby = comp_polsby(plans, shp, epsg = epsg, ncores = ncores),
    hull = comp_ch(plans, shp, epsg = epsg, ncores = ncores),
    reock = comp_reock(plans, shp, epsg = epsg, ncores = ncores),
    bbox = comp_bbox_reock(plans, shp, epsg = epsg, ncores = ncores),
    box_reock = comp_box_reock(plans, shp, epsg = epsg, ncores = ncores),
    lenwid = comp_lw(plans, shp, epsg = epsg, ncores = ncores),
    boyce = comp_bc(plans, shp, epsg = epsg, ncores = ncores),
    sym_x = comp_x_sym(plans, shp, epsg = epsg, ncores = ncores),
    sym_y = comp_y_sym(plans, shp, epsg = epsg, ncores = ncores),
    skew = comp_skew(plans, shp, epsg = epsg, ncores = ncores),
    corners = comp_corners(plans, shp, epsg = epsg, ncores = ncores),
    jagged = comp_jagged(plans, shp, epsg = epsg, ncores = ncores),
    components = comp_components(plans, shp, epsg = epsg, ncores = ncores),
    holes = comp_holes(plans, shp, epsg = epsg, ncores = ncores)
  )
}
