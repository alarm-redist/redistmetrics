#' Split feeder penalty per district
#'
#' @param plans A `redist_plans` object or an integer matrix (n_units × n_plans)
#' of district labels.
#' @param lower An assignment vector of lower level school assignments.
#' @param pop Optional numeric vector (length n_units) of unit populations.
#' If `NULL`, attempts to infer from common column names in `shp`; falls back
#' to all ones.
#'
#' @return A numeric vector of length `ndists * nplans`, i.e., the district-by-plan
#' scores flattened column-major. Use `by_plan()` to get one value per plan.
#' @export
split_feeders <- function(plans, lower, pop = NULL) {
    # coerce plans to integer matrix
    plans_matrix <- if (inherits(plans, "redist_plans")) {
        redist::get_plans_matrix(plans)
    } else {
        as.matrix(plans)
    }
    storage.mode(plans_matrix) <- "integer"
    
    n_units <- nrow(plans_matrix)
    n_plans <- ncol(plans_matrix)
    if (n_units == 0L || n_plans == 0L)
        stop("`plans` must have at least one unit and one plan.", call. = FALSE)
    
    # validate lower plan
    if (length(lower) != n_units)
        stop("`lower` must have length equal to nrow(plans).", call. = FALSE)
    lower <- as.integer(lower)
    
    # validate districts
    ndists <- max(plans_matrix, na.rm = TRUE)
    if (!is.finite(ndists) || ndists < 1L)
        stop("District labels in `plans` must be positive integers.", call. = FALSE)

    # get population
    shp <- as_sf(map)
    if (is.null(pop)) {
        dat <- if (inherits(shp, "redist_map")) attr(shp, "shp") %||% shp else shp
        if (inherits(dat, "sf")) {
            pop_candidates <- c("pop", "population", "total_pop", "TOTPOP", "TOT_POP", "pop_total")
            pick <- pop_candidates[pop_candidates %in% names(dat)]
            pop <- if (length(pick)) dat[[pick[1]]] else rep(1, n_units)
        } else {
            pop <- rep(1, n_units)
        }
    }
    if (length(pop) != n_units)
        stop("`pop` must have length equal to nrow(plans).", call. = FALSE)
    pop <- as.numeric(pop)
                                        
    # call C++ function to calculate phase commute scores
    res_mat <- splitfeeders(
        plans          = plans_matrix,
        lower          = lower,
        pop            = pop,
        ndists         = as.integer(ndists)
    )
    
    c(res_mat)
}

#' Capacity utilization penalty per district
#'
#' @param plans A `redist_plans` object or an integer matrix (n_units × n_plans)
#' of district labels.
#' @param schools_capacity An integer vector of school capacities in the same order as schools.
#' @param pop Optional numeric vector (length n_units) of unit populations.
#' If `NULL`, attempts to infer from common column names in `shp`; falls back
#' to all ones.
#'
#' @return A numeric vector of length `ndists * nplans`, i.e., the district-by-plan
#' scores flattened column-major. Use `by_plan()` to get one value per plan.
#' @export
capacity_util <- function(plans, schools_capacity, pop = NULL) {
    # coerce plans to integer matrix
    plans_matrix <- if (inherits(plans, "redist_plans")) {
        redist::get_plans_matrix(plans)
    } else {
        as.matrix(plans)
    }
    storage.mode(plans_matrix) <- "integer"
    
    n_units <- nrow(plans_matrix)
    n_plans <- ncol(plans_matrix)
    if (n_units == 0L || n_plans == 0L)
        stop("`plans` must have at least one unit and one plan.", call. = FALSE)
    
    # validate districts
    ndists <- max(plans_matrix, na.rm = TRUE)
    if (!is.finite(ndists) || ndists < 1L)
        stop("District labels in `plans` must be positive integers.", call. = FALSE)

    # get population
    shp <- as_sf(map)
    if (is.null(pop)) {
        dat <- if (inherits(shp, "redist_map")) attr(shp, "shp") %||% shp else shp
        if (inherits(dat, "sf")) {
            pop_candidates <- c("pop", "population", "total_pop", "TOTPOP", "TOT_POP", "pop_total")
            pick <- pop_candidates[pop_candidates %in% names(dat)]
            pop <- if (length(pick)) dat[[pick[1]]] else rep(1, n_units)
        } else {
            pop <- rep(1, n_units)
        }
    }
    if (length(pop) != n_units)
        stop("`pop` must have length equal to nrow(plans).", call. = FALSE)
    pop <- as.numeric(pop)
                                        
    # call C++ function to calculate phase commute scores
    res_mat <- capacityutil(
        plans          = plans_matrix,
        schools_capacity = schools_capacity,
        pop            = pop,
        ndists         = as.integer(ndists)
    )
    
    c(res_mat)
}

#' School outside zone penalty per district
#'
#' @param plans A `redist_plans` object or an integer matrix (n_units × n_plans)
#' of district labels.
#' @param schools An integer vector (1-based) of row indices that correspond to
#' school units in `shp`.
#'
#' @return A numeric vector of length `ndists * nplans`, i.e., the district-by-plan
#' scores flattened column-major. Use `by_plan()` to get one value per plan.
#' @export
school_outside_zone <- function(plans, schools) {
    # coerce plans to integer matrix
    plans_matrix <- if (inherits(plans, "redist_plans")) {
        redist::get_plans_matrix(plans)
    } else {
        as.matrix(plans)
    }
    storage.mode(plans_matrix) <- "integer"
    
    n_units <- nrow(plans_matrix)
    n_plans <- ncol(plans_matrix)
    if (n_units == 0L || n_plans == 0L)
        stop("`plans` must have at least one unit and one plan.", call. = FALSE)
    
    # validate school indices
    schools <- as.integer(schools)
    if (any(schools < 1L | schools > n_units))
        stop("`schools` contains indices out of range.", call. = FALSE)
    
    # validate districts
    ndists <- max(plans_matrix, na.rm = TRUE)
    if (!is.finite(ndists) || ndists < 1L)
        stop("District labels in `plans` must be positive integers.", call. = FALSE)
                                        
    # call C++ function to calculate phase commute scores
    res_mat <- schooloutsidezone(
        plans          = plans_matrix,
        schools        = schools - 1L,
        ndists         = as.integer(ndists)
    )
    
    c(res_mat)
}

#' Attendance island penalty per district
#'
#' @param plans A `redist_plans` object or an integer matrix (n_units × n_plans)
#' of district labels.
#'
#' @return A numeric vector of length `ndists * nplans`, i.e., the district-by-plan
#' scores flattened column-major. Use `by_plan()` to get one value per plan.
#' @export
attendance_islands <- function(plans) {
    # coerce plans to integer matrix
    plans_matrix <- if (inherits(plans, "redist_plans")) {
        redist::get_plans_matrix(plans)
    } else {
        as.matrix(plans)
    }
    storage.mode(plans_matrix) <- "integer"
    
    n_units <- nrow(plans_matrix)
    n_plans <- ncol(plans_matrix)
    if (n_units == 0L || n_plans == 0L)
        stop("`plans` must have at least one unit and one plan.", call. = FALSE)
    
    # validate districts
    ndists <- max(plans_matrix, na.rm = TRUE)
    if (!is.finite(ndists) || ndists < 1L)
        stop("District labels in `plans` must be positive integers.", call. = FALSE)
                                        
    # matrix to hold counts
    res_mat <- matrix(NA_real_, nrow = ndists, ncol = n_plans)

    for (p in seq_len(n_plans)) {
        plan <- plans_matrix[, p]

        for (d in seq_len(ndists)) {
            # get indices of units in this district
            idx <- which(!is.na(plan) & plan == d)

            if (length(idx) == 0L) {
                # no units assigned to this district in this plan
                res_mat[d, p] <- NA_real_
            } else {
                # union all units in district, cast to MULTIPOLYGON, count parts
                uni <- sf::st_union(geom[idx])
                mp  <- sf::st_cast(uni, "MULTIPOLYGON")
                # number of discontiguous parts in this district
                n_parts <- sf::lengths(sf::st_geometry(mp))
                # only count extra islands
                res_mat[d, p] <- max(n_parts - 1L, 0L)
            }
        }
    }
    
    c(res_mat)
}

# Safely turn a redist_map into its underlying sf (or pass sf through)
as_sf <- function(map) {
    if (inherits(map, "redist_map")) {
        shp <- attr(map, "shp")
        if (is.null(shp)) stop("`map` looks like a redist_map but has no `attr(, 'shp')`.")
        shp
    } else if (inherits(map, "sf")) {
        map
    } else {
        stop("`map` must be a `redist_map` or an `sf` with geometries.", call. = FALSE)
    }
}