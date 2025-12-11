#' Commute-disruption penalty per district
#'
#' @param plans A `redist_plans` object or an integer matrix (n_units × n_plans)
#' of district labels.
#' @param map A `redist_map` or an `sf` with one row per unit.
#' @param current An integer vector of district assignments under the old plan.
#' @param schools An integer vector (1-based) of row indices that correspond to
#' school units in `shp`.
#' @param commute_times A numeric matrix (n_units × n_schools) of commute times
#' (in seconds) from each unit to each school. Can be computed via
#' `redistmetrics::get_commute_matrix()`.
#' @param pop Optional numeric vector (length n_units) of unit populations.
#' If `NULL`, attempts to infer from common column names in `shp`; falls back
#' to all ones.
#'
#' @return A numeric vector of length `ndists * nplans`, i.e., the district-by-plan
#' scores flattened column-major. Use `by_plan()` to get one value per plan.
#' @export
phase_commute <- function(plans, map, current, schools, commute_times, pop = NULL) {
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
    
    # validate old plan
    if (length(current) != n_units)
        stop("`current` must have length equal to nrow(plans).", call. = FALSE)
    current <- as.integer(current)
    
    # validate school indices
    schools <- as.integer(schools)
    if (any(schools < 1L | schools > n_units))
        stop("`schools` contains indices out of range.", call. = FALSE)
        
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
    
    # validate districts
    ndists <- max(plans_matrix, na.rm = TRUE)
    if (!is.finite(ndists) || ndists < 1L)
        stop("District labels in `plans` must be positive integers.", call. = FALSE)

    # call C++ function to calculate phase commute scores
    res_mat <- phasecommute(
        plans          = plans_matrix,
        current        = current,
        pop            = pop,
        schools        = schools - 1L,
        commute_times  = commute_times,
        ndists         = as.integer(ndists)
    )
    
    c(res_mat)
}

#' Max commute penalty per district
#'
#' @param plans A `redist_plans` object or an integer matrix (n_units × n_plans)
#' of district labels.
#' @param map A `redist_map` or an `sf` with one row per unit.
#' @param schools An integer vector (1-based) of row indices that correspond to
#' school units in `shp`.
#' @param commute_times A numeric matrix (n_units × n_schools) of commute times
#' (in seconds) from each unit to each school. Can be computed via
#' `redistmetrics::get_commute_matrix()`.
#'
#' @return A numeric vector of length `ndists * nplans`, i.e., the district-by-plan
#' scores flattened column-major. Use `by_plan()` to get one value per plan.
#' @export
max_commute <- function(plans, map, schools, commute_times) {
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
    res_mat <- maxcommute(
        plans          = plans_matrix,
        schools        = schools - 1L,
        commute_times  = commute_times,
        ndists         = as.integer(ndists)
    )
    
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


#' Compute a block-by-school commute-time matrix (seconds)
#'
#' @param shp An `sf` with one row per unit (or a `redist_map` whose underlying `sf` is used).
#' @param schools An integer vector (1-based) of row indices for school units in `shp`.
#' @param profile OSRM profile, e.g., `"car"`
#' @param server Base URL of the OSRM server (e.g., `"http://127.0.0.1:5000"`).
#' @param src_chunk Chunk size for source rows when calling OSRM.
#' @param dst_chunk Chunk size for destination rows when calling OSRM.
#'
#' @return A numeric matrix with dimensions `nrow(shp) × length(schools)` of commute times in seconds.
#' @export
get_commute_matrix <- function(shp, schools, profile, server, src_chunk, dst_chunk) {
    # get representative points
    schools <- sf::st_point_on_surface(shp[schools,])
    blocks <- sf::st_point_on_surface(shp)

    # convert to WGS84 lon/lat for mapbox
    schools <- sf::st_transform(schools, 4326)
    blocks <- sf::st_transform(blocks, 4326)

    # get lon/lat matrices
    schools_coord <- sf::st_coordinates(schools)
    blocks_coord <- sf::st_coordinates(blocks)

    # compute commute matrix in seconds
    commute_times <- get_commute_times(blocks_coord, schools_coord, profile, 
                                       server, src_chunk, dst_chunk
    )

    cli::cli_alert_info("Calculated commute times matrix with {nrow(commute_times)} blocks and {ncol(commute_times)} schools.")

    commute_times
}

# get matrix of commute times between blocks and schools via OSRM
get_commute_times <- function(blocks_coord, 
                              schools_coord, 
                              profile = "driving", 
                              server = "http://127.0.0.1:5000", 
                              src_chunk = 100, 
                              dst_chunk = 140) {
    # ensure osrm talks to server/profile
    old_url <- getOption("osrm.server")
    old_profile <- getOption("osrm.profile")
    on.exit({
        options(osrm.server = old_url)
        options(osrm.profile = old_profile)
    }, add = TRUE)
    options(osrm.server = server, osrm.profile = profile)

    to_sf <- function(mat) {
        sf::st_as_sf(
            data.frame(lon = mat[,1], lat = mat[,2]),
            coords = c("lon", "lat"), crs = 4326, agr = "constant"
        )
    }

    blocks_sf <- to_sf(blocks_coord)
    schools_sf <- to_sf(schools_coord)

    n_blocks <- nrow(blocks_sf)
    n_schools <- nrow(schools_sf)
    commute_times <- matrix(NA_real_, nrow = n_blocks, ncol = n_schools)

    # chunked osrm::osrmTable calls
    for (b0 in seq(1, n_blocks, by = src_chunk)) {
        b1 <- min(b0 + src_chunk - 1, n_blocks)
        src <- blocks_sf[b0:b1, , drop = FALSE]
        
        for (s0 in seq(1, n_schools, by = dst_chunk)) {
            s1 <- min(s0 + dst_chunk - 1, n_schools)
            dst <- schools_sf[s0:s1, , drop = FALSE]
            
            chunk_ok <- TRUE
            duration_sec <- NULL 

            tryCatch({
                table <- osrm_table_safe(src = src, dst = dst, profile = profile, server = server, measure = "duration")
                duration_sec <- as.matrix(table$duration)
                
                # check that dimensions are as expected
                if (!all(dim(duration_sec) == c(nrow(src), nrow(dst)))) {
                    stop(sprintf("Returned table is %dx%d, expected %dx%d.", 
                                 nrow(duration_sec), ncol(duration_sec), 
                                 nrow(src), nrow(dst)))
                }
            }, error = function(e) {
                chunk_ok <<- FALSE
                cli::cli_warn(c("Failed to get commute times from OSRM server for blocks {b0}-{b1} and schools {s0}-{s1}:",
                    "x" = "{e$message}",
                    "*" = "Filling with {.val NA}."))
            })

            # insert commute times into matrix
            if (chunk_ok) {
                commute_times[b0:b1, s0:s1] <- duration_sec
            }
        }
    }

    commute_times # return final matrix
}

osrm_table_safe <- function(src, dst, profile = "driving",
                            server = "http://127.0.0.1:5000",
                            measure = "duration") {
    stopifnot(measure %in% c("duration","distance"))
    
    src <- sf::st_transform(src, 4326)
    dst <- sf::st_transform(dst, 4326)
    
    to_str <- function(g) {
        paste(apply(sf::st_coordinates(g), 1, \(xy) paste0(xy[1], ",", xy[2])), collapse = ";")
    }
    src_str <- to_str(src)
    dst_str <- to_str(dst)
    
    n_src <- nrow(src)
    n_dst <- nrow(dst)
    
    # 0-based indices into the COMBINED list: "src;dst"
    src_idx <- paste0(seq(0, n_src - 1), collapse = ";")
    dst_idx <- paste0(seq(n_src, n_src + n_dst - 1), collapse = ";")
    
    base <- sub("/+$", "", trimws(server))
    url  <- sprintf("%s/table/v1/%s/%s;%s?sources=%s&destinations=%s",
                    base, profile, src_str, dst_str, src_idx, dst_idx)
                    
    resp <- httr::GET(url)
    httr::stop_for_status(resp)
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    j <- jsonlite::fromJSON(txt)
    
    out <- list()
    if (!is.null(j$durations)) {
        dur <- j$durations
        out$duration <- if (is.list(dur)) {
            do.call(rbind, lapply(dur, function(row) as.numeric(row)))
        } else {
            as.matrix(dur)
        }
    }
    out
}