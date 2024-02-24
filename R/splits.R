#' Compute Number of Administrative Units Split
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_admin(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_admin(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_admin <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- dplyr::n_distinct(plans[, 1])
  if (max(plans[, 1]) != nd) {
    plans = reindex(plans)
  }

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)

  # run splits with max_split = 1 ----
  rep(splits(plans, admin, nd, 1), each = nd)
}

#' Compute Number of Sub-Administrative Units Split
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar sub_admin TRUE
#' @template template_nosf
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_sub_admin(plans = nh$r_2020, shp = nh, sub_admin = county)
#'
#' # Or many plans:
#' splits_sub_admin(plans = nh_m[, 3:5], shp = nh, sub_admin = county)
#'
splits_sub_admin <- function(plans, shp, sub_admin) {
  # prep inputs ----
  plans <- process_plans(plans)

  # prep admin ----
  sub_admin <- rlang::eval_tidy(rlang::enquo(sub_admin), data = shp)
  if (is.null(sub_admin)) {
    cli::cli_abort('{.arg sub_admin} not found in {.arg shp}.')
  }

  plans <- plans[!is.na(sub_admin), , drop = FALSE]
  sub_admin <- sub_admin[!is.na(sub_admin)]
  sub_admin <- make_id(sub_admin)

  nd <- length(unique(plans[, 1]))
  if (max(plans[, 1]) != nd) {
    plans = reindex(plans)
  }

  # run splits with max_split = 1 ----
  rep(splits(plans, sub_admin, nd, 1), each = nd)
}

#' Compute Number of Administrative Units Split More than Once
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @returns A numeric vector. Can be shaped into a district-by-plan matrix.
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_multi(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_multi(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_multi <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))
  if (max(plans[, 1]) != nd) {
    plans = reindex(plans)
  }

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)

  # run splits with max_split = 2 ----
  rep(splits(plans, admin, nd, 2), each = nd)
}



#' Count the Number of Splits in Each Administrative Unit
#'
#' Tallies the number of unique administrative unit-districts. An unsplit administrative
#' unit will return an entry of 1, while each additional administrative unit-district
#' adds 1.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @return numeric matrix
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_count(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_count(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_count <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  row_names <- unique(admin)
  admin <- make_id(admin)
  nc <- length(unique(admin))

  admin_splits_count(plans, admin, nd, nc) |>
    `rownames<-`(value = row_names)
}

#' Count the Number of Splits in Each Sub-Administrative Unit
#'
#' Tallies the number of unique sub-administrative unit-districts. An unsplit administrative
#' unit will return an entry of 1, while each additional sub-administrative unit-district
#' adds 1.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar sub_admin TRUE
#' @template template_nosf
#'
#' @return numeric matrix
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_sub_count(plans = nh$r_2020, shp = nh, sub_admin = county)
#'
#' # Or many plans:
#' splits_sub_count(plans = nh_m[, 3:5], shp = nh, sub_admin = county)
#'
splits_sub_count <- function(plans, shp, sub_admin) {
  # prep inputs ----
  plans <- process_plans(plans)

  # prep admin ----
  sub_admin <- rlang::eval_tidy(rlang::enquo(sub_admin), data = shp)
  if (is.null(sub_admin)) {
    cli::cli_abort('{.arg sub_admin} not found in {.arg shp}.')
  }

  plans <- plans[!is.na(sub_admin), , drop = FALSE]
  sub_admin <- sub_admin[!is.na(sub_admin)]
  row_names <- unique(sub_admin)
  sub_admin <- make_id(sub_admin)

  nd <- length(unique(plans[, 1]))
  if (max(plans[, 1]) != nd) {
    plans = reindex(plans)
  }

  nc <- length(unique(sub_admin))

  admin_splits_count(plans, sub_admin, nd, nc) |>
    `rownames<-`(value = row_names)
}

#' Count the Total Splits in Each Plan
#'
#' Counts the total number of administrative splits.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @templateVar admin TRUE
#' @template template_nosf
#'
#' @return numeric matrix
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#' # For a single plan:
#' splits_total(plans = nh$r_2020, shp = nh, admin = county)
#'
#' # Or many plans:
#' splits_total(plans = nh_m[, 3:5], shp = nh, admin = county)
#'
splits_total <- function(plans, shp, admin) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  # prep admin ----
  admin <- rlang::eval_tidy(rlang::enquo(admin), data = shp)
  if (is.null(admin)) {
    cli::cli_abort('{.arg admin} not found in {.arg shp}.')
  }
  admin <- make_id(admin)
  nc <- length(unique(admin))

  rep(colSums(admin_splits_count(plans, admin, nd, nc)) - nc, each = nd)
}

#' Fuzzy Splits by District (Experimental)
#'
#' Not all relevant geographies nest neatly into Census blocks, including communities
#' of interest or neighborhood. For these cases, this provides a tabulation by district of
#' the number of splits. As some geographies can be split multiple times, the
#' sum of these splits may not reflect the total number of splits.
#'
#' Beware, this requires a `nbr` shape input and will be slower than checking splits in cases where
#' administrative unit nests cleanly into the geographies represented by `shp`.
#'
#' @templateVar plans TRUE
#' @templateVar shp TRUE
#' @param nbr Geographic neighborhood, community, or other unit to check splits for.
#' @param thresh Percent as decimal of an area to trim away. Default is .01, which is 1%.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return numeric matrix
#' @export
#' @concept splits
#'
#' @examples
#' data(nh)
#' data(nh_m)
#'
#' # toy example,
#' # suppose we care about the splits of the counties and they don't nest
#' nh_cty <- nh %>% dplyr::group_by(county) %>% dplyr::summarize()
#'
#' # For a single plan:
#' splits_district_fuzzy(plans = nh$r_2020, shp = nh, nbr = nh_cty)
#'
#' # Or many plans:
#' splits_district_fuzzy(plans = nh_m[, 3:5], shp = nh, nbr = nh_cty)
splits_district_fuzzy <- function(plans, shp, nbr, thresh = 0.01, epsg) {
  # prep inputs ----
  plans <- process_plans(plans)
  nd <- length(unique(plans[, 1]))

  shp <- planarize(shp, epsg)
  nbr <- planarize(nbr, epsg)
  nbr$NAME <- seq_len(nrow(nbr))

  apply(plans, MARGIN = 2, function(pl) {
    un <- shp %>%
      dplyr::mutate(plan_nbr = pl) %>%
      dplyr::as_tibble() %>%
      sf::st_as_sf() %>%
      dplyr::group_by(.data$plan_nbr) %>%
      dplyr::summarise()

    x <- lapply(seq_len(nd), function(i) {
      d <- un %>% dplyr::filter(.data$plan_nbr == i)
      nbr %>%
        geo_filter(to = d) %>%
        geo_trim(to = d, thresh) %>%
        dplyr::pull(.data$NAME) %>%
        sort()
    })

    nbr$rep <- vapply(
      nbr$NAME,
      function(n) {
        sum(sapply(x, function(y) any(n %in% y))) > 1
      },
      TRUE
    )

    nbr$rep_d <- lapply(
      nbr$NAME,
      function(n) {
        which(sapply(x, function(y) any(n %in% y)))
      }
    )

    vapply(seq_len(nd), function(i) {
      vapply(
        nbr %>%
          dplyr::filter(.data$rep) %>%
          dplyr::pull(.data$rep_d),
        function(x) {
          any(i %in% x)
        },
        integer(1)
      ) %>%
        sum()
    }, integer(1))
  })

}

# Helper
reindex <- function(plans, nd) {
  matrix(vctrs::vec_group_id(as.integer(plans)), nrow=nrow(plans), ncol=ncol(plans))
}
