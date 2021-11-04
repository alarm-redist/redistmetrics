#' Maine Election and Demographic Data
#'
#' This data set contains demographic, election, and geographic information for
#' the 209 voting tabulation districts in Maine in 2020.
#'
#' @name maine
#' @usage data("maine")
#' @format A tibble with 209 and 41 columns
#' - GEOID20: 2020 VTD GEOID
#' - state: state name
#' - county: county name
#' - vtd: VTD portion of GEOID
#' - pop: total population
#' - pop_hisp: Hispanic population
#' - pop_white: White, not Hispanic population
#' - pop_black: Black, not Hispanic population
#' - pop_aian: American Indian and Alaska Native, not Hispanic population
#' - pop_asian: Asian, not Hispanic population
#' - pop_nhpi: Native Hawaiian and Pacific Islander, not Hispanic population
#' - pop_other: other race, not Hispanic population
#' - pop_two: multi-race, not Hispanic population
#' - vap: total voting-age population
#' - vap_hisp: Hispanic voting-age population
#' - vap_white: White, not Hispanic voting-age population
#' - vap_black: Black, not Hispanic voting-age population
#' - vap_aian: American Indian and Alaska Native, not Hispanic voting-age population
#' - vap_asian: Asian, not Hispanic voting-age population
#' - vap_nhpi: Native Hawaiian and Pacific Islander, not Hispanic voting-age population
#' - vap_other: other race, not Hispanic voting-age population
#' - vap_two: multi-race, not Hispanic voting-age population
#' - pre_16_dem_cli: Votes for Democratic president 2016
#' - pre_16_rep_tru: Votes for Republican president 2016
#' - uss_18_dem_rin: Votes for Democratic senate 2018
#' - uss_18_rep_bra: Votes for Republican senate 2018
#' - gov_18_dem_mil: Votes for Democratic governor 2018
#' - gov_18_rep_moo: Votes for Republican governor 2018
#' - pre_20_dem_bid: Votes for Democratic president 2020
#' - pre_20_rep_tru: Votes for Democratic president 2020
#' - uss_20_dem_gid: Votes for Democratic senate 2020
#' - uss_20_rep_col: Votes for Democratic senate 2020
#' - arv_16: Average Republican vote 2016
#' - adv_16: Average Democratic vote 2016
#' - arv_18: Average Republican vote 2018
#' - adv_18: Average Democratic vote 2018
#' - arv_20: Average Republican vote 2020
#' - adv_20: Average Democratic vote 2020
#' - nrv: Normal Republican vote
#' - ndv: Normal Democratic vote
#' - geometry: sf geometry
#'
#' @references
#' Voting and Election Science Team, 2020, "2020 Precinct-Level Election Results",
#' <https://doi.org/10.7910/DVN/K7760H>, Harvard Dataverse, V23
#'
#' Voting and Election Science Team, 2018, "2016 Precinct-Level Election Results",
#' <https://doi.org/10.7910/DVN/NH5S2I>, Harvard Dataverse, V71
#'
#' Voting and Election Science Team, 2019, "2018 Precinct-Level Election Results",
#' <https://doi.org/10.7910/DVN/UBKYRU>, Harvard Dataverse, V48
#'
#' Kenny & McCartan (2021, Aug. 10). ALARM Project: 2020 Redistricting Data Files.
#' Retrieved from <https://github.com/alarm-redist/census-2020/>
#'
#' @md
#' @concept data
#' @examples
#' data(maine)
#'
NULL
