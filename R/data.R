#' New Hampshire Election and Demographic Data
#'
#' This data set contains demographic, election, and geographic information for
#' the 326 voting tabulation districts in New Hampshire in 2020.
#'
#' @name nh
#' @usage data("nh")
#' @format A tibble with 326 rows and 45 columns
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
#' - pre_16_rep_tru: Votes for Republican president 2016
#' - pre_16_dem_cli: Votes for Democratic president 2016
#' - uss_16_rep_ayo: Votes for Republican senate 2016
#' - uss_16_dem_has: Votes for Democratic senate 2016
#' - gov_16_rep_sun: Votes for Republican governor 2016
#' - gov_16_dem_van: Votes for Democratic governor 2016
#' - gov_18_rep_sun: Votes for Republican governor 2018
#' - gov_18_dem_kel: Votes for Democratic governor 2018
#' - pre_20_dem_bid: Votes for Democratic president 2020
#' - pre_20_rep_tru: Votes for Republican president 2020
#' - uss_20_dem_sha: Votes for Democratic senate 2020
#' - uss_20_rep_mes: Votes for Republican senate 2020
#' - gov_20_dem_fel: Votes for Democratic governor 2020
#' - gov_20_rep_sun: Votes for Republican governor 2020
#' - arv_16: Average Republican vote 2016
#' - adv_16: Average Democratic vote 2016
#' - arv_18: Average Republican vote 2018
#' - adv_18: Average Democratic vote 2018
#' - arv_20: Average Republican vote 2020
#' - adv_20: Average Democratic vote 2020
#' - nrv: Normal Republican vote
#' - ndv: Normal Democratic vote
#' - geometry: sf geometry, simplified for size using rmapshaper
#' - r_2020: Republican proposed plan for 2020 Congressional districts
#' - d_2020: Democratic proposed plan for 2020 Congressional districts
#' - adj: zero-indexed adjacency graph
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
#' @concept data
#' @examples
#' data(nh)
#'
NULL

#' New Hampshire Election and Demographic Data as a `redist_map`
#'
#' This data set contains demographic, election, and geographic information for
#' the 326 voting tabulation districts in New Hampshire in 2020.
#'
#' @name nh_map
#' @usage data("nh_map")
#' @format A redist_map with 326 rows and 45 columns
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
#' - pre_16_rep_tru: Votes for Republican president 2016
#' - pre_16_dem_cli: Votes for Democratic president 2016
#' - uss_16_rep_ayo: Votes for Republican senate 2016
#' - uss_16_dem_has: Votes for Democratic senate 2016
#' - gov_16_rep_sun: Votes for Republican governor 2016
#' - gov_16_dem_van: Votes for Democratic governor 2016
#' - gov_18_rep_sun: Votes for Republican governor 2018
#' - gov_18_dem_kel: Votes for Democratic governor 2018
#' - pre_20_dem_bid: Votes for Democratic president 2020
#' - pre_20_rep_tru: Votes for Republican president 2020
#' - uss_20_dem_sha: Votes for Democratic senate 2020
#' - uss_20_rep_mes: Votes for Republican senate 2020
#' - gov_20_dem_fel: Votes for Democratic governor 2020
#' - gov_20_rep_sun: Votes for Republican governor 2020
#' - arv_16: Average Republican vote 2016
#' - adv_16: Average Democratic vote 2016
#' - arv_18: Average Republican vote 2018
#' - adv_18: Average Democratic vote 2018
#' - arv_20: Average Republican vote 2020
#' - adv_20: Average Democratic vote 2020
#' - nrv: Normal Republican vote
#' - ndv: Normal Democratic vote
#' - r_2020: Republican proposed plan for 2020 Congressional districts
#' - d_2020: Democratic proposed plan for 2020 Congressional districts
#' - adj: zero-indexed adjacency graph
#' - geometry: sf geometry, simplified for size using rmapshaper
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
#' @concept data
#' @examples
#' data(nh_map)
#'
NULL

#' Redistricting Plans for New Hampshire as `redist_plans`
#'
#'
#' This data set contains two reference plans (`d_2020` and `r_2020`) and
#' 50 simulated plans for New Hampshire, based on 2020 demographics, simulated at
#' a population tolerance of 0.05%.
#'
#' @name nh_plans
#' @usage data("nh_plans")
#' @format A redist_plans with 104 rows and 3 columns
#' - draw: factor identifying the reference plans (`d_2020` and `r_2020`) and 50 simulted plans
#' - district: district number (`1` or `2`)
#' - total_pop: total population in the district
#'
#' @concept data
#' @examples
#' data(nh_plans)
#'
NULL

#' Redistricting Plans for New Hampshire as `matrix`
#'
#'
#' This data set contains two reference plans (`d_2020` and `r_2020`) and
#' 50 simulated plans for New Hampshire, based on 2020 demographics, simulated at
#' a population tolerance of 0.05%.
#'
#' @name nh_m
#' @usage data("nh_m")
#' @format A matrix with 52 columns and 326 rows where each column is a plan
#'
#' @concept data
#' @examples
#' data(nh_m)
#'
NULL
