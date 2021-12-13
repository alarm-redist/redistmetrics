test_that("comp_polsby works", {
  a <- comp_polsby(plans = nh$r_2020, shp = nh)
  e <- c(0.232437498551951, 0.15827627110717)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_polsby(plans = nh_m[, 1:2], shp = nh)
  e <- c(0.184495499505702, 0.179642567783063, 0.232437498551951, 0.15827627110717)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_bc works", {
  a <- comp_bc(plans = nh$r_2020, shp = nh)
  e <- c(0.792135014565373, 0.833023452308662)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_bc(plans = nh_m[, 1:2], shp = nh)
  e <- c(0.817730679839768, 0.705558754903565, 0.792135014565373, 0.833023452308662)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_ch works", {
  a <- comp_ch(plans = nh$r_2020, shp = nh)
  e <- c(0.676396448650639, 0.662673827045224)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_ch(plans = nh_m[, 1:2], shp = nh)
  e <- c(0.619365024147118, 0.708346785033898, 0.676396448650639, 0.662673827045224)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_edges_rem works", {
  a <- comp_edges_rem(plans = nh$r_2020, shp = nh, adj = nh$adj)
  e <- c(73, 73)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_edges_rem(plans = nh_m[, 1:2], shp = nh, adj = nh$adj)
  e <- c(75, 75, 73, 73)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_frac_kept works", {
  a <- comp_frac_kept(nh$r_2020, shp = nh, adj = nh$adj)
  e <- c(0.958475540386803, 0.958475540386803)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_frac_kept(nh_m[, 1:2], shp = nh, adj = nh$adj)
  e <- c(0.957337883959044, 0.957337883959044, 0.958475540386803, 0.958475540386803)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_fh works", {
  a <- comp_fh(nh$r_2020, shp = nh, total_pop = nh$pop)
  e <- c(5.37469076047245e+22, 5.37469076047245e+22)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_fh(nh_m[, 1:2], shp = nh, total_pop = nh$pop)
  e <- c(d_2020 = 5.12945237027316e+22, d_2020 = 5.12945237027316e+22,
         r_2020 = 5.37469076047245e+22, r_2020 = 5.37469076047245e+22)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_log_st works", {
  a <- comp_log_st(nh$r_2020, shp = nh, adj = nh$adj, counties = nh$county)
  e <- c(-Inf, -Inf)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_log_st(nh_m[, 1:2], shp = nh, adj = nh$adj, counties = nh$county)
  e <- c(-Inf, -Inf, -Inf, -Inf)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_lw works", {
  a <- comp_lw(nh$r_2020, shp = nh)
  e <- c(0.916157575884433, 0.522345452389112)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_lw(nh_m[, 1:2], shp = nh)
  e <- c(0.576300432861704, 0.431270012221621, 0.916157575884433, 0.522345452389112)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_reock works", {
  a <- comp_reock(nh$r_2020, shp = nh)
  e <- c(0.444465285671442, 0.250215157908113)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_reock(nh_m[, 1:2], shp = nh)
  e <- c(0.302484301653267, 0.234708920045101, 0.444465285671442, 0.250215157908113)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("comp_schwartz works", {
  a <- comp_schwartz(nh$r_2020, shp = nh)
  e <- c(0.482117722710907, 0.39783950420637)
  expect_equal(a, e, tolerance = 1e-4)

  a <- comp_schwartz(nh_m[, 1:2], shp = nh)
  e <- c(0.429529393063737, 0.423842621480028, 0.482117722710907, 0.39783950420637)
  expect_equal(a, e, tolerance = 1e-4)
})
