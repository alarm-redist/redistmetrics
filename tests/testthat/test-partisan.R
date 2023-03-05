test_that("part_bias works", {
  a <- part_bias(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_bias(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0, 0, 0)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_decl works", {
  a <- part_decl(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0332887482661483, 0.0332887482661483)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_decl(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(NaN, NaN, 0.0332887482661483, 0.0332887482661483)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_decl_simple works", {
  a <- part_decl_simple(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.152471201485029, 0.152471201485029)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_decl_simple(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(NaN, NaN, 0.152471201485029, 0.152471201485029)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_dseats works", {
  a <- part_dseats(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(1L, 1L)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_dseats(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(2L, 2L, 1L, 1L)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_dvs works", {
  a <- part_dvs(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.491471811541016, 0.58477584986199)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_dvs(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.532085831110753, 0.542936682110054, 0.491471811541016, 0.58477584986199)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_egap works", {
  a <- part_egap(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0817197636722627, 0.0817197636722627)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_egap(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(-0.425023874363298, -0.425023874363298, 0.0817197636722627, 0.0817197636722627)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_egap_ep works", {
  a <- part_egap_ep(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0762356007425145, 0.0762356007425145)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_egap_ep(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(-0.424981492869942, -0.424981492869942, 0.0762356007425145, 0.0762356007425145)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_lop_wins works", {
  a <- part_lop_wins(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0762356007425145, 0.0762356007425145)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_lop_wins(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(NaN, NaN, 0.0762356007425145, 0.0762356007425145)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_mean_median works", {
  a <- part_mean_median(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_mean_median(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0, 0, 0)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_resp works", {
  a <- part_resp(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru, v = 0.5)
  e <- c(0, 0)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_resp(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0, 0, 0)
  expect_equal(a, e, tolerance = 1e-4)


  a <- part_resp(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru,
                 bandwidth = 0.05)
  e <- c(20, 20, 0, 0)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_rmd works", {
  a <- part_rmd(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_rmd(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(8.49534111881552, 8.49534111881552, 8.49534111881552, 8.49534111881552)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_sscd works", {
  a <- part_sscd(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.90859786336743, 0.90859786336743)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_sscd(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0603019836926911, 0.0603019836926911, 0.90859786336743, 0.90859786336743)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_tau_gap works", {
  a <- part_tau_gap(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0284448215286677, 0.0284448215286677)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_tau_gap(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(-0.98850905428411, -0.98850905428411, 0.0284448215286677, 0.0284448215286677)
  expect_equal(a, e, tolerance = 1e-4)
})
