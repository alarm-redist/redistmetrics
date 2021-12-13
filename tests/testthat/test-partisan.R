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
  e <- c(0.152495322806012, 0.152495322806012)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_decl(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(NaN, NaN, 0.152495322806012, 0.152495322806012)
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
  e <- c(-0.0817445550640845, -0.0817445550640845)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_egap(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.42490548281868, 0.42490548281868, -0.0817445550640845, -0.0817445550640845)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_egap_ep works", {
  a <- part_egap_ep(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(-0.0762476614030061, -0.0762476614030061)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_egap_ep(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.424977486779193, 0.424977486779193, -0.0762476614030061, -0.0762476614030061)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_lopsided_wins works", {
  a <- part_lopsided_wins(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.0762476614030061, 0.0762476614030061)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_lopsided_wins(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(NaN, NaN, 0.0762476614030061, 0.0762476614030061)
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
  e <- c(NaN, NaN)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_resp(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(NaN, NaN, NaN, NaN)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("part_rmd works", {
  a <- part_rmd(nh$r_2020, shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0, 0)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_rmd(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(8.50003635946404, 8.50003635946404, 8.50003635946401, 8.50003635946401)
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
  e <- c(-0.028456858885723, -0.028456858885723)
  expect_equal(a, e, tolerance = 1e-4)

  a <- part_tau_gap(nh_m[, 1:2], shp = nh, dvote = pre_20_dem_bid, rvote = pre_20_rep_tru)
  e <- c(0.98850776308525, 0.98850776308525, -0.028456858885723, -0.028456858885723)
  expect_equal(a, e, tolerance = 1e-4)
})
