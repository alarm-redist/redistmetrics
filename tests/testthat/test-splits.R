test_that("splits_admin works", {
  a <- splits_admin(nh$r_2020, nh, county)
  e <- c(6L, 6L)
  expect_equal(a, e, tolerance = 1e-4)

  a <- splits_admin(nh_m[, 1:2], nh, county)
  e <- c(5L, 5L, 6L, 6L)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("splits_sub_admin works", {
  a <- splits_sub_admin(nh$r_2020, nh, county)
  e <- c(6L, 6L)
  expect_equal(a, e, tolerance = 1e-4)

  a <- splits_sub_admin(nh_m[, 1:2], nh, county)
  e <- c(5L, 5L, 6L, 6L)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("splits_multi works", {
  a <- splits_multi(nh$r_2020, nh, county)
  e <- c(0L, 0L)
  expect_equal(a, e, tolerance = 1e-4)

  a <- splits_multi(nh_m[, 1:2], nh, county)
  e <- c(0L, 0L, 0L, 0L)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("splits_count works", {
  a <- splits_count(nh$r_2020, nh, county)
  e <- structure(c(1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L),
                 dim = c(10L, 1L),
                 dimnames = list(
                   c("Belknap County", "Carroll County", "Cheshire County",
                     "Coos County", "Grafton County", "Hillsborough County", "Merrimack County",
                     "Rockingham County", "Strafford County", "Sullivan County"),
                   NULL))
  expect_equal(a, e, tolerance = 1e-4)

  a <- splits_count(nh_m[, 1:2], nh, county)
  e <- structure(c(2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 1L,
                   1L, 2L, 2L, 2L, 2L, 2L, 1L),
                 dim = c(10L, 2L),
                 dimnames = list(
                   c("Belknap County", "Carroll County", "Cheshire County",
                     "Coos County", "Grafton County", "Hillsborough County", "Merrimack County",
                     "Rockingham County", "Strafford County", "Sullivan County"
                   ), NULL))
  expect_equal(a, e, tolerance = 1e-4)
})


test_that("splits_total works", {
  a <- splits_total(nh$r_2020, nh, county)
  e <- c(6, 6)
  expect_equal(a, e, tolerance = 1e-4)

  a <- splits_total(nh_m[, 1:2], nh, county)
  e <- c(5, 5, 6, 6)
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("splits_sub_total works", {
  a <- splits_sub_total(nh$r_2020, nh, county)
  e <- c(6, 6)
  expect_equal(a, e, tolerance = 1e-4)

  a <- splits_sub_total(nh_m[, 1:2], nh, county)
  e <- c(5, 5, 6, 6)
  expect_equal(a, e, tolerance = 1e-4)
})

