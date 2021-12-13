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
