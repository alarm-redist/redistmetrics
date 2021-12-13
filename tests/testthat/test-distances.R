test_that("dist_info works", {
  a <- dist_info(nh$r_2020, nh, pop)
  e <- 0
  expect_equal(a, e, tolerance = 1e-4)

  a <- dist_info(nh_m[, 1:2], nh, pop)
  e <- structure(c(0, 1.17049464100364, 1.17049464100364, 0),
                 .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL))
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("dist_man works", {
  a <- dist_man(nh$r_2020)
  e <- 0
  expect_equal(a, e, tolerance = 1e-4)

  a <- dist_man(nh_m[, 1:2])
  e <- structure(c(0, 76, 76, 0),
                 .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL))
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("dist_euc works", {
  a <- dist_euc(nh$r_2020)
  e <- 0
  expect_equal(a, e, tolerance = 1e-4)

  a <- dist_euc(nh_m[, 1:2])
  e <- structure(c(0, 8.71779788708135, 8.71779788708135, 0),
                 .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL))
  expect_equal(a, e, tolerance = 1e-4)
})

test_that("dist_ham works", {
  a <- dist_ham(nh$r_2020)
  e <- 0
  expect_equal(a, e, tolerance = 1e-4)

  a <- dist_ham(nh_m[, 1:2])
  e <- structure(c(0L, 76L, 76L, 0L),
                 .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL))
  expect_equal(a, e, tolerance = 1e-4)
})
