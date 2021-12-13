test_that("inc_pairs works", {
  fake_inc <- rep(FALSE, nrow(nh))
  fake_inc[3:4] <- TRUE

  a <- inc_pairs(nh$r_2020, nh, fake_inc)
  e <- c(1, 1)
  expect_equal(a, e, tolerance = 1e-4)

  a <- inc_pairs(nh_m[, 1:2], nh, fake_inc)
  e <- c(d_2020 = 0L, d_2020 = 0L, r_2020 = 1L, r_2020 = 1L)
    expect_equal(a, e, tolerance = 1e-4)
})
