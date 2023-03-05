test_that("compet_talisman works", {
  a <- compet_talisman(nh$r_2020, nh, pre_16_rep_tru, pre_16_dem_cli)
  e <- c(0.0512151870528045, 0.0512151870528045)
  expect_equal(a, e, tolerance = 1e-4)

  a <- compet_talisman(nh_m[, 1:2], nh, pre_16_rep_tru, pre_16_dem_cli)
  e <- c(0.00872398279100295, 0.00872398279100295, 0.0512151870528045,
         0.0512151870528045)
  expect_equal(a, e, tolerance = 1e-4)
})
