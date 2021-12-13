test_that("seg_dissim works", {
  a <- seg_dissim(nh$r_2020, nh, vap_hisp, vap)
  e <- c(0.0108565782431512, 0.0108565782431512)
  expect_equal(a, e, tolerance = 1e-4)

  a <- seg_dissim(nh_m[, 1:2], nh, vap_hisp, vap)
  e <- c(0.00575128901111287, 0.00575128901111287, 0.0108565782431512, 0.0108565782431512)
  expect_equal(a, e, tolerance = 1e-4)
})
