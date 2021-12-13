test_that("process_plans works", {
  a <- process_plans(nh_m)
  expect_equal(a, nh_m)
})
