test_that("multiplication works", {
  expect_equal(OR_95CI(1.0055, 0.2534, 0.05, 2),"2.73 (1.66, 4.49)")
})

