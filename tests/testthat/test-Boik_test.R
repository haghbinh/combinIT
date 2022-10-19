test_that("Boik_test works", {
  expect_true(abs(Boik_test(MVGH, nsim = 1000)$statistic-0.50012362)<0.001,)
})
