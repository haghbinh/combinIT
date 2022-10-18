test_that("Boik.test works", {
  expect_true(abs(Boik.test(MVGH, nsim = 1000)$statistic-0.50012362)<0.001,)
})
