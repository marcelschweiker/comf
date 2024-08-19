library(testthat)
library(comf) 

test_that("calcAD calculates ankle draft correctly", {
  output_si <- capture.output(calcAD(ta = 25, tr = 25, vel = 0.2, rh = 50, clo = 0.5, met = 1.2, vAnkle = 0.3))
  expect_true(any(grepl("Ankle_draft_ppd: 18.5", output_si)))
  expect_true(any(grepl("Acceptability: TRUE", output_si)))

  output_ip <- capture.output(calcAD(ta = (77 - 32) * 5/9, tr = (77 - 32) * 5/9, vel = 0.2, rh = 50, clo = 0.5, met = 1.2, vAnkle = 0.4))
  expect_true(any(grepl("Ankle_draft_ppd: 23.6", output_ip)))
  expect_true(any(grepl("Acceptability: FALSE", output_ip)))

  expect_warning(calcAD(ta = 25, tr = 25, vel = 0.3, rh = 50, clo = 0.5, met = 1.2, vAnkle = 0.3))
})