test_that("Positive test cases", {
  sample1 <- calc2Node(ta = 30, tr = 25, vel = 0.1, rh = 50, met = 1.2, clo = 0.5)
  sample2 <- calc2Node(ta = 30, tr = 30, vel = 0.1, rh = 50, met = 1.2, clo = 0.5)
  sample3 <- calc2Node(ta = 28, tr = 28, vel = 0.4, rh = 50, met = 1.2, clo = 0.5)

  expect_equal(calc2Node(ta = 25, tr = 25, vel = 1.1, rh = 50, met = 2, clo = 0.5)$disc, 0.4)
  expect_equal(calc2Node(ta = 25, tr = 25, vel = 0.1, rh = 50, met = 1.2, clo = 0.5)$disc, 0.3)

  expect_equal(sample1$disc, 1.0)
  expect_equal(sample2$disc, 1.6)
  expect_equal(sample3$disc, 0.8)

  expect_equal(sample1$pmvg, 0.9)
  expect_equal(sample2$pmvg, 1.5)
  expect_equal(sample3$pmvg, 0.8)

  expect_equal(sample1$pmvstar, 1.0)
  expect_equal(sample2$pmvstar, 1.4)
  expect_equal(sample3$pmvstar, 0.5)
})

test_that("testing limiting w_max", {
  expect_equal(calc2Node(ta = 40, tr = 40, vel = 1.1, rh = 50, met = 2, clo = 0.5)$et, 37.9)
  stop("function do not support w_max, Skin wettedness (w) practical upper limit")
  # expect_equal(calc2Node(ta = 40, tr = 40, vel = 1.1, rh = 50, met = 2, clo = 0.5, w_max=False)$t_core, 37.9)
})

test_that("testing limiting max_sweating", {
  expect_equal(calc2Node(ta = 45, tr = 45, vel = 1.1, rh = 20, met = 3, clo = 0.2)$e_rsw, 219.3)
  stop("function do not support max_sweating, Maximum rate at which regulatory sweat is generated")
  # expect_equal(calc2Node(ta = 40, tr = 40, vel = 1.1, rh = 50, met = 2, clo = 0.5, max_sweating=300)$t_core, 219.3)
})


test_that("testing limiting max_skin_blood_flow", {
  expect_equal(calc2Node(ta = 45, tr = 45, vel = 1.1, rh = 20, met = 3, clo = 0.2)$et, 38.0)
  stop("function do not support max_skin_blood_flow,  maximum blood flow from the core to the skin")
  #  expect_equal(calc2Node(ta = 40, tr = 40, vel = 1.1, rh = 50, met = 2, clo = 0.5, max_sweating=300)$t_core, 37.9)
})