test_that("calculate calcWbgt with parameters", {
  expect_equal(
    isTRUE(
      all.equal(
        calcWbgt(twb = 25, tg = 32, tdb = 20, with_solar_load = TRUE),
        25.9
      )
    ),
    TRUE
  )
  expect_error(
    calcWbgt(twb = 25, tg = 32, with_solar_load = TRUE),
    "Please enter the dry bulb air temperature",
  )

  # data from Table D.1 ISO 7243
  expect_equal(
    isTRUE(all.equal(calcWbgt(twb = 17.3, tg = 40, round = TRUE), 24.1)),
    TRUE
  )
  expect_equal(
    isTRUE(all.equal(calcWbgt(twb = 21.1, tg = 55, round = TRUE), 31.3)),
    TRUE
  )
  expect_equal(
    isTRUE(all.equal(calcWbgt(twb = 16.7, tg = 40, round = TRUE), 23.7)),
    TRUE
  )
})

test_that("calculate calcWbgt with twb and tg", {
  expect_equal(isTRUE(all.equal(calcWbgt(25, 30), 26.5)), TRUE)
})

test_that("calculate calcWbgt with twb, tg, and tdb", {
  expect_equal(isTRUE(all.equal(calcWbgt(25, 30, 20), 26.5)), TRUE)
})

test_that("round calcWbgt to one decimal place", {
  expect_equal(isTRUE(all.equal(calcWbgt(25, 30, round = FALSE), 26.5)), TRUE)
})

test_that("calculate calcWbgt with twb and tg set to zero", {
  expect_equal(isTRUE(all.equal(calcWbgt(0, 0), 0.0)), TRUE)
})

test_that("calculate calcWbgt with tdb set to zero", {
  expect_equal(isTRUE(all.equal(calcWbgt(25, 30, 0), 26.5)), TRUE)
})

test_that("calculate calcWbgt with twb and tg set to None", {
  expect_error(calcWbgt(NULL, NULL))
})
