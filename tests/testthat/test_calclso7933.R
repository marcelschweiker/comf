# Define the test
test_that("Test calcIso7933 function outputs", {
  result_1 <- calcIso7933(Ta = 40, Tr = 40, HR = 33.85, Va = 0.3, Met = 150, Icl = 0.5, posture = 2)
  expected_1 <- data.frame(
    Dlimloss50 = 440.0,
    Dlimloss95 = 298.0,
    Dlimtre = 480.0,
    SWtotg = 6166.4,
    Tre = 37.5,
    Tsk = 35.3,
    stringsAsFactors = FALSE
  )
  expect_equal(result_1[names(expected_1)], expected_1, tolerance = 1e-8)

  result_2 <- calcIso7933(Ta = 35, Tr = 35, HR = 71, Va = 0.3, Met = 150, Icl = 0.5, posture = 2)
  expected_2 <- data.frame(
    Dlimloss50 = 385.0,
    Dlimloss95 = 256.0,
    Dlimtre = 75.0,
    SWtotg = 6934.6,
    Tre = 39.8,
    Tsk = 36.4,
    stringsAsFactors = FALSE
  )
  expect_equal(result_2[names(expected_2)], expected_2, tolerance = 1e-8)

  result_3 <- calcIso7933(Ta = 30, Tr = 50, HR = 70.65, Va = 0.3, Met = 150, Icl = 0.5, posture = 2)
  expected_3 <- data.frame(
    Dlimloss50 = 380.0,
    Dlimloss95 = 258.0,
    Dlimtre = 480.0,
    SWtotg = 7166.2,
    Tre = 37.7,
    Tsk = 35.7,
    stringsAsFactors = FALSE
  )
  expect_equal(result_3[names(expected_3)], expected_3, tolerance = 1e-8)

  result_4 <- calcIso7933(Ta = 43, Tr = 43, HR = 34.7, Va = 0.3, Met = 103, Icl = 0.5, posture = 1)
  expected_4 <- data.frame(
    Dlimloss50 = 401.0,
    Dlimloss95 = 271.0,
    Dlimtre = 480.0,
    SWtotg = 6765.1,
    Tre = 37.3,
    Tsk = 35.3,
    stringsAsFactors = FALSE
  )
  expect_equal(result_4[names(expected_4)], expected_4, tolerance = 1e-8)
})
