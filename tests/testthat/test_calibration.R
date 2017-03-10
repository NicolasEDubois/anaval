context("calibration")

test_that("DOE output is a data.frame", {
    expect_is(calib_doe(nRun = 3, nCalibCurvesPerRun = 2, nrepCalib = 3, ConcVect = c(0,
        50, 100, 125, 150, 175, 200)), "data.frame")
})

test_that("DOE data.frame has the right number of rows", {
    expect_equal(dim(calib_doe(nRun = 3, nCalibCurvesPerRun = 1, nrepCalib = 1, ConcVect = c(0)))[1],
        3)
    expect_equal(dim(calib_doe(nRun = 1, nCalibCurvesPerRun = 2, nrepCalib = 1, ConcVect = c(0)))[1],
        2)
    expect_equal(dim(calib_doe(nRun = 1, nCalibCurvesPerRun = 1, nrepCalib = 4, ConcVect = c(0)))[1],
        4)
    expect_equal(dim(calib_doe(nRun = 1, nCalibCurvesPerRun = 1, nrepCalib = 1, ConcVect = c(0,
        100, 200)))[1], 3)
    expect_equal(dim(calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5, ConcVect = c(0,
        50, 100, 125, 150, 175, 200)))[1], 210)
    expect_equal(dim(calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5, ConcVect = c(0,
        50, 100, 125, 150, 175, 200)))[2], 6)
})

test_that("The DOE data.frame matches the reference data.frame", {
  data("ref_calib_doe")
  test_doe = calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5, ConcVect = c(0, 50, 100, 125, 150, 175, 200))
  expect_equal(test_doe, ref_calib_doe)
})
