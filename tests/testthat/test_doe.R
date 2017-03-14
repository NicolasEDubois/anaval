context("DOE")

test_that("DOE_Run_Repl_Conc output is a matrix", {
  resp = DOE_Run_Repl_Conc(nRun = 3, nreplicates = 2, ConcVect = c(0, 50, 100, 125,150, 175, 200 ),
          Threshold='NaN', factorlist=c('RunTechnician','ConcentrationLabel','ConcentrationValue',
                                        'ReplicateNumber','Status','Response'))
  expect_is(resp, "matrix")
})

test_that("DOE data.frame has the right number of rows", {
  resp = DOE_Run_Repl_Conc(nRun = 3, nreplicates = 2, ConcVect = c(0, 50, 100, 125,150, 175, 200 ),
                           Threshold='NaN', factorlist=c('RunTechnician','ConcentrationLabel','ConcentrationValue',
                                                         'ReplicateNumber','Status','Response'))
  expect_equal(dim(resp)[1],42)
  expect_equal(unique(resp[,'RunTechnician']),c("Run001","Run002","Run003"))
  expect_equal(unique(resp[,'ConcentrationValue']),c("0", "50", "100", "125", "150", "175", "200"))
  expect_equal(unique(resp[,'ReplicateNumber']),c("1", "2"))
})
