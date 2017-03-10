#' Generate simulated observations following a linear model with normal random error.
#'
#' @param MyDOE is a data.frame with 6 columns (RunTechnician, ConcentrationLabel,+
#' ConcentrationValue, CalibCurve, ReplicateNumber,
#' Responses). This data.frame is produced by a DOE generating function.
#' @param intercept the intercept of the linear model.
#' @param slope the slope of the linear model.
#' @param SDrun the standard deviation between analytical runs.
#' @param SDrep the standard deviation between replicates (within an analytical run).
#' @param biais the bias between the true values and the simulated data.
#' @return The output is a data.frame with 6 columns (RunTechnician, ConcentrationLabel,
#'  ConcentrationValue, CalibCurve, ReplicateNumber,Responses).
#' Responses column is filled with the simulated data following a linear model with normal random error.
#' @examples
#' MyDOE <- calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5,
#'   ConcVect = c(0,50, 100, 125, 150, 175, 200))
#'
#' CalibDs <- norm_lin_dataset_sim(MyDOE, intercept = 1, slope = 2, SDrun=1.5,
#'  SDrep=3, biais = 0)
#' @seealso DOE generating functions such as \code{\link{calib_doe}}
#' @export
norm_lin_dataset_sim <- function(MyDOE, intercept = 0, slope = 1, SDrun = 10, SDrep = 10, biais = 0) {
  nRun = length(unique(MyDOE[, "RunTechnician"]))
  nConc = length(unique(MyDOE[, "ConcentrationValue"]))  # number of Concentration levels
  # nreplicates = length(unique(MyDOE[,'ReplicateNumber']))
  nreplicates = dim(MyDOE)[1]/(nRun * nConc)
  MeasuredConcentration = as.numeric(MyDOE[, "Response"]) * NaN

  MyDOE = as.matrix(MyDOE)

  rowId = 1
  for (i in c(1:nRun)) {

    gamma = stats::rnorm(1, mean = 0, sd = SDrun * abs(slope))

    for (j in c(1:nConc)) {
      for (k in c(1:nreplicates)) {
        epsilon = stats::rnorm(1, mean = 0, sd = SDrep * abs(slope))

        MyDOE[rowId, "Response"] = linear_link(intercept = intercept, slope = slope, as.numeric(as.character(MyDOE[rowId,
                                                                                                                   "ConcentrationValue"]))) + gamma + epsilon + biais
        MeasuredConcentration[rowId] = linear_link_inv(intercept = intercept, slope = slope, as.numeric(as.character(MyDOE[rowId,
                                                                                                                           "Response"])))

        rowId = rowId + 1
      }
    }
  }
  if (sum(dimnames(MyDOE)[[2]] == "MeasuredConcentration") == 0) {
    Ds = cbind(MyDOE, MeasuredConcentration)
  } else {
    MyDOE[, "MeasuredConcentration"] = MeasuredConcentration
    Ds = MyDOE
  }
  return(Ds)
}
