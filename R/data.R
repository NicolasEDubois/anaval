
#' Example of an empty calibration dataset.
#'
#' A dataset containing a design of experiment for calibration assessment (with empty results).
#'
#' ref_calib_doe.
#'
#' @format A data frame with 210 rows and 6 variables:
#' \describe{
#'   \item{RunTechnician}{A string of character with the name of the run (lab session)}
#'   \item{ConcentrationLabel}{A string of character with the lable of the concentration,
#'    eg. 'Blanc' or '050'}
#'   \item{ConcentrationValue}{A numeric corresponding to the concentration value}
#'   \item{CalibCurve}{A string of character corresponding to the id of the calibration curve}
#'   \item{ReplicateNumber}{A string of character corresponding to the id of the replicate}
#'   \item{Response}{A numeric value corresponding to the observed measurement, in equipment unit.
#'   If there is no observation the response as a NaN value.}
#' }
"ref_calib_doe"


#' Example of an empty calibration dataset.
#'
#' A dataset containing a design of experiment for testing the simulation of the generation of calibration data.
#' Summary: 1000 runs, nCalibCurvesPerRun = 1 nrepCalib = 1, Only 1 concentration 10 ppm.
#'
#' ref_sim_tot_var
#'
#' @format A data frame with 210 rows and 6 variables:
#' \describe{
#'   \item{RunTechnician}{A string of character with the name of the run (lab session)}
#'   \item{ConcentrationLabel}{A string of character with the lable of the concentration,
#'    eg. 'Blanc' or '050'}
#'   \item{ConcentrationValue}{A numeric corresponding to the concentration value}
#'   \item{CalibCurve}{A string of character corresponding to the id of the calibration curve}
#'   \item{ReplicateNumber}{A string of character corresponding to the id of the replicate}
#'   \item{Response}{A numeric value corresponding to the observed measurement, in equipment unit.
#'   If there is no observation the response as a NaN value.}
#' }
"ref_sim_tot_var"
