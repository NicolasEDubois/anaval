% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{calib_data}
\alias{calib_data}
\title{Example of a calibration dataset with data.}
\format{A data frame with 210 rows and 6 variables:
\describe{
  \item{RunTechnician}{A string of character with the name of the run (lab session)}
  \item{ConcentrationLabel}{A string of character with the lable of the concentration,
   eg. 'Blanc' or '050'}
  \item{ConcentrationValue}{A numeric corresponding to the concentration value}
  \item{CalibCurve}{A string of character corresponding to the id of the calibration curve}
  \item{ReplicateNumber}{A string of character corresponding to the id of the replicate}
  \item{Response}{A numeric value corresponding to the observed measurement, in equipment unit.
  If there is no observation the response as a NaN value.}
}}
\usage{
calib_data
}
\description{
A dataset containing a dataset for testing estimation of the fixed effect of calibration.
Summary: 3 runs, nCalibCurvesPerRun = 2 nrepCalib = 1, 7 concentration level 0, 50, 100, 125, 150, 175, 200 ppm.
}
\details{
calib_data
}
\keyword{datasets}
