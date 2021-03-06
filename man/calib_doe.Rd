% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibration.R
\name{calib_doe}
\alias{calib_doe}
\title{Generate a design of experiment}
\usage{
calib_doe(nRun = 3, nCalibCurvesPerRun = 2, nrepCalib = 3,
  ConcVect = c(0, 50, 100, 125, 150, 175, 200))
}
\arguments{
\item{nRun}{A positive integer corresponding to the number of analytical run(s).
According to CLSI C24, a run is an interval (i.e., a period of time or series of measurements)
within which the accuracy and precision of the measuring system is expected to be stable.}

\item{nCalibCurvesPerRun}{A positive integer corresponding to the number of calibration curve(s) per analytical run.}

\item{nrepCalib}{A positive integer corresponding to the number of replicate(s).}

\item{ConcVect}{A vector containing one or several concentration or dilution in a numeric form.}
}
\value{
MyDOE (the output) is a data.frame with 6 columns (RunTechnician, ConcentrationLabel, ConcentrationValue, CalibCurve, ReplicateNumber,
Responses). Responses column contains onlys NaN at this time and will be filled by simulation or with the experimental results.
}
\description{
calib_doe return a DOE in a data.frame where responses is NaN.
}
\examples{
calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5,
 ConcVect = c(0,50,100,125,150,175,200))
}
