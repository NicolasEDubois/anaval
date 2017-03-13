% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibration.R
\name{calib_coef}
\alias{calib_coef}
\title{Estimate the fixed coeficient of a calibration dataset.}
\usage{
calib_coef(DS, LowerRange = 0, UpperRange = 200, MyModel = "lm",
  Mode = "Definition of a range of parameters", diagnostics = TRUE)
}
\arguments{
\item{DS}{is a data.frame with 6 columns (RunTechnician, ConcentrationLabel, ConcentrationValue, CalibCurve, ReplicateNumber,
Responses). Responses column might have been filled by simulation or with the experimental results.}

\item{LowerRange}{A  real number corresponding to the lower bound of the range in which the modelisation applies.}

\item{UpperRange}{A  real number corresponding to the upper bound of the range in which the modelisation applies.}

\item{MyModel}{A string of character indicating the type of model to apply. Currently only "lm" for linear model.}

\item{ConcVect}{A vector containing one or several concentration or dilution in a numeric form.}
}
\value{
Resp A list with the estimate of the fixed effects, the fixed effect estimates of all calibration curves and data usefull for diagnostics)
}
\description{
Calib_coef return a list with the estimation of the fixed coefficients and model data for diagnostics.
}
\examples{
RespCalib = calib_coef(DS = CalibDs,LowerRange=0,UpperRange=200,MyModel='lm', diagnostics='TRUE')
}