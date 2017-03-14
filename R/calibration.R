#' Generate a design of experiment
#'
#' calib_doe return a DOE in a data.frame where responses is NaN.
#'
#' @param nRun A positive integer corresponding to the number of analytical run(s).
#' According to CLSI C24, a run is an interval (i.e., a period of time or series of measurements)
#' within which the accuracy and precision of the measuring system is expected to be stable.
#'
#' @param nCalibCurvesPerRun A positive integer corresponding to the number of calibration curve(s) per analytical run.
#'
#' @param nrepCalib A positive integer corresponding to the number of replicate(s).
#'
#' @param ConcVect A vector containing one or several concentration or dilution in a numeric form.
#' @return MyDOE (the output) is a data.frame with 6 columns (RunTechnician, ConcentrationLabel, ConcentrationValue, CalibCurve, ReplicateNumber,
#'Responses). Responses column contains onlys NaN at this time and will be filled by simulation or with the experimental results.
#' @examples
#' calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5,
#'  ConcVect = c(0,50,100,125,150,175,200))
#' @export
calib_doe <- function(nRun = 3, nCalibCurvesPerRun = 2, nrepCalib = 3, ConcVect = c(0, 50,
    100, 125, 150, 175, 200)) {
    # DOE
    factorlist <- c("RunTechnician", "ConcentrationLabel", "ConcentrationValue", "CalibCurve",
        "ReplicateNumber", "Response")
    f1 <- paste("Run", sprintf("%03d", c(1:nRun)), sep = "")  # 1st factor levels

    f2 <- ConcVect  # 2nd factor levels

    k1 <- length(f1)  # number of 1st factors
    k2 <- length(f2)  # number of 2nd factors

    f3 <- c(1:nCalibCurvesPerRun)

    n <- nrepCalib * nCalibCurvesPerRun

    MyDOE <- matrix(NaN, nrow = k1 * k2 * n, ncol = length(factorlist))

    colnames(MyDOE) <- c(factorlist)

    # Simulation of the response (signal)
    rowId <- 1
    for (i in c(1:k1)) {
        for (k in c(1:nCalibCurvesPerRun)) {
            for (l in c(1:nrepCalib)) {
                for (j in c(1:k2)) {
                  MyDOE[rowId, "RunTechnician"] = f1[i]
                  MyDOE[rowId, "ConcentrationLabel"] = paste(sprintf("%03d", as.numeric(f2[j])),
                    sep = "")
                  MyDOE[rowId, "ConcentrationValue"] = f2[j]
                  MyDOE[rowId, "CalibCurve"] = paste(f1[i], sprintf("%02d", as.numeric(f3[k])),
                    sep = "_")
                  MyDOE[rowId, "ReplicateNumber"] = paste(sprintf("%02d", l, sep = ""))
                  MyDOE[rowId, "Response"] = NaN
                  rowId <- rowId + 1
                }
            }
        }
    }
    return(as.data.frame(MyDOE))
}

#' Estimate the fixed coeficient of a calibration dataset.
#'
#' Calib_coef return a list with the estimation of the fixed coefficients and model data for diagnostics.
#' @param DS is a data.frame with 6 columns (RunTechnician, ConcentrationLabel, ConcentrationValue, CalibCurve, ReplicateNumber,
#'Responses). Responses column might have been filled by simulation or with the experimental results.
#' @param LowerRange A  real number corresponding to the lower bound of the range in which the modelisation applies.
#' @param UpperRange A  real number corresponding to the upper bound of the range in which the modelisation applies.
#' @param MyModel A string of character indicating the type of model to apply. Currently only "lm" for linear model.
#' @return Resp A list with the estimate of the fixed effects, the fixed effect estimates of all calibration curves and data usefull for diagnostics)
#' @examples
#' data(calib_data)
#' RespCalib = calib_coef(DS = calib_data,LowerRange=0,UpperRange=200,MyModel='lm')
#' @export
calib_coef <- function(DS, LowerRange = 0, UpperRange = 200, MyModel = "lm") {
  DS2 = as.data.frame(DS[as.numeric(DS[, "ConcentrationValue"]) >= LowerRange & as.numeric(DS[, "ConcentrationValue"]) <=
                           UpperRange, ])

  ListCalCurve = unique(DS2[, "CalibCurve"])

  ncal = length(ListCalCurve)

  if (MyModel == "lm") {

    for (i in c(1:ncal)) {
      Ds3 = DS2[DS2$CalibCurve == ListCalCurve[i], ]

      Response = as.numeric(as.character(Ds3$Response))
      Conc = as.numeric(as.character(Ds3$ConcentrationValue))

      lmModel <- stats::lm(Response ~ Conc)

      Full = stats::lm(Response ~ as.factor(Conc))  #fit full model
      LackOfFitTest = stats::anova(lmModel, Full)  #get lack-of-fit test

      if (i == 1) {
        MatrixfixedCoef = matrix(NaN, ncol = length(lmModel$coefficients), nrow = ncal)
        colnames(MatrixfixedCoef) = c("Intercept", "Slope")

        PredResid = cbind(lmModel$fitted.values, lmModel$residuals)
        colnames(PredResid) = c("fittedValues", "resisudals")

        LOFpValVect = c(1:ncal) * NaN
      } else {
        PredResidTemp = cbind(lmModel$fitted.values, lmModel$residuals)
        PredResid = rbind(PredResid, PredResidTemp)
        rm(PredResidTemp)
      }

      MatrixfixedCoef[i, ] = lmModel$coefficients
      LOFpValVect[i] = LackOfFitTest$`Pr(>F)`[2]
    }

    estInt = mean(MatrixfixedCoef[, 1])
    estSlope = mean(MatrixfixedCoef[, 2])

    FixedEffectEst = c(estInt = estInt, estSlope = estSlope)

    Resp = list(FixedEffectEst = FixedEffectEst, MatrixfixedCoef, PredResid = PredResid, LOFpValVect = LOFpValVect)

  }
  return(Resp)
}

#' Provides and displays diagnostics
#'
#' calib_diagnostics returns diagnostics to assess the validity of the calibration model.
#' @param PredResid is a matrix with two columns corresponding to the fitted values and the residuals.
#' @param LOFpValVect is a numeric vector with the p-values of the lack of fit test for each calibration curves.
#' @return N/A text and plot for Rmarkdown
#' @examples
#' data(calib_data)
#' RespCalib = calib_coef(DS = calib_data,LowerRange=0,UpperRange=200,MyModel='lm')
#' calib_diagnostics(RespCalib$PredResid, RespCalib$LOFpValVect)
#' @export
calib_diagnostics <- function(PredResid, LOFpValVect) {
  graphics::plot(PredResid[, 1], PredResid[, 2], xlab = "Predicted value", ylab = "Pearson residuals")

  stats::qqnorm(PredResid[, 2])
  skewness = moments::skewness(PredResid[, 2])
  kurtosis = moments::kurtosis(PredResid[, 2])
  graphics::text(2, 0, paste("skewness: ", round(skewness, 2), sep = ""))
  graphics::text(2, -5, paste("kurtosis: ", round(kurtosis, 2), sep = ""))

  graphics::boxplot(LOFpValVect, main = "Lack-of-Fit tests", xlab = "Lack-of-Fit tests of all calibration curves",
          ylab = "p-value", xlim = c(0, 10), ylim = c(0, 1.1))
  graphics::text(6, 1.05, paste("p-val < 5% for ", sum(LOFpValVect < 0.05), " cal. curve(s)", sep = ""))
  graphics::text(6, 0.9, paste("p-val < 5% for ", round(sum(LOFpValVect < 0.05)/length(LOFpValVect) * 100, 0),
                     " % of the cal. curves", sep = ""))
}

#' Displays the calibration curves
#'
#' calib_plot plots the calibration curves.
#' @param Ds is a data.frame with 6 columns (RunTechnician, ConcentrationLabel, ConcentrationValue, CalibCurve, ReplicateNumber,
#'Responses). Responses column might have been filled by simulation or with the experimental results.
#' @return N/A plots for Rmarkdown or shiny
#' @examples
#' data(calib_data)
#' calib_plot(calib_data)
#'
#' myDOE =calib_doe(nRun = 2, nCalibCurvesPerRun = 2, nrepCalib = 2,
#' ConcVect = c(0, 50, 100, 125, 150, 175, 200))
#' calib_data2 = norm_lin_dataset_sim(myDOE, intercept = 0, slope = 1, SDrun = 0.3,
#' SDrep = 0.3, biais = 0)
#' calib_plot(calib_data2)
#'
#' myDOE =calib_doe(nRun = 60, nCalibCurvesPerRun = 60, nrepCalib = 3,
#' ConcVect = c(0, 50, 100, 125, 150, 175, 200))
#' calib_data3 = norm_lin_dataset_sim(myDOE, intercept = 0, slope = 1, SDrun = 0.3,
#' SDrep = 0.3, biais = 0)
#' calib_plot(calib_data3)
#'
#' @export
calib_plot <- function(Ds) {
  Ds2 = as.data.frame(Ds)

  if (length(unique(Ds[, "CalibCurve"])) < 15) {
    d <- ggplot2::ggplot(Ds2, ggplot2::aes(x = as.numeric(as.character(Ds2[, "ConcentrationValue"])),
                            y = as.numeric(as.character(Ds2[,"Response"])), group = CalibCurve, colour = CalibCurve)) + ggplot2::xlab("Theoritical Concentration in ppm") + ggplot2::ylab("Analytical response") + ggplot2::geom_point(ggplot2::aes(colour = factor(CalibCurve))) + ggplot2::stat_smooth(ggplot2::aes(colour = factor(CalibCurve)),method = "lm", se = FALSE)
    print(d)
  } else if (length(unique(Ds[, "RunTechnician"])) < 15) {
    d <- ggplot2::ggplot(Ds2, ggplot2::aes(x = as.numeric(as.character(Ds2[, "ConcentrationValue"])),
            y = as.numeric(as.character(Ds2[,"Response"])), group = RunTechnician, colour = RunTechnician)) + ggplot2::xlab("Theoritical Concentration in ppm") + ggplot2::ylab("Analytical response") + ggplot2::geom_point(ggplot2::aes(colour = factor(RunTechnician)))    + ggplot2::stat_smooth(ggplot2::aes(colour = factor(RunTechnician)),method = "lm", se = FALSE)
    print(d)
  } else print(ggplot2::ggplot(Ds2,
          ggplot2::aes(x = as.numeric(as.character(Ds2[, "ConcentrationValue"])),
                       y = as.numeric(as.character(Ds2[,"Response"])))) + ggplot2::xlab("Theoritical Concentration in ppm") + ggplot2::ylab("Analytical response") + ggplot2::geom_point(ggplot2::aes()) + ggplot2::stat_smooth(ggplot2::aes(), method = "lm", se = FALSE))
}
