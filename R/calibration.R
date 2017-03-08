#' Generate a design of experiment
#'
#' \code{calib_doe} return a DOE in a data.frame where responses is NaN.
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
#' calib_doe(nRun = 2, nCalibCurvesPerRun = 3, nrepCalib = 5, ConcVect = c(0,50, 100, 125, 150, 175, 200))
calib_doe <- function(nRun = 3, nCalibCurvesPerRun = 2, nrepCalib = 3, ConcVect = c(0, 50, 100, 125, 150,
    175, 200)) {
    # DOE
    factorlist <- c("RunTechnician", "ConcentrationLabel", "ConcentrationValue", "CalibCurve", "ReplicateNumber",
        "Response")
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
                  MyDOE[rowId, "ConcentrationLabel"] = paste(sprintf("%03d", as.numeric(f2[j])), sep = "")
                  MyDOE[rowId, "ConcentrationValue"] = f2[j]
                  MyDOE[rowId, "CalibCurve"] = paste(f1[i], sprintf("%02d", as.numeric(f3[k])), sep = "_")
                  MyDOE[rowId, "ReplicateNumber"] = paste(sprintf("%02d", l, sep = ""))
                  MyDOE[rowId, "Response"] = NaN
                  rowId <- rowId + 1
                }
            }
        }
    }
    return(as.data.frame(MyDOE))
}

CalibFixedCoefEst <- function(DS, LowerRange = 0, UpperRange = 200, MyModel = "lm", Mode = "Definition of a range of parameters",
    diagnostics = TRUE) {
    DS2 = as.data.frame(DS[as.numeric(DS[, "ConcentrationValue"]) >= LowerRange & as.numeric(DS[, "ConcentrationValue"]) <=
        UpperRange, ])

    ListCalCurve = unique(DS2[, "CalibCurve"])

    ncal = length(ListCalCurve)

    if (MyModel == "lm") {

        for (i in c(1:ncal)) {
            Ds3 = DS2[DS2$CalibCurve == ListCalCurve[i], ]

            Response = as.numeric(as.character(Ds3$Response))
            Conc = as.numeric(as.character(Ds3$ConcentrationValue))

            lmModel <- lm(Response ~ Conc)

            Full = lm(Response ~ as.factor(Conc))  #fit full model
            LackOfFitTest = anova(lmModel, Full)  #get lack-of-fit test

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

        if (Mode == "Definition of a range of parameters") {
            CoefSummaryTable = SummaryStat(Ds0 = MatrixfixedCoef, VarVect = c("Intercept", "Slope"), GroupVar = "")
            MyNames = dimnames(CoefSummaryTable$Intercept)[[2]]
            CoefSummaryTable$Intercept = cbind(CoefSummaryTable$Intercept, CoefSummaryTable$Intercept[,
                "Mean"] - 3 * CoefSummaryTable$Intercept[, "Sd"], CoefSummaryTable$Intercept[, "Mean"] +
                3 * CoefSummaryTable$Intercept[, "Sd"])
            CoefSummaryTable$Slope = cbind(CoefSummaryTable$Slope, CoefSummaryTable$Slope[, "Mean"] -
                3 * CoefSummaryTable$Slope[, "Sd"], CoefSummaryTable$Slope[, "Mean"] + 3 * CoefSummaryTable$Slope[,
                "Sd"])
            colnames(CoefSummaryTable$Intercept) = c(MyNames, "Lower bound (mean - 3*S)", "Lower bound (mean + 3*S)")
            colnames(CoefSummaryTable$Slope) = c(MyNames, "Lower bound (mean - 3*S)", "Lower bound (mean + 3*S)")

            Resp = list(FixedEffectEst = FixedEffectEst, MatrixfixedCoef, Table = CoefSummaryTable, PredResid = PredResid,
                LOFpValVect = LOFpValVect)
        }

    }
    return(Resp)
}

