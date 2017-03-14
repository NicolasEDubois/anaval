#' Display the layout of the design of experiment of a dataset in a contengency table.
#'
#' @param Ds1 a matrix or a data.frame produced by a DOE generating function or by a generation of simulated observations function.
#' @param GroupVar is a string of character corresponding to a name of categorical variable (i.e. a column) of Ds1.
#' Each level of this variable will be a row of the contengency table.
#' @param GroupVar2 is a string of character corresponding to a name of categorical variable (i.e. a column) of Ds1.
#' Each level of this variable will be a column of the contengency table.
#' @return An output of class 'table' which corresponds to the contengency table of the two selected variables.
#' @examples
#' data(ref_calib_doe)
#' display_a_DOE(ref_calib_doe, GroupVar = 'CalibCurve', GroupVar2 = 'ConcentrationValue')
#' @export
display_a_DOE = function(Ds1, GroupVar = "CalibCurve", GroupVar2 = "ConcentrationValue") {
  ListName = dimnames(Ds1)[[2]]

  # GroupVar Selection
  ColSelect = ListName[ListName == GroupVar]
  FactorGroupVar = Ds1[!duplicated(Ds1[, ColSelect]), ColSelect]
  GroupVarVector = Ds1[, ColSelect]

  # ContVar
  ColSelect2 = ListName[ListName == GroupVar2]
  FactorGroupVar2 = Ds1[!duplicated(Ds1[, ColSelect2]), ColSelect2]
  GroupVarVector2 = as.character(Ds1[, ColSelect2])

  ContengencyTable = as.matrix(table(GroupVarVector, GroupVarVector2))

  return(ContengencyTable)
}

#' Summarize the dataset through descriptive statistics
#' @param Ds0 matrix or a data.frame
#' @param VarVect a vector with the name(s) of the numerical variable(s) to summerized
#' @param GroupVar (optional) a string of character corresonding a categorical variable that can
#'  be used to apply the summary statistics to a subgroups of observations
#'  of the dataset.
#'  @examples
#'  data(calib_data)
#'  summary_stat(Ds0=calib_data,VarVect=c('MeasuredConcentration'), GroupVar= 'ConcentrationLabel')
#' @export
summary_stat <- function(Ds0, VarVect, GroupVar = "") {
  if (GroupVar == "") {
    Output <- list()

    for (j in 1:length(VarVect)) {
      MyVariable = find_column(Ds0, VarVect[j])

      MyDataFrame = data.frame(matrix(NaN, ncol = 4, nrow = 1))
      names(MyDataFrame) <- c("Count", "Mean", "Sd", "CV")
      Count <- sum(table(MyVariable))
      Mean <- mean(as.numeric(as.character(MyVariable)))
      SD <- sd(as.numeric(as.character(MyVariable)))
      CV <- abs(as.numeric(as.character(SD))/as.numeric(as.character(Mean)))

      MyDataFrame[1, ] <- c(Count, Mean, SD, CV)
      Output[[j]] <- MyDataFrame
    }

  } else {
    GroupVector = find_column(Ds0, NomdeColonne = GroupVar, "Vector")
    GroupColumn = find_column(Ds0, NomdeColonne = GroupVar, "ColumnNumber")
    ListGroup = unique(GroupVector)

    Output <- list()

    for (j in 1:length(VarVect)) {
      MyVariable = find_column(Ds0, VarVect[j])

      MyDataFrame = data.frame(matrix(NaN, ncol = 4, nrow = length(ListGroup)))
      row.names(MyDataFrame) <- ListGroup
      names(MyDataFrame) <- c("Count", "Mean", "Sd", "CV")

      for (i in 1:length(ListGroup)) {
        # Selection of the sub data set corresponding to one instance of the group
        GroupLeveli <- ListGroup[i]
        MyVariable2 = MyVariable[GroupVector == GroupLeveli]

        # Count
        Count <- sum(table(MyVariable2))
        Mean <- mean(as.numeric(as.character(MyVariable2)))
        SD <- sd(as.numeric(as.character(MyVariable2)))
        CV <- abs(as.numeric(as.character(SD))/as.numeric(as.character(Mean)))

        MyDataFrame[i, ] <- c(Count, Mean, SD, CV)
      }
      Output[[j]] <- MyDataFrame
    }
  }
  names(Output) <- VarVect
  return(Output)
}

#' Retrieve the data of a column form a matrix or a data.frame.
#' @param Ds0 matrix or a data.frame
#' @param NomdeColonne the name of the column of interest
#' @param Output (optional) export the data of the column as a vector or only the number of the column.
#'  @examples
#'  ExMatrix = cbind(c(11,12,13),c(21,22,23),c(31,32,33))
#'  colnames(ExMatrix) <- c('first_col','second_col','third_col')
#'  find_column(ExMatrix, NomdeColonne='second_col', Output = "Vector")
#'  find_column(ExMatrix, NomdeColonne='second_col', Output = "Number")
#' @export
find_column <- function(Ds0, NomdeColonne, Output = "Vector") {
  a = dimnames(Ds0)[[2]]
  b = 1:length(a)
  c = b[a == NomdeColonne]
  d = Ds0[, c]
  if (Output == "Vector") {
    return(d)
  } else return(c)
}

