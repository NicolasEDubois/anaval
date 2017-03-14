#' Generate the design of experiment for validation studies
#'
#' @param nRun is a numirical scalar corresponding to the number of run(s) sessions
#'  that is (are) orginised in the design of experiment.
#' @param nreplicates is a numirical scalar corresponding to the number of replicate
#'  in the design of experiment.
#' @param ConcVect is a vector of numerical values corresponding to the concentration
#'  (or dilution) in the design of experiment.
#' @param Threshold is a numerical scalar corresponding to the detection limit or
#' the decision limit.
#' @param factorlist is a vector of characters corresponding to the name of the columns
#'  of the DOE Do not change!
#' @return An output of class 'matrix' which corresponds to the full factorial design
#' of experiment that is run(s) x replicate(s) x concentration level(s).
#' @examples
#' DOE_Run_Repl_Conc(nRun = 3, nreplicates = 2, ConcVect = c(0, 50, 100, 125,150, 175, 200 ),
#'  Threshold='NaN',
#'  factorlist=c('RunTechnician','ConcentrationLabel',
#' 'ConcentrationValue','ReplicateNumber','Status','Response'))
#' @export
DOE_Run_Repl_Conc <- function(nRun = 3, nreplicates = 2, ConcVect = c(0, 50, 100, 125,150, 175, 200 ), Threshold='NaN', factorlist=c('RunTechnician','ConcentrationLabel','ConcentrationValue','ReplicateNumber','Status','Response'))
{
  nConc = length(ConcVect)          # number of Concentration levels
  ReplicateList = c(1:nreplicates)

  MyDOE =  matrix(NaN,nrow = nRun * nConc * nreplicates, ncol = length(factorlist))
  colnames(MyDOE) = c(factorlist)

  RunList = paste('Run',sprintf("%03d",c(1:nRun)), sep='') # 1st factor levels

  rowId = 1
  for (i in c(1:nRun))
  {

    for(j in c(1:nConc))
    {
      for(k in c(1:nreplicates))
      {
        MyDOE[rowId,'RunTechnician'] = RunList[i]
        MyDOE[rowId,'ConcentrationLabel'] = sprintf("%03d",as.numeric(ConcVect[j]))
        MyDOE[rowId,'ConcentrationValue'] = ConcVect[j]
        MyDOE[rowId,'ReplicateNumber'] = k
        if(!(Threshold == 'NaN'))
        {
          if(as.numeric(MyDOE[rowId,'ConcentrationValue']) > Threshold)
          {
            MyDOE[rowId,'Status'] = 'Positive'
          } else if(as.numeric(MyDOE[rowId,'ConcentrationValue']) <= Threshold) {
            MyDOE[rowId,'Status'] = 'Negative'
          }
          else MyDOE[rowId,'Status'] = 'Error'
        }
        rowId=rowId+1
      }
    }
  }
  return(MyDOE)
}

