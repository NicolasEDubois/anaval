#' Plot the measured concentration (or dilution) vs a variable defined by the user.
#' @param Ds is a data.frame with 6 columns (RunTechnician, ConcentrationLabel, ConcentrationValue,
#'  CalibCurve, ReplicateNumber, Responses). Or a matrix with 7 columns (RunTechnician, ConcentrationLabel, ConcentrationValue,
#'  ReplicateNumber, Status, Responses, MeasuredConcentration).
#'  Responses column might have been filled by simulation or with the experimental results.
#' @param YResp a string of character corresponding to the name of the y axis.
#' @param YLongName a string of character corresponding to the label of the y axis (the measured concentration).
#'  @examples
#'  monDOE = DOE_Run_Repl_Conc(nRun = 3, nreplicates = 2, ConcVect = c(0, 50, 100, 125,150, 175, 200 ), Threshold='NaN', factorlist=c('RunTechnician','ConcentrationLabel','ConcentrationValue','ReplicateNumber','Status','Response'))
#'  TPL <- norm_lin_dataset_sim(monDOE, intercept = 1, slope = 2, SDrun=1.5, SDrep=3, biais = 0)
#' bivariate_plot(TPL)
#'
#' monDOE = DOE_Run_Repl_Conc(nRun = 7, nreplicates = 2, ConcVect = c(0, 50, 100, 125,150, 175, 200 ), Threshold='NaN', factorlist=c('RunTechnician','ConcentrationLabel','ConcentrationValue','ReplicateNumber','Status','Response'))
#' TPL <- norm_lin_dataset_sim(monDOE, intercept = 1, slope = 2, SDrun=1.5, SDrep=3, biais = 0)
#' bivariate_plot(TPL)
#' @export
bivariate_plot <- function(Ds, YResp = 'MeasuredConcentration', YLongName = 'Measured response')
{
  # Plot to verify the simulation data
  if (length(unique(Ds[,'RunTechnician'])) < 7)
  {
    p <- ggplot2::qplot(as.numeric(as.character(Ds[,'ConcentrationValue'])),as.numeric(as.character(Ds[,'MeasuredConcentration'])), xlab="Theoritical Concentration (in ppm)", ylab=YLongName, main="Bivariate graph: Response vs Concentration", colour = Ds[,'RunTechnician'] , shape = Ds[,'RunTechnician'])
    q <- p + ggplot2::labs(colour='Run', shape='Run')
    print(q)
  } else if (length(unique(Ds[,'RunTechnician'])) > 6) {
    p <- ggplot2::qplot(as.numeric(as.character(Ds[,'ConcentrationValue'])),as.numeric(as.character(Ds[,'MeasuredConcentration'])), xlab="Theoritical Concentration (in ppm)", ylab=YLongName, main="Bivariate graph: Response vs Concentration")
    print(p) }
}
