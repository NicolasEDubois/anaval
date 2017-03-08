#' Linear link function gives the response (i.e. y or ordinate) of a linear model.
#'
#' @param intercept is a real number corresponding to the intercept of the linear model.
#' @param slope is a real number corresponding to the slope of the linear model.
#' @param x is a real number (or a vector of real numbers) corresponding the abscissa (the input) of the linear model.
#' @return y, the output is an real number (or a vector of real numbers) corresponding to the ordinate(s) of the linear model.
#' @examples linear_link(intercept = 0, slope=1,x=c(1:3))
linear_link <- function(intercept = 0, slope = 1, x) {
    y = intercept + x * slope
    return(y)
}

#' Inverse linear link function gives the abscissa (x) of a linear model.
#'
#' @param intercept is a real number corresponding to the intercept of the linear model.
#' @param slope is a real number corresponding to the slope of the linear model.
#' @param y is a real number (or a vector of real numbers) corresponding the ordinate(s) of the linear model.
#' @return x, the output is an real number (or a vector of real numbers) corresponding to the abscissa of the linear model.
#' @examples linear_link(intercept = 0, slope=1,x=c(1:3))
linear_link_inv <- function(intercept = 0, slope = 1, y) {
    x = (y - intercept)/slope
    # names(x)=''
    return(x)
}
