#' Creates a systematic grid of points
#'
#' Creates a systematic grid of points 
#'
#' @param lower_x_lim the lower limit for x-coordinate, default is 0
#' @param upper_x_lim the upper limit for x-coordinate, default is 1
#' @param lower_y_lim the lower limit for y-coordinate, default is 0
#' @param upper_y_lim the upper limit for y-coordinate, default is 1
#' @param ncol the number of cols in the systematic grid, default is 10
#' @param nrow the number of rows in the systematic grid, default is 10

#' @return A data.frame with x- and y-coordinates of simulated locations
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

pointSimSyst <- function (nrow = 10, ncol = 10, lower_x_lim = 0, upper_x_lim = 1, 
    lower_y_lim = 0, upper_y_lim = 1) 
{
    x_range <- upper_x_lim - lower_x_lim
    y_range <- upper_y_lim - lower_y_lim
    y_mat <- lower_y_lim + y_range * (nrow - matrix(rep(1:nrow, 
        times = ncol), nrow = nrow, ncol = ncol))/(nrow) + y_range/(2 * 
        nrow)
    x_mat <- lower_x_lim + x_range * (t(matrix(rep(1:ncol, times = nrow), 
        nrow = ncol, ncol = nrow)) - 1)/(ncol) + x_range/(2 * 
        ncol)
    data.frame(x = matrix(x_mat, ncol = 1), y = matrix(y_mat, 
        ncol = 1))
}

