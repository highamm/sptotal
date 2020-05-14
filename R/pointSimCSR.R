#' simulate completely spatially random point patterns.
#'
#' simulates a completely spatially random point patterns. This function is
#' only used in simulating data sets.
#'
#' @param npoints number of points to add that are completely spatially random (CSR), default = 100
#' @param lower_x_lim left limit of boundary, default = 0
#' @param upper_x_lim right limit of boundary, default = 1
#' @param lower_y_lim lower limit of boundary, default = 0
#' @param upper_y_lim upper limit of boundary, default = 1
#'
#' @return data.frame of two columns, x-coordinate in the first, and y-coordinate in the second.
#'
#' @author Jay Ver Hoef
#' @export

pointSimCSR <- function(npoints = 100, lower_x_lim = 0, upper_x_lim = 1,
	lower_y_lim = 0, upper_y_lim = 1)
{
	x_range <- upper_x_lim - lower_x_lim
	y_range <- upper_y_lim - lower_y_lim
	tibble::data_frame(x=lower_x_lim + runif(npoints)*x_range,
		y=lower_y_lim + runif(npoints)*y_range)
}

