#' Spatial Correlation Models
#'
#' Note that, currently, only three of these models are implemented
#' in the \code{sptotal} package: \code{corModelExponential},
#'  \code{corModelGaussian}, and \code{corModelSpherical}.
#'
#' @param distance.matrix The distance matrix for sampled sites
#' @param range The range that determines how quickly covariance
#' among sites tapers
#' @return Correlation Matrix
#' @export corModelExponential
#' @export corModelGaussian
#' @export corModelSpherical

#' @describeIn corModelExponential Exponential Correlation Structure
corModelExponential <- function(distance.matrix, range) {
	exp(-distance.matrix / range)
}

corModelExpRadon2 <- function(distance.matrix) {
	(1 + distance.matrix)*exp(-distance.matrix)
}

corModelExpRadon4 <- function(distance.matrix) {
	(1 + distance.matrix + distance.matrix^2/3)*exp(-distance.matrix)
}

#' @describeIn corModelExponential Gaussian Correlation Structure
corModelGaussian <- function(distance.matrix, range) {
	exp(-distance.matrix^2 / range)
}

corModelStable <- function(distance.matrix, extrap) {
	exp(-distance.matrix^extrap)
}

corModelRationalQuad <- function(distance.matrix) {
	1/(1+distance.matrix^2)
}

corModelCauchyGrav <- function(distance.matrix) {
	1/sqrt(1+distance.matrix^2)
}

corModelCauchyMag <- function(distance.matrix) {
	1/(sqrt(1+distance.matrix^2))^3
}

#' @describeIn corModelExponential Cauchy Correlation Structure
#' @param extrap Smoothness parameter (optional)

corModelCauchy <- function(distance.matrix, range, extrap) {
	1/(1+distance.matrix^2)^extrap
}

corModelCircular <- function(distance.matrix) {
	d <- distance.matrix
	d[distance.matrix > 1] <- 0
	CovMat <- 2*(acos(d) - d*sqrt(1 - d^2))/pi
	CovMat[distance.matrix >= 1] <- 0
	CovMat
}

#' @describeIn corModelExponential Spherical Correlation Structure

corModelSpherical <- function(distance.matrix, range) {
	CovMat <- (1 - 1.5 * (distance.matrix / range) +
	    0.5 * (distance.matrix / range) ^ 3)
	CovMat[distance.matrix / range > 1] <- 0
	CovMat
}
#' @describeIn corModelExponential Cubic Correlation Structure

corModelCubic <- function(distance.matrix) {
	CovMat <- (1 - 7*distance.matrix^2 + 35*distance.matrix^3/4 - 7*distance.matrix^5/2
		+ 3*distance.matrix^7/4)
	CovMat[distance.matrix > 1] <- 0
	CovMat
}

corModelPenta <- function(distance.matrix) {
	CovMat <- (1 - 22*distance.matrix^2/3 + 33*distance.matrix^4 - 77*distance.matrix^5/2
		+ 33*distance.matrix^7/2 - 11*distance.matrix^9/2 + 5*distance.matrix^11/6)
	CovMat[distance.matrix > 1] <- 0
	CovMat
}

corModelCardinalSine <- function(distance.matrix) {
	d <- distance.matrix
	d[distance.matrix == 0] <- 1
	CorMat <- sin(d)/d
	CorMat[distance.matrix == 0] <- 1
	CorMat
}

corModelBesselJ <- function(distance.matrix, extrap) {
	d <- distance.matrix
	d[distance.matrix == 0] <- 1
	extrap <- extrap + 2.0
	CorMat <- d^(1-extrap/2)*besselJ(d, extrap/2 - 1)*2^(extrap/2 - 1)*gamma(extrap/2)
	CorMat[distance.matrix == 0] <- 1
	CorMat
}

corModelBesselK <- function(distance.matrix, extrap) {
	d <- distance.matrix
	d[distance.matrix == 0] <- 1
	CorMat <- d^extrap*besselK(d, extrap)/(2^(extrap - 1)*gamma(extrap))
	CorMat[distance.matrix == 0] <- 1
	CorMat
}


