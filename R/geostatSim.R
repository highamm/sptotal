#' Simulate geostatistical data on set of given locations
#'
#' Spatially correlated data are simulated assuming a multivariate normal
#' random error vector. For simplicity, only \code{"Exponential"} and \code{"Spherical"} simulation options are given here.
#'
#' @param loc.data data.frame with x- and y-coordinates of locations for simulated data
#' @param xcol name of the column in loc.data with x-coordinates, default is "x"
#' @param ycol name of the column loc.data with y-coordinates, default is "y"
#' @param parsil partial sill of autocorrelation model, default = 1
#' @param range range of autocorrelation model, default = 1
#' @param nugget range of autocorrelation model, default = 0
#' @param minorp proportion of range in x direction to that of y direction for unrotated anisotropic model, default = 1
#' @param rotate rotation of anisotropic axes, default = 90
#' @param extrap extra covariance paramter
#' @param CorModel autocorrelation model, default = "Exponential".  Other possibilities are "Spherical".
#'
#' @return data.frame of three columns, the original location data appended with a 3rd column of simulated geostatistical data
#'
#' @author Jay Ver Hoef
#' @export
geostatSim <- function(loc.data, xcol = "x", ycol = "y",
	parsil = 1, range = 1, nugget = 0,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")
{

  distGeoAni <- function(xrow, yrow, xcol, ycol, rotate = 0, range = 1, minorp = 1)
    {
      # expand all x-coordinates
        sxr = matrix(xrow, nrow = length(xrow), ncol = length(xcol))
        sxc <- matrix(xcol, nrow = length(xrow), ncol = length(xcol),
          byrow = TRUE)
        syr = matrix(yrow, nrow = length(yrow), ncol = length(ycol))
        syc <- matrix(ycol, nrow = length(yrow), ncol = length(ycol),
          byrow = TRUE)
      # find difference in coordinates between all pairwise locations
        sxdif <- sxr - sxc
        sydif <- syr - syc
      # rotate coordinates
        newx <- cos(rotate*.0174533)*sxdif - sin(rotate*.0174533)*sydif
        newy <- sin(rotate*.0174533)*sxdif + cos(rotate*.0174533)*sydif
      # scale coordinates by minor and major axes */
        newx <- newx/(range*minorp)
        newy <- newy/range
      # compute distance for the scaled and rotated coordinates */
        sqrt(newx^2 + newy^2)
    }

	xcoord <- loc.data[,xcol]
	ycoord <- loc.data[,ycol]
	n <- length(xcoord)
	dismat <- distGeoAni(xcoord, ycoord, xcoord, ycoord, rotate, range, minorp)
	# compute correlation matrix for scaled distance matrix
		if(CorModel == "Exponential") CovMat <- corModelExponential(dismat, 1)
#		if(CorModel == "ExpRadon2") CovMat <- CorModel.ExpRadon2(dismat)
#		if(CorModel == "ExpRadon4") CovMat <- CorModel.ExpRadon4(dismat)
#		if(CorModel == "Gaussian") CovMat <- CorModel.Gaussian(dismat)
#		if(CorModel == "Stable") CovMat <- CorModel.Stable(dismat, extrap)
#		if(CorModel == "RationalQuad") CovMat <- CorModel.RationalQuad(dismat)
#		if(CorModel == "CauchyGrav") CovMat <- CorModel.CauchyGrav(dismat)
#		if(CorModel == "CauchyMag") CovMat <- CorModel.CauchyMag(dismat)
#		if(CorModel == "Cauchy") CovMat <- CorModel.Cauchy(dismat, extrap)
#		if(CorModel == "Circular") CovMat <- CorModel.Circular(dismat)
		if(CorModel == "Spherical") CovMat <- corModelSpherical(dismat, 1)
#		if(CorModel == "Cubic") CovMat <- CorModel.Cubic(dismat)
#		if(CorModel == "Penta") CovMat <- CorModel.Penta(dismat)
#		if(CorModel == "CardinalSine") CovMat <- CorModel.CardinalSine(dismat)
#		if(CorModel == "BesselK") CovMat <- CorModel.BesselK(dismat, extrap)
#		if(CorModel == "BesselJ") CovMat <- CorModel.BesselJ(dismat, extrap)
	CovMat <- parsil * CovMat + diag(nugget, nrow = n, ncol = n)
	data.frame(loc.data, z = t(chol(CovMat)) %*% rnorm(n))
}

