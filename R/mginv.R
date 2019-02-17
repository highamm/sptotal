#' Constructing the generalized inverse of a matrix
#'
#' Computes the generalized inverse of a matrix X
#'
#' This function is used in the \code{m2LL.FPBK.nodet} functions in order
#' to estimate the spatial covariance parameters
#'
#' @param X The matrix to be inverted
#' @param tol The tolerance of the estimation
#'
#' @return The generalized inverse matrix
#' @export mginv

mginv <- function(X, tol = sqrt(.Machine$double.eps)) {
	dnx <- dimnames(X)
	if(is.null(dnx)) dnx <- vector("list", 2)
	s <- svd(X)
	nz <- s$d > tol * s$d[1]
	structure(
		if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
		dimnames = dnx[2:1])
}
