library(sptotal)

#create systematic grid of points
xy = pointSimSyst(nrow = 20, ncol = 20, lower_x_lim = 0, upper_x_lim = 1,
    lower_y_lim = 0, upper_y_lim = 1)

plot(xy)

set.seed(1001)

# create autocorrelated random error structure
eps = geostatSim(xy, xcol = "x", ycol = "y",
	parsil = 1, range = 1, nugget = .01,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")[,3]

#create independent continuous X1 variable
X1 = rnorm(length(eps))
#create independent continuous X2 variable
X2 = rnorm(length(eps))
#create independent continuous X3 variable
X3 = rnorm(length(eps))
#create independent continuous X4 variable
X4 = rnorm(length(eps))
#create independent continuous X5 variable
X5 = rnorm(length(eps))
#create autocorrelated continuous X6 variable
X6 = geostatSim(xy, xcol = "x", ycol = "y",
	parsil = 1, range = .2, nugget = .01,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")[,3]
#create autocorrelated continuous X7 variable
X7 = geostatSim(xy, xcol = "x", ycol = "y",
	parsil = 1, range = .2, nugget = .01,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")[,3]
#create factor F1 variable
F1 = as.factor(rep(1:3, times = ceiling(length(eps)/3)))[1:length(eps)]
F1 = F1[order(runif(length(eps)))]
#create factor F2 variable
F2 = as.factor(rep(1:5, times = ceiling(length(eps)/5)))[1:length(eps)]
F2 = F2[order(runif(length(eps)))]

Z = 10 + 0*X1 + .1*X2 + .2*X3 + .3*X4 + .4*X5 + .4*X6 + .1*X7 +
  .4*(as.numeric(F1) - 1) + .1*(as.numeric(F2) - 1) + 2*eps

wts1 = rep(1/length(Z), times = length(Z))
wts2 = rep(0, times = length(Z))
wts2[xy$x > .25 & xy$x < .5 & xy$y > .25 & xy$y < .5] = 1

simdata = data.frame(x = xy$x, y = xy$y, X1 = X1, X2 = X2, X3 = X3, X4 = X4,
  X5 = X5, X6 = X6, X7 = X7, F1 = F1, F2 = F2, Z = Z, wts1 = wts1, wts2 = wts2)

#save(simdata, file = paste0('/media/jay/ExtraDrive1/sptotal_pack',
#  '/sptotal/data/simdata.rda'))

