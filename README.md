`sptotal` implements finite population block kriging (Ver Hoef (2008)), a geostatistical approach to predicting means and totals of count data for finite populations. `sptotal` is currently under development.

## Installation

`sptotal` can be installed using `devtools`

```{r}
library(devtools)
install_git("https://github.com/highamm/sptotal.git")
```

## Simple Example

The `sptotal` package can be used for spatial prediction in settings where there are a finite number of sites and some of these sites were not sampled. Note that, to keep this example simple, we are simulating response values that are spatially independent. In a real example, we assume that there is some spatial dependence in the response.

```{r, results = "hide"}
set.seed(102910)
spatial_coords <- expand.grid(1:10, 1:10)
toy_df <- data.frame(xco = spatial_coords[ ,1], yco = spatial_coords[ ,2], counts = sample(c(rpois(50, 15), rep(NA, 50)), size = 100, replace = TRUE))

mod <- slmfit(formula = counts ~ 1, xcoordcol = "xco", ycoordcol = "yco", data = toy_df)
summary(mod)

pred <- predict(mod)
## look at the predictions
pred$Pred_df[1:6, c("xco", "yco", "counts", "counts_pred_count")]
```

## Methods and Basic Functions

\code{sptotal} Main Functions:

\code{slmfit} fits a spatial linear model to the response on the
observed/sampled sites. \code{check.variogram} can be used to construct
an empirical variogram of the residuals of the spatial linear model.

\code{FPBKpred} uses the spatial linear model in \code{slmfit} and finite
population block kriging to predict counts/densities at unobserved locations.
A prediction for the total count as well as a prediction variance
are given by default.

\code{get.predinfo} and \code{get.predplot} take the resulting object from
\code{FPBKpred} to construct (1) summary information, including the
prediction, prediction variance, and a prediction interval as well as
(2) a plot of the site-wise predictions.

For more details on how to use these functions, please see the Vignette.

The methods in this package are based on the following reference:

Ver Hoef, Jay M. "Spatial methods for plot-based sampling of wildlife populations." \emph{Environmental and Ecological Statistics} 15, no. 1 (2008): 3-13.

## Citation

To cite this package in the literature, run the following line:

```{r}
citation("sptotal")
```



