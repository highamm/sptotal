`sptotal` implements finite population block kriging (FPBK, Ver Hoef (2008)), a geostatistical approach to predicting means and totals of count data for finite populations.

See <a href="https://highamm.github.io/sptotal/index.html" target="_blank"> sptotal's Website </a> for more information.

The accepted Journal of Open Source Software paper on `sptotal` can be found at [![DOI](https://joss.theoj.org/papers/10.21105/joss.05363/status.svg)](https://doi.org/10.21105/joss.05363)


   
## Statement of Need

The primary purpose of ``sptotal`` is to provide an implementation of the Finite Population Block Kriging (FPBK) methods developed in Ver Hoef (2002) and Ver Hoef (2008). The method is useful when

* there are a finite number of spatial locations (or sites) 
* only a subset of the spatial locations are sampled
* there is expected to be some spatial correlation

Examples of settings where FPBK is useful include wildlife abundance surveys performed on a finite number of spatial locations. In these surveys, it is not uncommon to only sample a subset of the region.

## Installation Instructions

`sptotal` can be installed from CRAN

```{r}
install.packages("sptotal")
```

or using `devtools`

```{r}
library(devtools)
install_git("https://github.com/highamm/sptotal.git")
```

## Simple Example

The `sptotal` package can be used for spatial prediction in settings where there are a finite number of sites and some of these sites were not sampled. Note that, to keep this example simple, we are simulating response values that are spatially independent. In a real example, we assume that there is some spatial dependence in the response.

```{r}
set.seed(102910)
spatial_coords <- expand.grid(1:10, 1:10)
toy_df <- data.frame(xco = spatial_coords[ ,1],
yco = spatial_coords[ ,2], counts = sample(c(rpois(50, 15),
rep(NA, 50)), size = 100, replace = TRUE))

mod <- slmfit(formula = counts ~ 1, xcoordcol = "xco",
ycoordcol = "yco", data = toy_df)
summary(mod)

pred <- predict(mod)
```

We can look at the predictions with

```{r}
pred$Pred_df[1:6, c("xco", "yco", "counts", "counts_pred_count")]
```

## Methods and Basic Functions

`sptotal` Main Functions:

`slmfit()` fits a spatial linear model to the response on the
observed/sampled sites. \code{check.variogram} can be used to construct
an empirical variogram of the residuals of the spatial linear model.

`predict.slmfit()` uses the spatial linear model fitted with `slmfit()` and finite
population block kriging to predict counts/densities at unobserved locations.
A prediction for the total count as well as a prediction variance
are given by default.

For more details on how to use these functions and for a real world example applying the methods to a moose data set, please see the Vignette at <a href="https://highamm.github.io/sptotal/articles/sptotal-vignette.html
" target="_blank">https://highamm.github.io/sptotal/articles/sptotal-vignette.html </a>.

## Community Guidelines

We encourage users to submit GitHub issues and enhancement requests at <a href="https://github.com/highamm/sptotal
" target="_blank">https://github.com/highamm/sptotal</a> so we may continue to improve ``sptotal``.

## Citation

To cite this package in the literature, run the following line:

```{r}
citation("sptotal")
```

The methods in this package are based on the following references:

Ver Hoef, J. M. (2008). "Spatial methods for plot-based sampling of wildlife populations." _Environmental and Ecological Statistics_, __15__(1), 3–13.

Ver Hoef, J. M. (2002). "Sampling and geostatistics for spatial data." _Ecoscience_, __9__(2), 152– 161.


