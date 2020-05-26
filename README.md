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

## Methods

The methods in this package are based on the following reference:

Ver Hoef, Jay M. "Spatial methods for plot-based sampling of wildlife populations." Environmental and Ecological Statistics 15, no. 1 (2008): 3-13.

## Citation

To cite this package in the literature, run the following line:

```{r}
citation("sptotal")
```



