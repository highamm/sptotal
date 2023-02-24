---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'sptotal: an R package for predicting totals and weighted sums from spatial data'
tags:
  - R
  - kriging
  - finite population
  - spatial
  - prediction
authors:
  - name: Matt Higham
    affiliation: 1
  - name: Jay Ver Hoef
    affiliation: 2
  - name: Bryce Frank
    affiliation: 3
  - name: Michael Dumelle
    orcid: 0000-0002-3393-5529
    affiliation: 4
affiliations:
 - name: St. Lawrence University
   index: 1
 - name: National Oceanic and Atmospheric Administration
   index: 2
 - name: Bureau of Land Management
   index: 3
 - name: United States Environmental Protection Agency
   index: 4
citation_author: Higham et al.
date: 16 February 2023
year: 2023
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

In ecological monitoring surveys of an animal population, a plant population, or an environmental resource, predicting the total abundance, the mean, or some other quantity in a finite region is often of interest. However, because of time and money constraints, sampling the entire region is often unfeasible. The purpose of the ``sptotal`` ``R`` package is to provide software that gives a prediction for a quantity of interest, such as a total, and an associated standard error for the prediction. The predictor, referred to as the Finite-Population-Block-Kriging (FPBK) predictor in the literature [@ver_hoef_spatial_2008], incorporates possible spatial correlation in the data and also incorporates an appropriate variance reduction for sampling from a finite population. 

In the remainder of the paper, we give an overview of both the background of the method and of the ``sptotal`` package. 

# Statement of Need

The primary purpose of ``sptotal`` is to provide an implementation of the Finite Population Block Kriging (FPBK) methods developed in @ver2002sampling and @ver_hoef_spatial_2008. While we refer the interested reader to those sources for the full development of the FPBK predictor, we provide a very short overview of the setting in which the predictor can be used.

Suppose that we have a response variable $Y(\mathbf{s}_{i})$, $i = 1, 2, \ldots, N$, where the vector $\mathbf{s}_i$ contains the coordinates for the $i^{th}$ spatial location and $N$ is a finite number of spatial locations. Then $\mathbf{y}$, a vector of the $Y(\mathbf{s}_{i})$, can be modeled with a spatial linear model
\mbox{}
\begin{equation}
\mathbf{y} = \mathbf{X} \boldsymbol{\beta} + \boldsymbol{\epsilon},
\end{equation}
\noindent
where $\mathbf{X}$ is a design matrix for the fixed effects and $\boldsymbol{\beta}$ is a parameter vector of fixed effects. The random errors, $\boldsymbol{\epsilon}(\mathbf{s}_{i})$, have a mean of $\mathbf{0}$ and a covariance of 
\mbox{}
\begin{equation}
\text{var}(\boldsymbol{\epsilon}) = \tau^2 \mathbf{I} + \sigma^2 \mathbf{R},
\end{equation}

\noindent
where $\tau^2$ is the spatial independent error variance (commonly called the nugget), $\mathbf{I}$ is the identity matrix, $\sigma^2$ is the spatial dependent error variance (commonly called the partial sill), and $\mathbf{R}$ is a spatial correlation matrix. A common model used to generate $\mathbf{R}$ is the exponential correlation function [@cressie2015statistics]. 

<!-- For observations at spatial locations $i$ and $i'$ at $h_{ii'}$ distance apart, row $i$ and column $i'$ of $\mathbf{R}$ is equal to  -->
<!-- \mbox{} -->
<!-- \begin{equation} -->
<!-- \mathbf{R}_{ii'} = \text{exp}(-h_{ii'} / \phi), -->
<!-- \end{equation} -->
<!-- \noindent -->
<!-- where $\phi$ is the range parameter .  -->

Our goal is to predict some linear function of the response, $f(\mathbf{y}) = \mathbf{b}^\prime \mathbf{y}$, where $\mathbf{b}$ is a vector of weights. A common vector of weights is a vector of 1's so that the resulting prediction is for the total abundance across all sites. In a finite population setting, if we are interested in predicting the realized total and we sampled all $N$ spatial locations, then we would simply add up the realized values of $Y(\mathbf{s}_i)$ across all $N$ locations. However, in many practical settings, we do not observe all $N$ locations and instead sample a subset of these locations.

If only some of the values in $\mathbf{y}$ are observed, then the ``sptotal`` package can be used to find the the Best Linear Unbiased Predictor (BLUP) for $\mathbf{b}^\prime \mathbf{y}$, referred to as the FPBK predictor, along with its prediction variance. When the number of sites sampled is equal to the total number of sites in the region, we know the realized total exactly and the prediction variance is equal to 0.

<!-- The prediction variance incorporates a reduction for sampling from a finite population so that, for example, when the While the derivation is too long to include here, we emphasize two characteristics of the FPBK predictor. First, the predictor incorporates spatial correlation, which is a common feature of ecological data. Second, the predictor incorporates a reduction in the prediction variance for sampling from a finite population.  -->
# Package Methods

Before discussing comparable methods and ``R`` packages, we show how the main functions in ``sptotal`` can be used on a real data set to predict total abundance. We use the `AKmoose_df` data in the ``sptotal`` package, provided by the Alaska Department of Fish and Game. 


```r
library(sptotal)
data("AKmoose_df")
```

The data contains a response variable `total`, x-coordinate centroid variable `x`, y-coordinate centroid variable `y`, and covariates `elev_mean` (the elevation) and `strat` (a stratification variable). There are a total of 860 rows of unique spatial locations. Locations that were not surveyed have an ``NA`` value for ``total``. 

The two primary functions in ``sptotal`` are ``slmfit()``, which fits a spatial linear model, and ``predict.slmfit()``, which predicts a quantity of interest (such as a mean or total) using a fitted ``slmfit`` object. `slmfit()` has required arguments `formula`, `data`, `xcoordcol`, and `ycoordcol`. If `data` is a simple features object from the `sf` [@pebesma2018simple] package, then `xcoordcol` and `ycoordcol` are not required. The `CorModel` argument is the correlation model used for the errors.


```r
moose_mod <- slmfit(total ~ elev_mean + strat, data = AKmoose_df,
                    xcoordcol = "x", ycoordcol = "y",
                    CorModel = "Exponential")
summary(moose_mod)
```

```
## 
## Call:
## total ~ elev_mean + strat
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.695 -3.768 -1.304  1.111 35.816 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 0.700203   2.128817   0.329  0.74254   
## elev_mean   0.004479   0.006620   0.677  0.49944   
## stratM      2.586271   0.868335   2.978  0.00323 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Covariance Parameters:
##              Exponential Model
## Nugget                29.71177
## Partial Sill           8.04658
## Range                 33.65766
## 
## Generalized R-squared: 0.03966569
```

With the `summary()` generic, we obtain output similar to the summary output of a linear model fit with `lm()`, as well as a table of fitted covariance parameter estimates. Next, we use `predict()` to obtain a prediction for the total abundance across all spatial locations, along with a standard error for the prediction. By default, `predict()` gives a prediction for total abundance, though the default can be modified by specifying a column of prediction weights for the vector $\mathbf{b}$ with the `wtscol` argument.


```r
predict(moose_mod)
```

```
## Prediction Info:
##       Prediction    SE 90% LB 90% UB
## total       1610 413.2  930.1   2290
##       Numb. Sites Sampled Total Numb. Sites Total Observed Average Density
## total                 218               860            742           3.404
```

The output of printed `predict()` gives a table of prediction information, including the `Prediction` (a total abundance of 1610 moose, in this example), the `SE` (Standard Error) of the prediction, and bounds for a prediction interval (with a nominal level of 90% by default). Additionally, some summary information about the data set used is given.

<!-- By default, `predict()` gives a prediction for the realized total in all spatial locations in the data frame used in `data` (i.e. by default, the $\mathbf{b}$ vector is a vector of 1's). If we want a prediction for a different quantity, such as a prediction for the total for the last 300 spatial locations in `AKmoose_df`, then we can manually construct a weights column to pass to `predict()` that contains a ``1`` for the last 300 rows and a ``0`` otherwise: -->



``sptotal`` also provides many helper generic functions for spatial linear models. The structure of the arguments and of the output of these generics often mirrors that of the generics used for base ``R`` linear models fit with ``lm()``. Examples (applied to the `moose_mod` object) include ``AIC(moose_mod)``, ``coef(moose_mod)``, ``fitted(moose_mod)``, ``plot(moose_mod)``, and ``residuals(moose_mod)``.

<!-- , which gives raw residuals for the observed spatial locations and ``residuals(moose_mod, type = "normalized")``, which gives the normalized residuals for the observed spatial locations. -->

# Comparable Methods and Related Work

Methods that can also be used to predict a total, mean, or other quantity in a finite population include design-based methods. Design-based methods make inferences based on how the sample was selected. One example of a design-based sampling method for spatial data is the Generalized Random Tesselation Stratified (GRTS) spatially balanced sampling algorithm [@stevens2004spatially]. If a GRTS sample is taken, then an analysis using a local neighborhood variance estimator can be used to obtain a prediction for a population total or mean with a variance that has a finite population adjustment. @dumelle2022comparison show that FPBK outperforms the GRTS design-based analysis in simulations. 

Statistical learning methods can also be applied to spatial data to obtain a prediction for a finite population total or mean. For example, k-nearest-neighbors [@fix1985discriminatory; knn] is a statistical learning algorithm popular in forestry applications that makes predictions at unobserved locations from the values of the closest observed locations. However, quantifying uncertainty in a prediction resulting from knn is much more challenging. Additionally, @ver2013comparison show that FPBK outperforms knn in many settings.

Note that there are many spatial packages in ``R`` that can be used to predict values at unobserved locations, including ``gstat`` [@pebesma2015package], ``geoR`` [@ribeiro2007geor], and ``spmodel`` (INSERT CITATION), among other packages. What ``sptotal`` contributes is the ability to obtain a prediction variance that incorporates a variance reduction when sampling from a finite number of sampling units. 

# Past and Ongoing Research Projects

@dumelle2022comparison used the ``sptotal`` package to compare model-based and design-based approaches for analysis of spatial data. Currently, a ``Shiny`` app is in development at the Alaska Department of Fish and Game that uses ``sptotal`` to predict abundance from moose surveys conducted in Alaska. 

# References

