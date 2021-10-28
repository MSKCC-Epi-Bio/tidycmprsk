
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycmprsk

<!-- badges: start -->

[![R-CMD-check](https://github.com/MSKCC-Epi-Bio/tidycmprsk/workflows/R-CMD-check/badge.svg)](https://github.com/MSKCC-Epi-Bio/tidycmprsk/actions)
[![Codecov test
coverage](https://codecov.io/gh/MSKCC-Epi-Bio/tidycmprsk/branch/main/graph/badge.svg)](https://codecov.io/gh/MSKCC-Epi-Bio/tidycmprsk?branch=main)
<!-- badges: end -->

The goal of `tidycmprsk` is to provide a compatible wrap of the
competing risks analysis R package `cmprsk`, such that the output
objects can work with methods like `model.frame()`, `model.matrix()`,
`tidy()`, and so on. This package can be incorporated with `gtsummary`
to provide convenient summary of competing risks models.

Currently, the package supports Fine and Gray’s subdistribution hazard
model (function `crr`). The non-parametric cumulative incidence function
(function `cuminc`) is under development.

## Installation

You can install the released version of tidycmprsk from GitHub

``` r
# install.packages("devtools")
devtools::install_github("MSKCC-Epi-Bio/tidycmprsk")
```

## Example

Fit a Fine and Gray model for the example data `trial` with covariate
`age`.

``` r
library(tidycmprsk)

crr_mod <- crr(Surv(ttdeath, death_cr) ~ age + trt, trial)
#> 11 cases omitted due to missing values
crr_mod
#> 
#> -- crr() -----------------------------------------------------------------------
#> * Call Surv(ttdeath, death_cr) ~ age + trt
#> * Failure type of interest "death from cancer"
#> 
#> Variable    HR     SE      95% CI       p-value    
#> age         1.01   0.010   0.99, 1.03   0.56       
#> trtDrug B   1.52   0.279   0.88, 2.62   0.13
```

The `tidycmprsk` plays will with other packages, such as `gtsummary`.

``` r
library(gtsummary)

tbl <- tbl_regression(crr_mod, exponentiate = TRUE)
```

<img src="man/figures/README-gtsummary_print-1.png" width="60%" />
