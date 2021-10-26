
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycmprsk

<!-- badges: start -->

[![R-CMD-check](https://github.com/MSKCC-Epi-Bio/tidycmprsk/workflows/R-CMD-check/badge.svg)](https://github.com/MSKCC-Epi-Bio/tidycmprsk/actions)
[![Codecov test
coverage](https://codecov.io/gh/MSKCC-Epi-Bio/tidycmprsk/branch/main/graph/badge.svg)](https://codecov.io/gh/ddsjoberg/tidycmprsk?branch=main)
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
devtools::install_github("/MSKCC-Epi-Bio/tidycmprsk")
```

## Example

Fit a Fine and Gray model for the example data `trial` with covariate
`age`.

``` r
library(tidycmprsk)

crr(Surv(ttdeath, death_cr) ~ age, trial)
#> 11 cases omitted due to missing values
#> 
#> -- crr() -----------------------------------------------------------------------
#> * Call Surv(ttdeath, death_cr) ~ age
#> * Failure type of interest "death from cancer"
#> 
#> Fine and Gray's model fit: 
#> # A tibble: 1 x 5
#>   term  estimate std.error statistic p.value
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>
#> 1 age    0.00581   0.00982     0.592    0.55
```
