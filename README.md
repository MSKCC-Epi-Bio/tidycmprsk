
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycmprsk

<!-- badges: start -->

[![R-CMD-check](https://github.com/MSKCC-Epi-Bio/tidycmprsk/workflows/R-CMD-check/badge.svg)](https://github.com/MSKCC-Epi-Bio/tidycmprsk/actions)
[![Codecov test
coverage](https://codecov.io/gh/MSKCC-Epi-Bio/tidycmprsk/branch/main/graph/badge.svg)](https://codecov.io/gh/ddsjoberg/tidycmprsk?branch=main)
<!-- badges: end -->

The goal of tidycmprsk is to …

## Installation

You can install the released version of tidycmprsk from GitHub

``` r
# install.packages("devtools")
devtools::install_github("/MSKCC-Epi-Bio/tidycmprsk")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidycmprsk)

crr(Surv(ttdeath, death_cr) ~ age, trial)
#> 11 cases omitted due to missing values
#> 
#> -- tidycrr() -------------------------------------------------------------------
#> * Call Surv(ttdeath, death_cr) ~ age
#> * Failure type of interest "death from cancer"
#> 
#> Fine and Gray's model fit: 
#>   term   estimate   std.error statistic p.value
#> 1  age 0.00581448 0.009824244 0.5918501    0.55
```
