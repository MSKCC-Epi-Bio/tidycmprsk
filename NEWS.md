# tidycmprsk 1.1.1

* HTML5 documentation update to satisfy CRAN requirement.

# tidycmprsk 1.1.0

* Updating package to account for changes in {gtsummary} v2.0. To use `gtsummary::tbl_regression()` with `tidycmprsk::crr()` models, the {tidycmprsk} package must be loaded (to access the S3 methods below). In the next release of gtsummary, tidycmprsk will no longer be a dependency.
  - Added {broom.helpers}, {cardx}, and {aod} to Suggests field.
  - Added `tbl_regression.tidycrr()` method.
  - Added `global_pvalue_fun.tidycrr()` method, which changes the default calculation in `gtsummary::add_global_p(anova_fun)` to use the Wald test.

# tidycmprsk 1.0.0

* Corrected a previous regression bug and `cuminc()` can again handle models with no observed censoring. (#89)

* Performance improvements to `cuminc()`. (@pteridin; #73)

* The `autoplot()` method for `cuminc()` objects is now defunct in favor of `ggsurvfit::ggcuminc()`. (#100)

* Updates ahead of the {purrr} v1.0 release.

# tidycmprsk 0.2.0

* The `autoplot.tidycuminc()` method has been deprecated in favor of `ggsurvfit::ggcuminc()`. (#81)

* Fix in `cuminc()` when one or more strata levels has no observations with one of the outcomes observed.

* Updated `cuminc()` and `crr()` to drop un-observed factor levels before processing the data. Previously, un-observed levels led to a singularity error. (#76)

* Bug fix in `tidy.tidycuminc()` when times specified are after the maximum follow-up time. (#77)

* Re-wrote an `ifelse()` statement to avoid a warning when tied times are present in `cuminc()`. (#74)

# tidycmprsk 0.1.2

* The `"strata"` column in tidied `cuminc()` results is now a factor. (#62)

* Fix in the documentation for `tbl_cuminc()` that resulted in a UTF-related error on Debian system R CMD Check. (#65)

* Adding `vcov()` method for `crr()` models (#63)

# tidycmprsk 0.1.1

* Adding the `tbl_cuminc()` function (similar to `gtsummary::tbl_survfit()`). Also added related methods, `add_n.tbl_cuminc()`, `add_nevent.tbl_cuminc()`, `add_p.tbl_cuminc()`, and `inline_text.tbl_cuminc()`

* Updated the definition of `n.event` and `n.censor` in `tidy.tidycuminc()` to match `summary.survfit()` when users specify `tidy.tidycuminc(times=)`. We now report the cumulative number of events/censors in the interval prior to the specified times. (#49)

* Added `crr(conf.level=)` and `cuminc(conf.level=)` arguments. The tidy data frame returned with each of these objects now has confidence limits that match `conf.level=`. (#45)

* The `tidy.ticycrr()` and `tidy.ticycuminc()` `conf.level=` defaults have been updated to `x$conf.level` passed in the original model call.

* Now returning the the number at risk, number of events, and the number of censored observations in `tidy.tidycuminc()`.

* Updated the default value to `tidy.tidycuminc(conf.int = TRUE)` from `FALSE`

* Update the `crr()` and `cuminc()` print methods for more clarity.

* Update documentations for `cuminc()` method with an explicit formula for the confidence interval.

* Returning the cumulative number of events and the cumulative number of censored observations in `tidy.tidycuminc()`.

* The `autoplot()` figure will now display the first outcome only as default. Previously, all outcomes were shown by default. 

# tidycmprsk 0.1.0

* Initial Release.
