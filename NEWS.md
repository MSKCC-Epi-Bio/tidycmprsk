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
