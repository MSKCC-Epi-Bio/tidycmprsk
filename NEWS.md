# tidycmprsk (development version)

* The `cmprsk::timepoints()` function returns cumulative incidence estimates with the timepoints as character column names. To avoid potential rounding issues converting numeric times to character and back to numeric, we now pass the replace the character times with numeric.

* Update the `crr()` print method for more clarity.

# tidycmprsk 0.1.0

* Initial Release.
