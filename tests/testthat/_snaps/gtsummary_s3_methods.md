# tbl_regression.tidycrr()

    Code
      as.data.frame(tbl_regression(crr(Surv(ttdeath, death_cr) ~ age + grade, trial)))
    Output
      11 cases omitted due to missing values
        **Characteristic** **log(HR)**  **95% CI** **p-value**
      1                Age        0.01 -0.01, 0.03         0.6
      2              Grade        <NA>        <NA>        <NA>
      3                  I        <NA>        <NA>        <NA>
      4                 II        0.06 -0.65, 0.77         0.9
      5                III        0.43  -0.20, 1.1         0.2

# global_pvalue_fun.tidycrr()

    Code
      as.data.frame(tbl, col_labels = FALSE)
    Output
        label estimate    conf.low p.value
      1   Age     0.01 -0.01, 0.03     0.6
      2 Grade     <NA>        <NA>     0.3
      3     I     <NA>        <NA>    <NA>
      4    II     0.06 -0.65, 0.77    <NA>
      5   III     0.43  -0.20, 1.1    <NA>

