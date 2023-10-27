# tbl_cuminc() works

    Code
      cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>% tbl_cuminc(outcomes = letters)
    Condition
      Error in `tbl_cuminc()`:
      ! Error in `outcomes` argument specification.
      i Must be one or more of "death from cancer" and "death other causes".

---

    Code
      cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>% tbl_cuminc(statistic = letters)
    Condition
      Error in `tbl_cuminc()`:
      ! Argument `statistic` must be a string.

---

    Code
      cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>% tbl_cuminc(label = letters)
    Condition
      Error in `tbl_cuminc()`:
      ! Argument `label` must be a string.

---

    Code
      cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>% tbl_cuminc() %>% add_p()
    Condition
      Error:
      ! Cannot add a p-value without a stratifying variable.

