## Test environments

* Ubuntu 18.04 LTS (on github actions), devel, release, oldrel-1, oldrel-2, oldrel-3
* Windows Server 2019 (on github actions), release, 3.6
* macOS (on github actions), release
* win-builder devel

## revdepcheck results

I checked 5 reverse dependencies using the `revdepcheck::revdep_check()` tool, which reported no new issues.
But I submitted the package two days ago to CRAN, and the CRAN checks found one new issue in the `ggsurvfit` package: a package I also maintain.
I investigated the issue and I updated a unit test to correct the failure in `ggsurvfit`.
I will submit an updated `ggsurvfit` after `tidycmprsk` is accepted.
Thank you for the thorough checks!

## R CMD check results

Maintainer: 'Daniel D. Sjoberg <danield.sjoberg@gmail.com>'
  
## Additional Comments

Thank you for your time.
