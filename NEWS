# spduration 0.15.1

* Fixes bug in `summary.spdur` that would return wrong estimates for `log(alpha)`.
* Better column names for `summary` and `xtable` methods.
* Fixes test error after `testthat` update.

# spduration 0.15.0

* Added accessor methods for `terms`, `model.matrix`, `coef`, and `vcov`. 
* Added `fitted` and `residuals` methods.
* Fixes issue #17, which led to errors in `summary.spdur` when called on a model 
with factor variables or without intercept terms. 
* Fixes an issue with the hazard rate plot confidence intervals (`plot_hazard(ci = TRUE)`), where CIs coud be wrong because coefficients were sampled by equation rather than using the full variance covariance matrix.

# spduration 0.14.0

* Added Belkin & Schofer 2003 coup data in `data(bscoup)`, see `?bscoup`. 
* Replaced `plot_hazard1` and `plot_hazard2` with `plot_hazard`, added support for loglog models, and other internal streamlining of plotting code. 
* Partial fix for summarizing and printing results for models without intercept term.
* Removed documentation and export for several internal functions.

# spduration 0.13.1

* Initial CRAN release
