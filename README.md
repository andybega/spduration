
# spduration
*********

[![Travis CI Build Status](https://magnum.travis-ci.com/andybega/spduration.svg?token=ji8RFNCqNWpGghfYBt4p&branch=master)](https://magnum.travis-ci.com/andybega/spduration)

spduration implements a split-population duration model for duration data with time-varying covariates where a significant subset of the population or spells will not experience failure. 

To install: 

* the latest release version: `install.packages("spduration")`
* the latest development version: `install_github("andybega/spduration")`

**By**: Daniel W. Hill, Nils Metternich, Andreas Beger ([andreas.beger@duke.edu](mailto:andreas.beger@duke.edu))

License: [GNU GPL 3.0](http://www.gnu.org/licenses/)

Please cite or reference:  

Andreas Beger, Daniel W. Hill, Daina Chiba, Nils Metternich, Shahryar
  Minhas and Michael D. Ward (2014). spduration: Split-Population Duration
  (Cure) Regression. R package version 0.12.

@Manual{,
  title = {spduration: Split-Population Duration (Cure) Regression},
  author = {Andreas Beger and Daniel W. Hill and Daina Chiba and Nils Metternich and Shahryar Minhas and Michael D. Ward},
  year = {2014},
  note = {R package version 0.12},
}

To do
---

 * forecast plot function
 * prediction/forecast uncertainty
 * add stats for distinguishing baseline hazard form
 * add exponential
 * P. Brandt suggestion about better starting values