## Test environments
* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

local - nothing

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Andreas Beger <adbeger@gmail.com>'
New submission

On `win-builder.r` I get one additional note with R-release, but not with R-devel:

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped

## Downstream dependencies

There aren't any.