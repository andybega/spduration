## Test environments
* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE <br>
Maintainer: 'Andreas Beger <adbeger@gmail.com>' <br>
New submission

On `win-builder.r` and with Travis CI I get one additional note:

* checking package dependencies ... NOTE <br>
  No repository set, so cyclic dependency check skipped

## Downstream dependencies

There aren't any.