# Development cycle
# 
#   1. do stuff to the package
# 
#   2. Keep track of major changes in NEWS.md
#
#   3. When ready to build new version:
#
#       - update date and version in DESCRIPTION, using Hadley's scheme: 
#         http://r-pkgs.had.co.nz/description.html#version
#
#   4. Check/test the packge, fix bugs, errors, warnings, notes that come up
#

library(devtools)

# Check local
devtools::check()

# Check Windows, R devel and release
build_win(version = "R-release")
build_win(version = "R-devel")

# commit to git and check travis
# https://magnum.travis-ci.com

#
#   5. Update cran-comments.md
#
R.Version()$version.string

devtools::build()
devtools::release()