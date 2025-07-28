# Package development -----------------------------------------------------
#
#   keep track in news.md
#

library("devtools")
library("usethis")
library("pkgdown")

# Before syncing to Github
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()
rmarkdown::render("README.Rmd")

# for non-import dependencies
if (!requireNamespace("pkg", quietly = TRUE)) {
  stop("Pkg needed for this function to work. Please install it.",
       call. = FALSE)
}

# maybe rebuild model.coups data, if changes in `spdur` object
rebuild_model.coups <- function() {
  load("data/coups.rda")
  dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
                            freq="year")
  model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups)
  save(model.coups, file = "data/model.coups.rda")
}
#rebuild_model.coups()


# Test coverage
cov <- package_coverage()
shine(cov)


# Package release ---------------------------------------------------------

library("devtools")
library("pkgdown")

#   Update NEWS
#
#   Update version in DESCRIPTION:
#     [major][minor][patch][dev]
#       - major: not backwards compatible
#       - minor: feature enhancements
#       - patch: fixes bugs
#       - dev (9000): working version

devtools::check(remote = TRUE, manual = TRUE)

check_win_devel()
check_win_release()
check_win_oldrelease()

# Valgrind
tar_file <-  tail(dir("..", pattern = "spduration_", full.names = T), 1)
system(sprintf("R CMD check --as-cran --use-valgrind ../%s", tar_file))

# commit to git for travis
# https://travis-ci.org

#   once emails are in and travis is done:
#
#   Update cran-comments.md

R.Version()$version.string

devtools::release()

# update local install
desc <- readLines("DESCRIPTION")
vers <- desc[grep("Version", desc)]
vers <- stringr::str_extract(vers, "[0-9\\.]+")
devtools::install_url(paste0("file://", getwd(), "/../spduration_", vers, ".tar.gz"))

