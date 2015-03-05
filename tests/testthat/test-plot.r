data(model.coups)

test_that("", {
  expext_error(separationplot.spdur(model.coups, stat="foo"), 
               "obs is NULL, no default for stat=foo")
})
