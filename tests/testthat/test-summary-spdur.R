
data("model.coups")

test_that("summary method works", {
  
  expect_error(out <- summary(model.coups), NA)
  
})

test_that("print method works", {
  
  expect_error(print(out))
  
})