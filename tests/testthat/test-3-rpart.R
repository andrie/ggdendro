context("rpart")

test_that("data_tree() returns the correct classes", {
  
  if(!require("rpart", quietly = TRUE)) skip("package rpart not available")
  
  fit <- rpart(Kyphosis ~ Age + Number + Start,   method="class", data=kyphosis)
  tdata <- dendro_data(fit)
  expect_is(tdata, "dendro")
  expect_is(segment(tdata), "data.frame")
  expect_is(label(tdata), "data.frame")
  expect_is(leaf_label(tdata), "data.frame")
  
})

