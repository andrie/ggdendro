
test_that("data_tree() returns the correct classes", {
  if (!require("rpart", quietly = TRUE)) skip("package rpart not available")

  fit <- rpart(Kyphosis ~ Age + Number + Start, method = "class", data = kyphosis)
  tdata <- dendro_data(fit)
  expect_s3_class(tdata, "dendro")
  expect_s3_class(segment(tdata), "data.frame")
  expect_s3_class(label(tdata), "data.frame")
  expect_s3_class(leaf_label(tdata), "data.frame")
})
