
test_that("data_hclust() returns the correct classes", {
  hc <- hclust(dist(USArrests), "ave")

  expect_s3_class(dendro_data(hc, type = "rectangle"), "dendro")

  ddata <- dendro_data(hc, type = "rectangle")
  expect_s3_class(segment(ddata), "data.frame")
  expect_s3_class(label(ddata), "data.frame")
  expect_equal(nrow(segment(ddata)), 196)
})
