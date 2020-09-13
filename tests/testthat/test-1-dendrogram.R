# Test functionality using testthat library
#
# Author: Andrie
#------------------------------------------------------------------------------


test_that("data_dendrogram() returns a correct classes", {
  hc <- hclust(dist(USArrests), "ave")
  dhc <- as.dendrogram(hc)

  expect_s3_class(dendro_data(dhc, type = "rectangle"), "dendro")

  ddata <- dendro_data(dhc, type = "rectangle")
  expect_s3_class(ddata$segments, "data.frame")
  expect_s3_class(ddata$labels, "data.frame")
  expect_equal(nrow(ddata$segments), 196)

  ddata <- dendro_data(dhc, type = "triangle")
  expect_s3_class(ddata$segments, "data.frame")
  expect_s3_class(ddata$labels, "data.frame")
  expect_equal(nrow(ddata$segments), 98)
})

#------------------------------------------------------------------------------

test_that("undefined model type throws error", {
  expect_error(dendro_data(USArrests))
})
