# Test functionality using testthat library
#
# Author: Andrie
#------------------------------------------------------------------------------

# tree

# data(iris)
# iris <- iris[, -5]

#------------------------------------------------------------------------------

context("dendrogram")

test_that("data_dendrogram() returns a correct classes", {
  hc <- hclust(dist(USArrests), "ave")
  dhc <- as.dendrogram(hc)

  expect_is(dendro_data(dhc, type = "rectangle"), "dendro")

  ddata <- dendro_data(dhc, type = "rectangle")
  expect_that(ddata$segments, is_a("data.frame"))
  expect_that(ddata$labels, is_a("data.frame"))
  expect_that(nrow(ddata$segments), equals(196))

  ddata <- dendro_data(dhc, type = "triangle")
  expect_that(ddata$segments, is_a("data.frame"))
  expect_that(ddata$labels, is_a("data.frame"))
  expect_that(nrow(ddata$segments), equals(98))
})

#------------------------------------------------------------------------------

test_that("undefined model type throws error", {
  expect_that(dendro_data(USArrests), throws_error())
})
