context("hclust")

test_that("data_hclust() returns the correct classes", {
  hc <- hclust(dist(USArrests), "ave")

  expect_is(dendro_data(hc, type = "rectangle"), "dendro")

  ddata <- dendro_data(hc, type = "rectangle")
  expect_is(segment(ddata), "data.frame")
  expect_is(label(ddata), "data.frame")
  expect_equal(nrow(segment(ddata)), 196)
})
