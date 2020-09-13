test_that("ggdendrogram plots and accepts ... parameters", {
  require(ggplot2)
  hc <- hclust(dist(USArrests), "ave")
  p1 <- ggdendrogram(hc)
  expect_s3_class(p1, "ggplot")
  p2 <- ggdendrogram(hc, size = 2)
  expect_s3_class(p2, "ggplot")
})
