context("ggdendrogram")

test_that("ggdendrogram plots and accepts ... parameters", {
  
  require(ggplot2)  
  hc <- hclust(dist(USArrests), "ave")
  p1 <- ggdendrogram(hc)
  expect_that(p1, is_a("ggplot"))
  p2 <- ggdendrogram(hc, size=2)
  expect_that(p2, is_a("ggplot"))
  #print(p1)
  #print(p2)
  
})
