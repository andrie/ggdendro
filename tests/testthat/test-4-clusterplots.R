context("clusterplots and dendrograms")

test_that("dendrogram plots", {
  require(ggplot2)
  hc <- hclust(dist(USArrests), "ave")
  hcdata <- dendro_data(hc, type = "rectangle")
  p <- ggplot() +
    geom_segment(data = segment(hcdata), aes(x = x0, y = y0, xend = x1, yend = y1)) +
    geom_text(data = label(hcdata), aes(x = x, y = y, label = text)) +
    coord_flip() +
    scale_y_reverse(expand = c(0.2, 0))
  expect_that(p, is_a("ggplot"))
  # print(p)
})
