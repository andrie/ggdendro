test_that("multiplication works", {

  # node_overflow_data <- readr::read_csv("data/mydata.csv", col_types = paste0(rep("d", 27), collapse = ""))
  node_overflow_data <- read_node_overflow_data()
  dhc <- hclust(dist(node_overflow_data), method = "average")
  dendro_data(dhc, type = "rectangle")
})
