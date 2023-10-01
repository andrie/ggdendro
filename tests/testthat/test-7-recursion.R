test_that("large dendogram completes in reasonable time", {

  skip_on_cran()
  
  # find data file location
  file_location <- c(
    "tests/testthat/data/node_overflow_data.rds", "data/node_overflow_data.rds"
  )
  fl <- file_location[file.exists(file_location)]
  node_overflow_data <- readRDS(fl)
  
  # run hclust
  dhc <- hclust(dist(node_overflow_data), method = "average")
  
  # start ggdendro tests
  elapsed_time <- system.time({
    dhd <- dendro_data(dhc, type = "rectangle")
  })[[3]]
  
  
  # performance expectation: complete in less than 4 seconds
  expect_lt(elapsed_time, 10)
  
  expect_s3_class(dhd, "dendro")
  p2 <- ggdendrogram(dhd, type = "rectangle")

  expect_s3_class(p2, "ggplot")
})
