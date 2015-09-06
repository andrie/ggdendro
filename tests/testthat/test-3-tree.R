context("tree")

test_that("data_tree() returns the correct classes", {
  if(!require("tree", quietly = TRUE)) skip("package tree not available")
  
  data(cpus, package="MASS")
  cpus.ltr <- tree(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax, data = cpus)

  expect_that(dendro_data(cpus.ltr), is_a("dendro"))
  tdata <- dendro_data(cpus.ltr)
  expect_that(segment(tdata), is_a("data.frame"))
  expect_that(label(tdata), is_a("data.frame"))
  expect_that(leaf_label(tdata), is_a("data.frame"))
  
})

