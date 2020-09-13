
test_that("data_tree() returns the correct classes", {
  skip_if_not_installed("tree")

  data(cpus, package = "MASS")
  cpus.ltr <- tree::tree(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax, data = cpus)

  expect_s3_class(dendro_data(cpus.ltr), "dendro")
  tdata <- dendro_data(cpus.ltr)
  expect_s3_class(segment(tdata), "data.frame")
  expect_s3_class(label(tdata), "data.frame")
  expect_s3_class(leaf_label(tdata), "data.frame")
})
