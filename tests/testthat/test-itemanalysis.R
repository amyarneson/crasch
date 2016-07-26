library(crasch)
context("item.analysis")

test_that("item.analysis produces correct output", {
  expect_equal(item.analysis(AMY)[[2]]$Count[item.analysis(AMY)[[2]]$Count != 0],
               unlist(apply(AMYwide, 2, table)) )
})
