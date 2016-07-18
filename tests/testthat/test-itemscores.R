library(crasch)
context("item.scores")

AMY <- craschR(scores = AMYwide, itemInfo = AMYitem, consInfo = AMYcons,
               varsInfo = AMYvars, estPackage = "TAM", retainOrig = FALSE,
               consecutive = TRUE, writeout = FALSE)
SUP <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
               varsInfo = NULL, estPackage = "TAM", retainOrig = FALSE,
               consecutive = FALSE, writeout = FALSE)

test_that("item.scores produces expected warnings", {
  expect_error(bad <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
                              varsInfo = NULL, estPackage = "TAM", retainOrig = FALSE,
                              consecutive = TRUE, writeout = FALSE),
               "Unidimensional analysis cannot be consecutive. Use consecutive=FALSE."
               )
  expect_error(bad <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
                              varsInfo = NULL, estPackage = "TAM", retainOrig = FALSE,
                              consecutive = FALSE, writeout = FALSE)
               )
})

test_that("item.scores works w/different combinations of arguments", {
  # uniD
  expect_equal(length(item.scores(SUP)), 1)
  expect_equal(length(item.scores(SUP)$SUP), 2)
  # multiD
  expect_equal(length(item.scores(AMY)), 2)
  expect_equal(length(item.scores(AMY, dim = 2)), 1)
  expect_equal(length(item.scores(AMY, dim = 2, freqs = FALSE)), 1)
})

test_that("item.scores produces expected errors",{
  # uniD with invalid dim specified
  expect_error(item.scores(SUP, dim = 3)
               )
})
