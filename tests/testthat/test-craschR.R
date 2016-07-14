library(crasch)
context("craschR: wide format, polytomous data")

test_that("craschR produces expected warnings", {
  expect_warning(SUP <- craschR(scores = SUPwide, itemInfo = SUPitem,
                                consInfo = SUPcons, varsInfo = SUPvars,
                                estPackage = "TAM", retainOrig = TRUE,
                                consecutive = FALSE, writeout = FALSE),
                 'No output was written to file. If you wish to write to file, use writeout=TRUE.')
})

test_that("craschR produces expected errors", {
  # mismatched input files
  expect_error(bad <- craschR(scores = SUPwide, itemInfo = AMYitem,
                              consInfo = SUPcons, varsInfo = SUPvars,
                              estPackage = "TAM", retainOrig = TRUE,
                              consecutive = FALSE, writeout = FALSE)
               )
  expect_error(bad <- craschR(scores = AMYwide, itemInfo = SUPitem,
                              consInfo = SUPcons, varsInfo = SUPvars,
                              estPackage = "TAM", retainOrig = TRUE,
                              consecutive = FALSE, writeout = FALSE)
  )

  # item scored at a level it should not have been

  # incorrect column labeling (will need to make a bad input file)
})
