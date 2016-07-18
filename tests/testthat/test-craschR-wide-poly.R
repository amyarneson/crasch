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
                              estPackage = "TAM", retainOrig = FALSE,
                              consecutive = FALSE, writeout = FALSE)
               )
  expect_error(bad <- craschR(scores = AMYwide, itemInfo = SUPitem,
                              consInfo = SUPcons, varsInfo = SUPvars,
                              estPackage = "TAM", retainOrig = FALSE,
                              consecutive = FALSE, writeout = FALSE)
               )

  # items scored at a level it should not have been
  # items 8 (q22) & 13 (q28)
  expect_error(bad <- craschR(scores = SUPwideError, itemInfo = SUPitem,
                              consInfo = SUPcons, varsInfo = NULL,
                              estPackage = "TAM", retainOrig = FALSE,
                              consecutive = TRUE, writeout = FALSE)
               )

  # incorrect column labeling (will need to make a bad input file)
  # items 1 & 15 in scores file
  expect_error(bad <- craschR(scores = SUPwideColError, itemInfo = SUPitem,
                              consInfo = SUPcons, varsInfo = NULL,
                              estPackage = "TAM", retainOrig = FALSE,
                              consecutive = FALSE, writeout = FALSE)
               )
  # in consInfo
  expect_error(bad <- craschR(scores = SUPwide, itemInfo = SUPitem,
                              consInfo = SUPconsColError, varsInfo = NULL,
                              estPackage = "TAM", retainOrig = FALSE,
                              consecutive = FALSE, writeout = FALSE)
               )
  # in itemInfo
  expect_error(bad <- craschR(scores = SUPwide, itemInfo = SUPitemColError,
                              consInfo = SUPcons, varsInfo = NULL,
                              estPackage = "TAM", retainOrig = FALSE,
                              consecutive = FALSE, writeout = FALSE)
               )
})
