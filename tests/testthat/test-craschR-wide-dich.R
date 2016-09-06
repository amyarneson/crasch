library(crasch)
context("craschR: wide format, dichotomous data")

test_that("craschR persMethod argument (dichotomous data)", {
  test <- craschR(scores = DIwide, itemInfo = DIitem, consInfo = DIcons,
                  estPackage = "TAM", retainOrig = TRUE,
                  persMethod = "WLE", consecutive = FALSE, writeout = FALSE)
  test <- craschR(scores = DIwide, itemInfo = DIitem, consInfo = DIcons,
                  estPackage = "TAM", retainOrig = TRUE,
                  persMethod = "MLE", consecutive = FALSE, writeout = FALSE)
})
