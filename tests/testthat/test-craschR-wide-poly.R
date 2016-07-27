library(crasch)
context("craschR: wide format, polytomous data")

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

  expect_error(bad <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
                              varsInfo = NULL, estPackage = "TAM", retainOrig = FALSE,
                              consecutive = TRUE, writeout = FALSE),
               "Unidimensional analysis cannot be consecutive. Use consecutive=FALSE."
  )
})

test_that("craschR produces identical results for consecutive analyses run
          separately & run together", {
  ADProws = which(AMY$itemInfo$cons.ID == 22)
  SUProws = which(AMY$itemInfo$cons.ID == 11)
  # item Parameters
    expect_equal(AMY$itemPars[ADProws,], ADP$itemPars, tolerance = .01)
    expect_equal(AMY$itemPars[SUProws,], SUP$itemPars, tolerance = .01)
  # item parameter standard errors
    expect_equal(AMY$itemSEs[ADProws,], ADP$itemSEs, tolerance = .01)
    expect_equal(AMY$itemSEs[SUProws,], SUP$itemSEs, tolerance = .01)
  # thresholds
    expect_equal(AMY$itemThres[ADProws,], ADP$itemThres, tolerance = .01)
    expect_equal(AMY$itemThres[SUProws,], SUP$itemThres, tolerance = .01)
  # itemFit for items (not steps) - just infit and outfit
    expect_equal(c(as.matrix(AMY$itemFit[ADProws, c(2,6)])),
                 c(as.matrix(ADP$itemFit[1:13, c(2,6)])), tolerance = .01)
    expect_equal(c(as.matrix(AMY$itemFit[SUProws, c(2,6)])),
                 c(as.matrix(SUP$itemFit[1:15, c(2,6)])), tolerance = .1)
  # person estimates
    expect_equal(AMY$persPars[,2], c(as.matrix(ADP$persPars)), tolerance = .01)
    expect_equal(AMY$persPars[,1], c(as.matrix(SUP$persPars)), tolerance = .01)
  # person standard errors
    expect_equal(AMY$persSEs[,2], c(as.matrix(ADP$persSEs)), tolerance = .01)
    expect_equal(AMY$persSEs[,1], c(as.matrix(SUP$persSEs)), tolerance = .01)
  # persRaw
    expect_equal(AMY$persRaw[,2], c(as.matrix(ADP$persRaw)), tolerance = .01)
    expect_equal(AMY$persRaw[,1], c(as.matrix(SUP$persRaw)), tolerance = .01)
  # persMax
    expect_equal(AMY$persMax[,2], c(as.matrix(ADP$persMax)),
                      tolerance = .01)
    expect_equal(AMY$persMax[,1], c(as.matrix(SUP$persMax)),
                      tolerance = .01)
  # persFit
    expect_equal(AMY$persFit[[2]], ADP$persFit[[1]], tolerance = .01)
    expect_equal(AMY$persFit[[1]], SUP$persFit[[1]], tolerance = .01)
  # popDist
    expect_equal(AMY$popDist$mean[2], as.numeric(ADP$popDist$mean),
                 tolerance = .01)
    expect_equal(AMY$popDist$mean[1], as.numeric(SUP$popDist$mean),
                 tolerance = .01)
    expect_equal(AMY$popDist$var.cov[2,2], as.numeric(ADP$popDist$var.cov),
                 tolerance = .01)
    expect_equal(AMY$popDist$var.cov[1,1], as.numeric(SUP$popDist$var.cov),
                 tolerance = .01)
  # sepRel
    expect_equal(as.numeric(AMY$sepRel[2]), as.numeric(ADP$sepRel),
                 tolerance = .01)
    expect_equal(as.numeric(AMY$sepRel[1]), as.numeric(SUP$sepRel),
                 tolerance = .01)
  # nothing to check for estSummary
  # classicalStats
    expect_equal(AMY$classicalStats$ADP, ADP$classicalStats$ADP,
                 tolerance = .01)
  # skipping tests for empties
  # skipping tests for scoresRecoded
  # skipping tests for all input (varsItem, varsCons, varsInfo, scoresOrig)
})
