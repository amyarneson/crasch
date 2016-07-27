library(crasch)
context("Split.halves")

ADP1_EAP <- craschR(scores = ADPwide[,c(1,2,4,7,8,10,12)],
                    itemInfo = ADPitem[c(1,2,4,7,8,10,12),], consInfo = ADPcons,
                    estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                    writeout = FALSE)
ADP2_EAP <- craschR(scores = ADPwide[,c(3,5,6,9,11,13)],
                    itemInfo = ADPitem[c(3,5,6,9,11,13),], consInfo = ADPcons,
                    estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                    writeout = FALSE)
ADP1_WLE <- craschR(scores = ADPwide[,c(1,2,4,7,8,10,12)],
                    itemInfo = ADPitem[c(1,2,4,7,8,10,12),], consInfo = ADPcons,
                    estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                    persMethod = "WLE", writeout = FALSE)
ADP2_WLE <- craschR(scores = ADPwide[,c(3,5,6,9,11,13)],
                    itemInfo = ADPitem[c(3,5,6,9,11,13),], consInfo = ADPcons,
                    estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                    persMethod = "WLE", writeout = FALSE)
ADP1_MLE <- craschR(scores = ADPwide[,c(1,2,4,7,8,10,12)],
                    itemInfo = ADPitem[c(1,2,4,7,8,10,12),], consInfo = ADPcons,
                    estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                    persMethod = "MLE", writeout = FALSE)
ADP2_MLE <- craschR(scores = ADPwide[,c(3,5,6,9,11,13)],
                    itemInfo = ADPitem[c(3,5,6,9,11,13),], consInfo = ADPcons,
                    estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                    persMethod = "MLE", writeout = FALSE)

test_that("Split.halves produces expected errors", {
  expect_error(Split.halves(ADP1_MLE, ADP2_WLE),
               'persMethod must match in results1 & results2.')
})

test_that("Split.halves from different persMethods", {
  Split.halves(ADP1_EAP, ADP2_EAP)
  Split.halves(ADP1_MLE, ADP2_MLE)
  Split.halves(ADP1_WLE, ADP2_WLE)

  # check that Inf/-Inf are taken care of
  ADP2_MLE_inf <- ADP2_MLE
  ADP2_MLE_inf$persPars[c(11,18,19),1] <- Inf
  Split.halves(ADP1_MLE, ADP2_MLE_inf)
  expect_equal(Split.halves(ADP1_MLE, ADP2_MLE),
               Split.halves(ADP1_MLE, ADP2_MLE_inf))
})
