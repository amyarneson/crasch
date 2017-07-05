library(crasch)
context("item.analysis")

SUP <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
               varsInfo = SUPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)
ADP <- craschR(scores = ADPwide, itemInfo = ADPitem, consInfo = ADPcons,
               varsInfo = ADPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)
AMY <- craschR(scores = AMYwide, itemInfo = AMYitem, consInfo = AMYcons,
               varsInfo = AMYvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = TRUE, writeout = FALSE)
DI <- craschR(scores = DIwide, itemInfo = DIitem, consInfo = DIcons,
              estPackage = "TAM", retainOrig = TRUE, writeout = FALSE)
# Note that ADM3analysis and ADM3consecutive need to be updated any time craschR() code changes!
# They take ~4 min each to run

test_that("item.analysis calculates correct output", {
  # table 1 - overall items
    # counts of non-missing (Count) and missing responses (Missing)
      expect_equal(item.analysis(AMY)[[1]]$Count +
                     item.analysis(AMY)[[1]]$Missing, rep(AMY$estSummary$N,
                                                          AMY$estSummary$I))
      expect_equal(item.analysis(SUP)[[1]]$Count +
                     item.analysis(SUP)[[1]]$Missing, rep(SUP$estSummary$N,
                                                          SUP$estSummary$I))
  # table 2 - steps
    # category counts
      expect_equivalent(item.analysis(AMY)[[2]]$Count[item.analysis(AMY)[[2]]$Count!=0],
                        unlist(apply(AMYwide, 2, table)) )
      expect_equivalent(item.analysis(SUP)[[2]]$Count[item.analysis(SUP)[[2]]$Count!=0],
                        unlist(apply(SUPwide, 2, table)) )
    # item scores (recoded)
      # not sure how to write this test?
      # watch for weirdness/errors during pilot
    # biserials
    # mean locations
    # sd_pers locations
})

test_that("item.analysis organizes output calculated elsewhere correctly", {
  # table 1 - overall items
    # construct
      expect_equal(as.character(item.analysis(AMY)[[1]]$Construct),
                   AMY$consInfo$short.name[match(AMY$itemInfo$cons.ID,
                                                 AMY$consInfo$cons.ID)])
      expect_equal(as.character(item.analysis(SUP)[[1]]$Construct),
                   SUP$consInfo$short.name[match(SUP$itemInfo$cons.ID,
                                                 SUP$consInfo$cons.ID)])
    # estimates
    # SEs of estimates
    # outfit
    # infit
  # table 2 - steps
    # construct
    # category names
    # estimates
    # SEs of estimates
    # thresholds
})
