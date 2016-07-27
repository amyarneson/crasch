library(crasch)
context("item.analysis")

test_that("item.analysis calculates correct output", {
  # table 1 - overall items
    # counts of non-missing (Count) and missing responses (Missing)
      expect_equal(item.analysis(AMY)[[1]]$Count +
                     item.analysis(AMY)[[1]]$Missing, AMY$estSummary$N)
      expect_equal(item.analysis(SUP)[[1]]$Count +
                     item.analysis(SUP)[[1]]$Missing, SUP$estSummary$N)
  # table 2 - steps
    # category counts
      expect_equal(item.analysis(AMY)[[2]]$Count[item.analysis(AMY)[[2]]$Count!=0],
                   unlist(apply(AMYwide, 2, table)) )
      expect_equal(item.analysis(SUP)[[2]]$Count[item.analysis(SUP)[[2]]$Count!=0],
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
      expect_equal(item.analysis(AMY)[[1]]$Construct,
                   AMY$consInfo$short.name[match(AMY$itemInfo$cons.ID,
                                                 AMY$consInfo$cons.ID)])
      expect_equal(item.analysis(SUP)[[1]]$Construct,
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
