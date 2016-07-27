###############################################################################
#' Calculates the split halves reliability coefficient.
#'
#' @param results1 The output from a run of \code{craschR}. (link?)
#' @param results2 The output from a run of \code{craschR}.  The estimation
#'   must be run on the same respondents with the same estimation settings.  The
#'   only difference in the two runs of \code{craschR} should be the items used.
#'   Note that you can only include items scored on the same dimension.
#'
#' @return The split-halves reliability coefficient.
#'
#' @export

split.halves <- function(results1, results2) {
  checkResults(results1)
  checkResults(results2)

  if (!identical(results1$estSummary$estPackage,
                 results2$estSummary$estPackage)) {
    stop('estPackage must match in results1 & results2.')
  }
  if (!identical(results1$estSummary$persMethod,
                 results2$estSummary$persMethod)) {
    stop('persMethod must match in results1 & results2.')
  }
  if (!(identical(row.names(results1$persPars), row.names(results2$persPars)))){
    stop('The same respondents (in the same order) must be included in results1 & results2.')
  }
  if (!identical(results1$consInfo, results2$consInfo)) {
    stop('consInfo objects must be identical for results1 & results2.')
  }
  if (nrow(results1$consInfo) != 1) {
    stop('Only unidimensional analyses may be used for results1.')
  }
  if (nrow(results2$consInfo) != 1) {
    stop('Only unidimensional analyses may be used for results2.')
  }

  pers1 <- results1$persPars
  pers2 <- results2$persPars

  # remove Inf, -Inf from persPars (if MLE or WLE)
  if (min(pers1[,1], na.rm = TRUE) == -Inf | max(pers1[,1], na.rm = TRUE) == Inf) {
    pers1[c(which(pers1[,1] == -Inf), which(pers1[,1] == Inf)),] = NA
  }
  if (min(pers2[,1], na.rm = TRUE) == -Inf | max(pers2[,1], na.rm = TRUE) == Inf) {
    pers2[c(which(pers2[,1] == -Inf), which(pers2[,1] == Inf)),] = NA
  }

  r = cor(pers1, pers2, method = "pearson", use = "complete.obs")
  as.numeric( (2*r) /(1 + r) )
}
