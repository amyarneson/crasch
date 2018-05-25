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

# note that Split is capitalized so that this does not get interpreted as a
#  method for the base function split()
Split.halves <- function(results1, results2) {
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

  r <- cor(pers1, pers2, method = "pearson", use = "complete.obs")
  as.numeric( (2*r) /(1 + r) )
}

###############################################################################
#' Returns and plots the mean location trajectories by category per item.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param itemOrder A numeric vector that specifies which items from the output
#'   should be graphed.  If \code{NULL}, all items will be graphed.  Note that
#'   items scored on different dimensions will be graphed separately.
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", any RColorBrewer palette or a vector containing any number of
#'    colors.  The colors from each palette will be circulated through items.
#' @param writeout A logical indicating whether the graphic should be written to
#'   to your working directory as your specified \code{imageType}.  If
#'   \code{TRUE}, the file name will begin \code{meanTraj}, followed by an index
#'   for the dimension, and the \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return A list containing matrices of the mean location estimates by score
#'   and their standard deviations.
#'
#' @export

mn.traj <- function(results, itemOrder = NULL,
                    palette = "BASS",
                    writeout = FALSE, imageType = "pdf", fileSuffix= NULL) {
  origPar <- par(no.readonly = TRUE) # to reset graphical parameters after
  checkResults(results)
  checkWrite(writeout, fileSuffix)
  checkImageType(imageType)
  checkItemOrder(itemOrder, results$itemInfo)

  if (is.null(itemOrder)) {
    itemOrder <- 1:results$estSummary$I
  }

  itemInfo <- results$itemInfo[itemOrder,]
  if (length(itemOrder == 1)) {
    scoresRecoded <- data.frame(results$scoresRecoded[,itemOrder])
    row.names(scoresRecoded) = row.names(results$scoresRecoded)
    colnames(scoresRecoded) = colnames(results$scoresRecoded)[itemOrder]
  } else {
    scoresRecoded <- results$scoresRecoded[,itemOrder]
  }
  D <- which(results$consInfo$cons.ID %in% unique(itemInfo$cons.ID))

  output <- list()

  for (d in D) {
    inclItems <- itemInfo$cons.ID == results$consInfo$cons.ID[d]

    meanLocs <- matrix(NA, nrow = sum(inclItems),
                                 ncol = ncol(results$itemPars))
    row.names(meanLocs) <- itemInfo$item.name[inclItems]
    colnames(meanLocs) <- paste0("recodedScore", 0:(ncol(meanLocs) - 1))
    sdLocs <- meanLocs

    j <- 1
    for (i in which(inclItems)) {
      meanLocs[j,] <- sapply(0:(sum(as.matrix(itemInfo[i, 6:ncol(itemInfo)])) - 1),
                               function(x) {
        mean(results$persPars[scoresRecoded[, i] == x, d],
             na.rm = TRUE)
      })[1:ncol(meanLocs)]

      sdLocs[j,] <- sapply(0:(sum(as.matrix(itemInfo[i, 6:ncol(itemInfo)])) - 1),
                           function(x) {
        sd(results$persPars[scoresRecoded[, i] == x, d],
           na.rm = TRUE)
      })[1:ncol(meanLocs)]
      j = j + 1
    }

    meanLocsLong <- reshape(data.frame(meanLocs), varying = 1:ncol(meanLocs),
                            v.names = "X", direction = "long")
    meanLocsLong = meanLocsLong[complete.cases(meanLocsLong), ]
    meanLocsLong$score = meanLocsLong$time - 1

    if (writeout) {
      eval(parse(text = paste0(imageType, "('meanTraj", d, fileSuffix, ".",
                               imageType, "')")))
    }

    # figure out colors
    if (length(palette) == 1) {
      if (identical(palette, "BASS")) {
        color <- "#80b1d3"
      } else if (palette %in% row.names(brewer.pal.info)) {
        color <- brewer.pal(min(length(inclItems),
          brewer.pal.info$maxcolors[which(row.names(brewer.pal.info) == palette)]),
          palette)
      } else if (all(areColors(palette))) {
        color <- palette
      } else {
      stop('Invalid palette argument.')
      }
    } else if (all(areColors(palette))) {
        color <- palette
    } else {
      stop('Invalid palette argument.')
    }

    interaction.plot(x.factor = meanLocsLong$score,
                     trace.factor = meanLocsLong$id,
                     response = meanLocsLong$X,
                     legend = FALSE, col = color, lty = 1, lwd = 2,
                     ylab = "Mean Location", xlab = "Numeric Score",
                     main = "Mean Location Trajectories by Item")
    mtext(as.character(results$consInfo$short.name[d]))

    if (writeout) {
      dev.off()
    }

    output[[which(D == d)]] <- list(meanLocations = meanLocs,
                                    sdLocations = sdLocs)
  }

  par(origPar)
  names(output) <- results$consInfo$short.name[D]
  return(output)
}


###############################################################################
#' Calculates Spearman's rho for fully dichotomous, unidimensional data.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param whichStep An integer indicating which step you want to compare (only
#'   applicable if you have 'equivalently scored' polytomous items meaning that
#'   all items are scored into the same levels and all scores are present in
#'   your data).
#' @param expOrder A vector with the expected ordering of the items.
#'
#' @return The split-halves reliability coefficient.
#'
#' @export

Sp.rho <- function(results, whichStep = 1, expOrder) {
  checkResults(results)

  if (!all.equal(sort(expOrder), 1:results$estSummary$I)) {
    stop('Invalid expOrd argument.')
  }

  itemThres <- results$itemThres

  if (!(whichStep %in% 1:(ncol(itemThres)))) {
    stop('Invalid whichStep argument.')
  }

  # needs to be a matrix
  # can't choose a step that doesn't exist
  # only for dichotomous or 'equivalently scored' data
  if (!(is.matrix(itemThres) | whichStep <= ncol(itemThres) |
        !anyNA(itemThres) | nrow(results$consInfo) == 1)) {
    stop('You cannot compute Spearmans Rho with your data.')
  }

  cor(expOrder, as.numeric(rank(itemThres[,whichStep])), method = "spearman")
}
