###############################################################################
#' Creates item analysis tables.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim Specify which dimension(s) to create graphic/tables for.  If
#'   \code{NULL}, output and graphics for each dimension will be produced.
#' @param writeout A logical indicated whether the estimate objects should be
#'   written to your working directory as CSVs.  If \code{TRUE}, two files will
#'   be produced, one beginning \code{item-byitem} and the other
#'   \code{item-bystep} with any specified \code{fileSuffix}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return A list with the following entries:
#'   \item{byItem}{A matrix with classical statistics and descriptives by item.}
#'   \item{byStep}{A matrix with classical statistics and descriptives by step.}
#'
#' @export

item.analysis <- function(results, writeout = FALSE, filePrefix= NULL) {

  # create the byItem table (for TAM output)
  # note that the item name will be the row name
  byItem = cbind(Item_no = 1:results$estSummary$I,
                 Construct = results$consInfo$short.name[
                   match(results$itemInfo$cons.ID, results$consInfo$cons.ID)],
                 Count = colSums(!is.na(results$scoresRecoded)),
                 Missing = colSums(is.na(results$scoresRecoded)),
                 Estimate = results$itemPars[,1],
                 SE_est = results$itemSEs[,1],
                 results$itemFit[1:nrow(results$itemPars), c(2:4,6:8)]
  )

  #persProp table needed for point biserials
  persProp = results$persRaw / results$persMax

  #byStep table for PCM
  if (ncol(results$itemThres) > 1) {
    K_total = sum(colSums(results$itemInfo[,6:ncol(results$itemInfo)]))
    byStep = data.frame(Item = vector(mode = "character", length = K_total),
                        Construct = vector(mode = "character", length = K_total),
                        Cat = vector(mode = "character", length = K_total),
                        Count = as.vector(rep(NA, K_total), mode = "numeric"),
                        Score = as.vector(rep(NA, K_total), mode = "numeric"),
                        Estimate = as.vector(rep(NA, K_total), mode = "numeric"),
                        SE_est = as.vector(rep(NA, K_total), mode = "numeric"),
                        Threshold = as.vector(rep(NA, K_total), mode = "numeric"),
                        PtBiserial = as.vector(rep(NA, K_total), mode = "numeric"),
                        MeanPersLoc = as.vector(rep(NA, K_total), mode = "numeric"),
                        SD_PersLoc = as.vector(rep(NA, K_total), mode = "numeric"),
                        stringsAsFactors = FALSE)

    start <- 1
    for (i in 1:results$estSummary$I) {
      cons = which(results$consInfo$cons.ID == results$itemInfo$cons.ID[i]) # row index
      K_i = sum(as.logical(results$itemInfo[i, 6:ncol(results$itemInfo)]))
      fill = c(start:(start + K_i - 1))
      # item, construct, and category (construct specific) names
        byStep$Item[fill] = rep(results$itemInfo$item.name[i], K_i)
        byStep$Construct[fill] = rep(results$consInfo$short.name[cons], K_i)
        byStep$Cat[fill] = as.matrix(results$consInfo[cons,4:ncol(results$consInfo)][which(results$itemInfo[i,6:ncol(results$itemInfo)]==1)])
      # figure out which categories are empty
        if (length(results$empties[[i]]) > 0) {
          byStep[fill[(byStep$Cat[fill] %in% results$consInfo[cons,4:ncol(results$consInfo)][results$empties[[i]]] )], 4] = 0
          # skip all empty categories for remaining statistics
          fill = fill[!(byStep$Cat[fill] %in% results$consInfo[cons,4:ncol(results$consInfo)][results$empties[[i]]])]
        }
      # counts per category
        byStep$Count[fill] = table(results$scoresRecoded[,i])
      # recoded scores
        byStep$Score[fill] = 0:(length(fill) - 1)
      # location estimates and standard errors
        byStep$Estimate[fill[-1]] = results$itemPars[i,2:(length(fill))]
        byStep$SE_est[fill[-c(1,length(fill))]] = results$itemSEs[i,2:(length(fill) - 1)]
      # thresholds
        byStep$Threshold[fill[-1]] = results$itemThres[i,1:(length(fill) - 1)]
      # point biserials
        byStep$PtBiserial[fill] = sapply(fill,function(x) {
          -ltm::biserial.cor(persProp[!is.na(results$scoresRecoded[,i]),cons],
                             results$scoresRecoded[!is.na(results$scoresRecoded[,i]),i]==byStep$Score[x])
        })
      # person locations
        byStep$MeanPersLoc[fill] = sapply(fill,function(x) {
          mean(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons],
               na.rm = TRUE)
        })
        byStep$SD_PersLoc[fill] = sapply(fill,function(x) {
          sd(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons],
             na.rm = TRUE)
        })

        start = start + K_i
    }
  } else if (ncol(results$itemThres == 1)) {
      # create the byStep table (for dichotomous items)
      K_total = 2 * results$estSummary$I
      byStep = data.frame(Item = vector(mode = "character", length = K_total),
                          Construct = vector(mode = "character", length = K_total),
                          Cat = vector(mode = "character", length = K_total),
                          Count = as.vector(rep(NA, K_total), mode = "numeric"),
                          Score = as.vector(rep(NA, K_total), mode = "numeric"),
                          Estimate = as.vector(rep(NA, K_total), mode = "numeric"),
                          SE_est = as.vector(rep(NA, K_total), mode = "numeric"),
                          Threshold = as.vector(rep(NA, K_total), mode = "numeric"),
                          PtBiserial = as.vector(rep(NA, K_total), mode = "numeric"),
                          MeanPersLoc = as.vector(rep(NA, K_total), mode = "numeric"),
                          SD_PersLoc = as.vector(rep(NA, K_total), mode = "numeric"),
                          stringsAsFactors = FALSE)

      for(i in 1:results$estSummary$I) {
        fill = c(2*i - 1, 2*i)
        # item and category names
          byStep$Item[fill] = rep(results$itemInfo$item.name[i], 2)
          byStep$Construct[fill] = rep(results$consInfo$short.name[cons], 2)
          byStep$Cat[fill] = as.matrix(results$consInfo[cons,4:ncol(results$consInfo)][which(results$itemInfo[i,6:ncol(results$itemInfo)]==1)])
        # there will be no empty categories (if there were, tam() would have failed)
        # counts per category
          byStep$Count[fill] = table(results$scoresRecoded[,i])
        # recoded scores
          byStep$Score[fill] = 0:1
        # location estimates and standard errors
          byStep$Estimate[fill[2]] = results$itemPars[i,1]
          byStep$SE_est[fill[2]] = results$itemSEs[i,1]
        # thresholds
          byStep$Threshold[fill[2]] = results$itemThres[i]
        # point biserials
          byStep$PtBiserial[fill] = sapply(fill,function(x) {
            -ltm::biserial.cor(persProp[!is.na(results$scoresRecoded[,i]),cons],
                               results$scoresRecoded[!is.na(results$scoresRecoded[,i]),i]==byStep$Score[x])
          })
        # person locations
          byStep$MeanPersLoc[fill] = sapply(fill,function(x) {
            mean(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons],
                 na.rm = TRUE)
          })
          byStep$SD_PersLoc[fill] = sapply(fill,function(x) {
            sd(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons],
               na.rm = TRUE)
          })

      }
    }

  if(writeout) {
    write.csv(byItem, paste0("item-byitem", fileSuffix, ".csv"))
    write.csv(byStep, paste0("item-bystep", fileSuffix, ".csv"))
  }

  list(byItem,byStep)
}


###############################################################################
#' Plots the standard error of measurement or test information fucntion.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim Specify which dimension(s) to create graphic/tables for.  If
#'   \code{NULL}, output and graphics for each dimension will be produced.
#' @param type \code{c("SEM", "TIC", "IIC")}
#' @param completeOnly
#' @param thetaGrid
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", any RColorBrewer palette, or a vector containing 2 colors (the
#'   first for the line and the second for the points).
#' @param writeout A logical indicated whether the graph should be written to
#'   your working directory as your indicated \code{imageType}.  If \code{TRUE},
#'   the file name will begin \code{SEM} or \code{TIC} and will include an index
#'   (if more than one graph is produced) and the \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return A plot of the specified test information curve.  (Note: if you want
#'   category information curves, you can use the TAM function
#'   \code{IRT.informationCurves()} directly.)
#'
#' @export

info.graph <- function(results, dim = NULL, type = "SEM", completeOnly = TRUE,
                       palette = "BASS",
                       thetaGrid = seq(-6, 6, length.out = 100),
                       writeout = FALSE, imageType = "pdf", fileSuffix = NULL) {
  if (results$estSummary$D > 1 & !results$estSummary$consecutive) {
    stop('Information curves only available for unidimensional (or consecutive) analyses.')
  }

  origPar = par(no.readonly = TRUE) # so graphical parameters can be reset after
  par(mai=c(1.36,1.093333,1.093333,0.56),
      mar=c(5.1,4.1,4.1,2.1),
      xpd=FALSE)

  if (palette == "BASS") {
    color <- c("gray52", "#80b1d3")
  } else if (palette %in% row.names(brewer.pal.info)) {
    color <- brewer.pal(3, palette)
  } else if (all(areColors(palette)) & length(palette) == 2) {
    color <- palette
  } else {
    stop('palette must be "BASS", an RColorBrewer palette, or a character with 2 valid color specifications.')
  }

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else if (is.numeric(dim)) {
    D <- dim
  }

  K <- ncol(results$itemThres)
  inclConsIDs <- results$consInfo$cons.ID[D]
  inclItem <- results$itemInfo$cons.ID %in% inclConsIDs

  # polytomous item information as in Muraki (1993)
  probs <- lapply(thetaGrid, catProbs, itemThres = results$itemThres[inclItem,])
  infoItem <- sapply(1:length(probs), function(x) {
    expScore <- apply(probs[[x]], 2, function(y) {
      sum(0:K * y, na.rm = TRUE)
    })
    colSums(sapply(1:length(expScore), function(y) {
      ((0:K - expScore[y]) ^ 2) * probs[[x]][,y]
    }), na.rm = TRUE)
  })

  # information for each observed person
  infoPers <- results$scoresRecoded[,inclItem]
  if (type == "IIC") {
    thetas <- results$scoresRecoded[,inclItem]
  } else {
    thetas <- results$persPars
  }
  for (d in D) {
    fillCol <- which(results$itemInfo$cons.ID[inclItem] == results$consInfo$cons.ID[d])
    for (n in 1:results$estSummary$N) {
      infoPers[n, fillCol] <- infoItem[,which.min(abs(results$persPars[n, d] - thetaGrid))][fillCol]
      if (type == "IIC") {
        thetas[n, fillCol] = rep(thetaGrid[which.min(abs(results$persPars[n, d] - thetaGrid))], length(fillCol))
      } else {
        thetas[n, d] = thetaGrid[which.min(abs(results$persPars[n, d] - thetaGrid))]
      }
    }
  }
  # put NAs in if they didn't answer the question
    infoPers[is.na(results$scoresRecoded[,inclItem])] <- NA

  testInfo <- matrix(nrow = length(thetaGrid), ncol = results$estSummary$D)
  testInfoPers <- matrix(nrow = results$estSummary$N, ncol = results$estSummary$D)
  for (d in D) {
    itemsD <- which(results$itemInfo$cons.ID[inclItem] == results$consInfo$cons.ID[d])
    testInfo[,d] = colSums(infoItem[itemsD,])
    testInfoPers[,d] = rowSums(infoPers[,itemsD], na.rm = TRUE)
  }

  # flag complete cases
  if (completeOnly) {
    persToPlot <- complete.cases(results$scoresRecoded)
  } else {
    persToPlot <- rep(TRUE, nrow(results$persPars))
  }

  if (type == "IIC") {
    for (i in 1:length(inclItem)) {
      if(writeout) {
        eval(parse(text = paste0(imageType, "('IIC-item", i, fileSuffix, ".",
                                 imageType, "')")))
      }

      maxX <- 2 * ceiling(max(thetaGrid)/2)
      minX <- 2 * floor(min(thetaGrid)/2)

      plot(1, type = "n", xlim = c(min(thetaGrid), max(thetaGrid)),
           ylim = c(0, max(infoItem[i,])), axes = FALSE,
           xlab = "Logits", ylab = "Information",
           main = "Item Information Curve")
      mtext(as.character(results$itemInfo$item.name[i]))
      lines(thetaGrid, infoItem[i,], lwd = 2, col = color[1])
      axis(1, at = seq(minX, maxX, 2))
      axis(1, at = seq(minX+1, maxX-1, 2), labels = FALSE)
      axis(2)

      points(thetas[,i], infoPers[,i], pch = 21, col = color[1], bg = color[2])

      if (writeout) {
        dev.off
      }
    }
  } else if (type == "TIC") {
    for (d in D) {
      if(writeout) {
        eval(parse(text = paste0(imageType, "('TIC-cons", d, fileSuffix, ".",
                                 imageType, "')")))
      }

      maxX <- 2 * ceiling(max(thetaGrid)/2)
      minX <- 2 * floor(min(thetaGrid)/2)

      plot(1, type = "n", xlim = c(min(thetaGrid), max(thetaGrid)),
           ylim = c(0, max(testInfo[,d])), axes = FALSE,
           xlab = "Logits", ylab = "Information",
           main = "Test Information Curve")
      mtext(as.character(results$consInfo$short.name[d]))
      lines(thetaGrid, testInfo[,d], lwd = 2, col = color[1])
      axis(1, at = seq(minX, maxX, 2))
      axis(1, at = seq(minX+1, maxX-1, 2), labels = FALSE)
      axis(2)

      points(thetas[persToPlot, d], testInfoPers[persToPlot, d], pch = 21, col = color[1], bg = color[2])

      if (writeout) {
        dev.off
      }
    }
  } else if (type == "SEM") {
    for (d in D) {
      if(writeout) {
        eval(parse(text = paste0(imageType, "('SEM-cons", d, fileSuffix, ".",
                                 imageType, "')")))
      }

      maxX <- 2 * ceiling(max(thetaGrid)/2)
      minX <- 2 * floor(min(thetaGrid)/2)

      plot(1, type = "n", xlim = c(min(thetaGrid), max(thetaGrid)),
           ylim = c(0, max(1 / testInfo[,d])), axes = FALSE,
           xlab = "Logits", ylab = "SEM",
           main = "Standard Error of Measurement")
      mtext(as.character(results$consInfo$short.name[d]))
      lines(thetaGrid, 1 / testInfo[,d], lwd = 2, col = color[1])
      axis(1, at = seq(minX, maxX, 2))
      axis(1, at = seq(minX+1, maxX-1, 2), labels = FALSE)
      axis(2)

      points(thetas[persToPlot, d], 1 / testInfoPers[persToPlot, d], pch = 21, col = color[1], bg = color[2])

      if (writeout) {
        dev.off
      }
    }
  } else {
    stop('Invalid type specified. Choose "SEM", "TIC", or "IIC".')
  }

  par(origPar)
}
