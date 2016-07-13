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


################################################################################
#' Plots in infit mean sqare statistics from \code{craschR} output.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param itemOrder A numeric vector that specifies which items from the output
#'   should be graphed.  If \code{NULL}, all items will be graphed.
#' @param params A string indicating for which parameters fit should be graphed.
#'   Can be \code{"items"} or \code{"steps"}.
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", "grey", or any RColorBrewer palette.
#' @param writeout A logical indicating whether the graphic should be written to
#'   to your working directory as your specified \code{imageType}.  If
#'   \code{TRUE}, the file name will begin \code{infit}, followed by the
#'   \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return One or more plots of the infit mean squares with "acceptable" region
#'   highlighted.
#'
#' @export

infit.MNSQ <- function(results, itemOrder = NULL, params = "items",
                       palette = "BASS",
                       writeout = FALSE, imageType = "pdf", fileSuffix = NULL) {
  origPar = par(no.readonly = TRUE) # to reset graphical parameters after

  # return error if all items are dichotomous (no steps!)

  if (is.null(itemOrder)) {
    plotOrder <- 1:results$estSummary$I
  } else if (is.numeric(itemOrder)) {
    plotOrder <- itemOrder
  } else {
    stop('Invalid itemOrder argument. Must be a vector with valid item numbers.')
  }

  if (results$estSummary$estPackage == "TAM") {
    if (params == "items") {
      toPlot <- data.frame(infit = results$itemFit$infit[plotOrder])
      row.names(toPlot) <- results$itemInfo$item.name[plotOrder]
    } else if (params == "steps") {
      if (is.null(itemOrder)) {
        toPlot <- data.frame(infit = results$itemFit$infit[(results$estSummary$I+1):nrow(results$itemFit)])
        row.names(toPlot) <- results$itemFit$item[(results$estSummary$I+1):nrow(results$itemFit)]
      } else {
        fullMatrix <- data.frame(
          as.matrix(stringr::str_split_fixed(as.character(results$itemFit$item[(results$estSummary$I+1):nrow(results$itemFit)]),
                                             '_step', n = 2)),
          infit = results$itemFit$infit[(results$estSummary$I+1):nrow(results$itemFit)])
        colnames(fullMatrix) <- c("item", "step", "infit")
        redMatrix <- reshape(fullMatrix, idvar = "item", timevar = "step",
                             direction = "wide")
        # dichotomous items will not show up w/step params - account for this
        if (any(apply(results$scoresRecoded, 2, max, na.rm = TRUE) == 1)) {
          insertRows <- which(apply(results$scoresRecoded, 2, max, na.rm = TRUE) == 1)
          for (i in insertRows) {
            if (i == 1) {
              redMatrix = rbind(rep(NA, ncol(redMatrix)), redMatrix)
            } else if (i == results$estSummary$I) {
              redMatrix = rbind(redMatrix, rep(NA, ncol(redMatrix)))
            } else {
              redMatrix = rbind(redMatrix[1:(i-1),], rep(NA, ncol(redMatrix)),
                                redMatrix[i:nrow(redMatrix),])
            }
          }
        }
        redMatrix = redMatrix[plotOrder,]
        redMatrix = reshape(redMatrix)
        redMatrix = redMatrix[complete.cases(redMatrix),]

        toPlot <- data.frame(infit = redMatrix[,3])
        row.names(toPlot) <- paste(redMatrix[,1], redMatrix[,2], sep = "_step")
      }
    } else {
      stop('Invalid params argument. Must be "items" or "steps".')
    }
  } else { # fill in once mirt portion is written
    if (params == "items") {

    } else if (params == "steps") {

    } else {
      stop('Invalid params argument. Must be "items" or "steps".')
    }
  }

  if (palette == "BASS") {
    color <- c(rgb(red = 128, green = 177, blue = 211, alpha = 127.5,
                   maxColorValue = 255), "#80b1d3")
  } else if (palette %in% row.names(brewer.pal.info)) {
    color <- brewer.pal(3, palette)
  } else if (all(areColors(palette)) & length(palette)==2) {
    color <- palette
  } else {
    stop('Invalid palette argument.')
  }

  if(writeout) {
    eval(parse(text = paste0(imageType, "('infit", fileSuffix, ".", imageType, "')")))
  }
  layout(matrix(1, nrow = 1))

  dotchart(x = rev(toPlot[,1]), rev(row.names(toPlot)),
           main = "Infit Mean Squares", xlab = "Infit MNSQ", ylab = "",
           xlim = c(min(.7, min(toPlot[,1])), max(1.4, max(toPlot[,1]))),
           pch = ".")
  rect(xleft = 3/4, xright = 4/3, ybottom = 0, ytop = (length(toPlot[,1]) + 1),
       col = color[1],
       border = NA)
  abline(v = 1, col = "darkgrey", lty = 3)
  points(rev(toPlot[,1]), 1:length(toPlot[,1]), pch = 21, bg = color[2])

  if(writeout) {
    dev.off()
  }

  par(origPar)
}


################################################################################
#' Plots the cumulative probability curves from \code{craschR} output.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param itemOrder A numeric vector that specifies which items from the output
#'   should be graphed.  If \code{NULL}, all items will be graphed.
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", "grey", or any RColorBrewer palette.
#' @param observed A logical indicating whether or not the observed proportions
#'   should be graphed as points along with the curve.
#' @param minCell A positive integer indicating the smallest cell value to plot
#'   an observed proportion.  Only applies if \code{observed=TRUE}.
#' @param focusTheta A numeric vector indicating at which logit values to plot
#'   a vertical line and print the probabilities for each category.  Only
#'   applies if \code{observed=FALSE}.
#' @param writeout A logical indicating whether the graphic should be written to
#'   to your working directory as your specified \code{imageType}.  If
#'   \code{TRUE}, the file name will begin \code{CPC}, an index, and the
#'   \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return One plot for each item is created.
#'
#' @export

CPC.graph <- function(results, itemOrder = NULL, palette = "BASS",
                      observed = FALSE, minCell = 8, focusTheta = c(-2,0,2),
                      writeout = FALSE, imageType = "pdf", fileSuffix = NULL) {
  origPar = par(no.readonly = TRUE) # to reset graphical parameters after

  if (is.null(itemOrder)) {
    itemInfo <- results$itemInfo
    itemThres <- results$itemThres
  } else if (is.numeric(itemOrder)) {
    itemInfo <- results$itemInfo[itemOrder,]
    itemThres <- results$itemThres[itemOrder,]
  } else {
    stop('itemOrder must be a numeric vector or NULL.')
  }
  I <- nrow(itemInfo)

  for (i in 1:I) {
    K_i <- sum(!is.na(itemThres[i,]))
    thres <- c(itemThres[i, !is.na(itemThres[i,])], Inf)

    if (palette == "BASS") {
      if (K_i == 2) {
        color = rgb(red = 128, green = 177, blue = 211, alpha = c(140,255),
                    maxColorValue = 255)
      } else {
        color = rgb(red = 128, green = 177, blue = 211,
                    alpha = seq(80, 255, length.out = K_i), maxColorValue = 255)
      }
      fillcol = "#80b1d3"
    } else if (palette %in% row.names(brewer.pal.info)) {
      color <- brewer.pal(max(K_i,3), palette)
      fillcol = "white"
    } else if (palette == "grey" | palette == "gray") {
      if (K_i == 2) {
        color = gray(level = c(.75, .6))
      } else {
        color = gray(level = seq(from = .75, to = .25, length.out = K_i))
      }
      fillcol = "gray"
    }  else {
      stop('palette must be "BASS", "grey", or an RColorBrewer palette.')
    }

    if (writeout) {
      eval(parse(text = paste0(imageType, "('ICC", itemInfo$item.ID[i],
                               fileSuffix, ".", imageType, "')")))
    }

    layout(matrix(1))
    par(mai = c(1.36, 1.093333, 1.093333, 0.56), mar = c(5.1, 4.1, 4.1, 2.1))
    plot(1, type = "n", xlim = c(-6, 6), ylim = c(0, 1), axes = FALSE,
         xlab = "Logits", ylab = "Probability",
         main = "Cumulative Category Probability Curves")
    mtext(as.character(itemInfo$item.name[i]))
    axis(1, at = seq(-6, 6, 2))
    axis(1, at = seq(-5, 5, 2), labels = FALSE)
    axis(2, at = seq(0, 1, .2), las = 1)
    axis(2, at = seq(.1, .9, .2), labels = FALSE)

    for (k in 1:K_i) {
      if (observed) {
        linecol = color[k]
        # group thetas at nearest .5 value for simplicity & to deal with sparse cells
        theta <- round(results$persPars[,which(results$consInfo$cons.ID ==
                                                 itemInfo$cons.ID[i])]/.5) * .5
        empPts = t(apply(prop.table(table(theta, results$scoresRecoded[,i]),
                                    margin = 1), 1,
                         function(x) {
                           cumsum(rev(x))
                         }))[,seq((K_i+1), 1, by = -1)]
        cellCts = t(apply(table(theta, results$scoresRecoded[,i]), 1,
                          function(x) {
                            cumsum(rev(x))
                          }))[,seq((K_i+1), 1, by = -1)]
      } else {
        linecol = "grey"
        x <- c(seq(from = -6, to = 6, length.out = 500))
        y1 <- c(boot::inv.logit(x - thres[k]))
        y2 <- c(boot::inv.logit(x - thres[k+1]))
        polygon(c(x, rev(x)), c(y1, rev(y2)), col = color[k], border = NA)
      }
      curve(boot::inv.logit(x - thres[k]), from = -6, to = 6, add = TRUE,
            col = linecol, lwd = 2)
      if (observed) {
        points(sort(unique(theta))[cellCts[,(k+1)] >= minCell],
               empPts[cellCts[,(k+1)] >= minCell,(k+1)], col = color[k],
               pch = 20)
      } else {
        abline(v = focusTheta, col = linecol, lty = 2, lwd = 2)
        for (a in 1:length(focusTheta)) {
          bounds <- boot::inv.logit(focusTheta[a] - thres)
          L <- placement <- rep(NA, length(bounds) + 1)
          for (k in 0:K_i) {
            if (k == 0) {
              upBd = 1
            } else {
              upBd = bounds[k]
            }
            if (k == K_i) {
              loBd = 0
            } else {
              loBd = bounds[k+1]
            }
            L[k+1] = paste0("p(",k,")=", round(upBd - loBd, 2))
            placement[k+1] = mean(c(upBd, loBd))
          }
          points(x = rep(focusTheta[a], length(bounds)), y = bounds, pch=21,
                 bg = fillcol)
          text(x = rep(focusTheta[a], length(bounds) + 1), y = placement,
               labels = L, pos = 4, cex = .5)
        }
      }
    }

    if (writeout) {
      dev.off()
    }
  }

  par(origPar)
}


################################################################################
#' Plots the item characteristic curves (or category characteristic curves, if
#' polytomous) from \code{craschR} output.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param itemOrder A numeric vector that specifies which items from the output
#'   should be graphed.  If \code{NULL}, all items will be graphed.
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", any RColorBrewer palette, or a vector containing 3 colors.
#' @param writeout A logical indicating whether the graphic should be written to
#'   to your working directory as your specified \code{imageType}.  If
#'   \code{TRUE}, the file name will begin \code{ICC}, an index, and the
#'   \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return One plot for each item is created.
#'
#' @export

ICC.graph <- function(results, itemOrder = NULL, palette = "BASS",
                      writeout = FALSE, imageType = "pdf", fileSuffix = NULL) {
  origPar = par(no.readonly = TRUE) # to reset graphical parameters after

  if (is.null(itemOrder)) {
    itemInfo <- results$itemInfo
    itemThres <- results$itemThres
  } else if (is.numeric(itemOrder)) {
    itemInfo <- results$itemInfo[itemOrder,]
    itemThres <- results$itemThres[itemOrder,]
  } else {
    stop('itemOrder must be a numeric vector or NULL.')
  }
  I <- nrow(itemInfo)
  lty <- rep(1:4, 3)  # no items with >12 categories will be graphed

  if (palette == "BASS") {
    color <- rep(c("#80b1d3", "darkgoldenrod1", "gray52"),4)
  } else if (palette %in% row.names(brewer.pal.info)) {
    color <- rep(brewer.pal(3, palette), 4)
  } else if (all(areColors(palette)) & length(palette)==3) {
    color <- rep(palette, 4)
  } else {
    stop('palette must be "BASS", an RColorBrewer palette, or a character with 3 valid color specifications.')
  }

  for (i in 1:I) {
    K_i <- sum(!is.na(itemThres[i,]))
    if (K_i > 12) {
      warning("Item ", i, " not graphed. Too many categories.")
    } else {
      thres <- c(-Inf, itemThres[i, !is.na(itemThres[i,])], Inf)

      if (writeout) {
        eval(parse(text = paste0(imageType, "('ICC", itemInfo$item.ID[i],
                                 fileSuffix, ".", imageType, "')")))
      }
      layout(matrix(1))
      par(mai = c(1.36, 1.093333, 1.093333, 0.56), mar = c(5.1, 4.1, 4.1, 2.1))

      plot(1, type = "n", xlim = c(-6, 6), ylim = c(0, 1), axes = FALSE,
           xlab = "Logits", ylab = "Probability", main="Category Characteristic Curves")
      mtext(as.character(itemInfo$item.name[i]))
      axis(1, at = seq(-6, 6, 2))
      axis(1, at = seq(-5, 5, 2), labels = FALSE)
      axis(2, at = seq(0, 1, .2), las = 1)
      axis(2, at = seq(.1, .9, .2), labels = FALSE)

      for (k in 0:K_i) {
        curve(boot::inv.logit(x - thres[k + 1]) - boot::inv.logit(x - thres[k + 2]),
              from = -6, to = 6, add = TRUE, lwd = 2, lty = lty[k + 1],
              col = color[k + 1])
      }

      cats <- as.logical(itemInfo[i, 6:ncol(itemInfo)])
      # deal with empty categories
      if (length(results$empties[[i]]) > 0) {
        cats[results$empties[[i]]] = FALSE
      }

      par(xpd = TRUE)
      legend(-6, .7, as.character(
        results$consInfo[which(results$consInfo$cons.ID == itemInfo$cons.ID[i]),
                         4:ncol(results$consInfo)])[cats], lty = lty, lwd = 2,
        col = color, cex = .6)

      if (writeout) {
        dev.off()
      }
    }
  }

  par(origPar)
}


