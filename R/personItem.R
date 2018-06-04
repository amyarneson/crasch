###############################################################################
#' Plots the standard error of measurement or test information function.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim Specify which dimension(s) to create graphic/tables for.  If
#'   \code{NULL}, output and graphics for each dimension will be produced.
#' @param type \code{c("SEM", "TIC", "IIC")}
#' @param completeOnly A logical indicating whether to graph points for
#'   respondents who received a score on all items or everyone, no matter what.
#' @param thetaGrid A vector containing the range of thetas for the function
#'   graph.
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", any RColorBrewer palette, or a vector containing 2 colors (the
#'   first for the curve and the second for the points).
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
  checkResults(results)
  checkWrite(writeout, fileSuffix)
  checkImageType(imageType)
  checkDim(dim, results$consInfo)
  if (!type %in% c("SEM", "TIC", "IIC")) {
    stop('Invalid type argument.')
  }
  if (!is.logical(completeOnly)) {
    stop('Invalid completeOnly argument.')
  }
  if (!is.numeric(thetaGrid)) {
    stop('Invalid thetaGrid argument.')
  }

  if (length(thetaGrid) < 100) {
    warning('It is recommended that thetaGrid contains >= 100 values.  Plot may look "choppy."  Interpret with caution.')
  }

  origPar = par(no.readonly = TRUE) # so graphical parameters can be reset after
  par(mai = c(1.36, 1.093333, 1.093333, 0.56),
      mar = c(5.1, 4.1, 4.1, 2.1),
      xpd = FALSE)

  if (identical(palette, "BASS")) {
    color <- c(curve = "gray52", points = "#80b1d3")
  } else if (length(palette) == 1) {
    if (palette %in% row.names(brewer.pal.info)) {
      color <- brewer.pal(3, palette)
    } else {
      stop('Invalid palette argument.')
    }
  } else if (all(areColors(palette)) & length(palette) == 2) {
    color <- palette
  } else {
    stop('Invalid palette argument.')
  }

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else if (is.numeric(dim)) {
    D <- dim
  }

  K <- ncol(results$itemThres)
  inclConsIDs <- results$consInfo$cons.ID[D]
  inclItem <- results$itemInfo$cons.ID %in% inclConsIDs
  itemInfo <- results$itemInfo[inclItem,]

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
    for (i in 1:sum(inclItem)) {
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
      mtext(as.character(itemInfo$item.name[i]))
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


################################################################################
#' A wrapper for the wrightMap() function from the WrightMap package.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim A numeric vector that specifies for which dimension(s) to create
#'   graphic/tables.  If \code{NULL}, output and graphics for each dimension
#'   will be produced.
#' @param byCat A logical indicating if items/steps should be grouped by the
#'   construct category.  If \code{FALSE}, items will be plotted in the order
#'   of \code{itemInfo}.
#' @param palette A character string indicating the color scheme.  Can be "BASS"
#'   or any RColorBrewer palette.  If you want to customize further, the
#'   \code{wrightMap()} function should be used directly, not this wrapper.
#' @param writeout A logical indicating whether the graphic should be written to
#'   to your working directory as your specified \code{imageType}.  If
#'   \code{TRUE}, the file name will begin \code{WM} and will include an index
#'   (if more than one graph is produced) and the \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return Wright Map graphic (multiple graphics will be produced if you have
#'   more than one dimension and ran consecutive analyses)
#'
#' @export

wm <- function(results, dim = NULL, byCat = FALSE, palette = "BASS",
               writeout = FALSE, imageType = "pdf", fileSuffix = NULL) {
  checkResults(results)
  checkWrite(writeout, fileSuffix)
  checkImageType(imageType)
  checkDim(dim, results$consInfo)
  if (!is.logical(byCat)) {
    stop('Invalid byCat argument.')
  }

  origPar = par(no.readonly = TRUE) # to reset graphical parameters after

  # if ordering by construct level, then the WMs MUST be consecutive (1 for each dim)
  if (byCat) {
    consecutive = TRUE
  } else {
    consecutive = results$estSummary$consecutive
  }

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else {
    D <- dim
  }

  itemInfo <- results$itemInfo
  itemThres <- results$itemThres

  if (consecutive) {
    thresList <- list()
    for (i in 1:length(D)) {
      # get the indices for the items on each construct
      thresList[[i]] <- which(results$itemInfo$cons.ID == results$consInfo$cons.ID[D[i]])
    }
  } else {
    thresList <- list(1:results$estSummary$I)
  }

  if (byCat) {
    itemXcat <- as.matrix(results$itemInfo[,6:ncol(results$itemInfo)])

    for (i in 1:results$estSummary$I) {
      # "remove" the empty categories
      if (length(results$empties[[i]]) > 0) {
        itemXcat[i, results$empties[[i]]] = FALSE
      }
      # "remove" the first TRUE for each item
      itemXcat[i, min(which(itemXcat[i,]))] = FALSE
    }

    # take the transpose
    itemXcat = t(itemXcat[,-1])
    # fill in the thresholds
    toGraph <- matrix(nrow = nrow(itemXcat), ncol = ncol(itemXcat))
    toGraph[itemXcat] <- c(t(results$itemThres))[!is.na(c(t(results$itemThres)))]
  } else {
    toGraph <- results$itemThres
  }

  if (length(palette) == 1) {
    if (palette == "BASS") {
      color <- c("black", "#80b1d3")
    } else if (palette %in% row.names(brewer.pal.info)) {
      color <- RColorBrewer::brewer.pal(3, palette)
    } else {
      stop('Invalid palette argument.')
    }
  } else {
    stop('Invalide palette argument.')
  }
  thresReturn <- list()

  for (i in 1:length(thresList)) {

    if (consecutive) {
      thetas <- results$persPars[,D[i]]
      cons <- consLabel <- paste0(results$consInfo$short.name[D[i]], " ")
      thresLabel <- matrix(rep(results$itemInfo$item.name, each = nrow(toGraph)),
                           nrow = nrow(toGraph), ncol = ncol(toGraph))
    } else {
      thetas <- results$persPars
      cons <- thresLabel <- NULL
      consLabel <- colnames(results$persPars)
    }

    if (byCat) {
      thres <- toGraph[,thresList[[i]]]
      xLabel <- results$consInfo[D[i], 5:ncol(results$consInfo)]
      thresPos <- c(2,4)
    } else {
      thres <- toGraph[thresList[[i]],]
      xLabel <- results$itemInfo$item.name[thresList[[i]]]
      thresPos <- 2
    }

    if (writeout) {
      if (!byCat) {
        fileName <- "-itemorder"
        if (imageType == "pdf" | imageType == "svg") {
          imgWidth <- min(14, length(thresList[[i]]))
          imgHeight <- 7
        } else {
          imgWidth <- min(480 * 2, 480/7 * length(thresList[[i]]))
          imgHeight <- 480
        }
      } else {
        fileName <- "-constructorder"
        if (imageType == "pdf" | imageType == "svg") {
          imgWidth <- max(6, 2*(ncol(results$consInfo) - 5))
          imgHeight <- 7
        } else {
          imgWidth <- max(6 * 480 / 7, 2 * 480 / 7 *(ncol(results$consInfo) - 5))
          imgHeight <- 480
        }
      }

      if (length(thresList)==1) {
        index <- NULL
      } else {
        index <- D[i]
      }

      eval(parse(text = paste0(imageType, "('WM", fileName, index, fileSuffix,
                               ".", imageType, "', width = ", imgWidth,
                               " , height = ", imgHeight, ")")))
    }

    wrightMap(thetas = thetas,
              thresholds = thres,
              main.title = paste0(cons, "Wright Map"),
              show.thr.lab = ncol(toGraph) > 1,
              axis.items = "",
              label.items = xLabel,
              label.items.srt = 45,
              dim.names = consLabel,
              thr.sym.pch = 21,
              thr.sym.col.fg = color[1],
              thr.sym.col.bg = color[2],
              thr.lab.pos = thresPos,
              return.thresholds = FALSE)

    thresReturn[[i]] <- thres
    colnames(thresReturn[[i]]) = results$itemInfo$item.name[thresList[[i]]]

    if (writeout) {
      dev.off()
    }
  }

  par(origPar)
  return(thresReturn)
}


