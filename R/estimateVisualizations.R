################################################################################
#' Create a histogram of person estimates.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim A numeric vector that specifies for which dimension(s) to create
#'   graphic/tables.  If \code{NULL}, output and graphics for each dimension
#'   will be produced.
#' @param palette The color scheme for the histogram. Can be "BASS or any
#'   RColorBrewer palette name (the first 2 colors of the 3-color palette will
#'   be used). Can also specify a vector with 2 colors in any R-supported form;
#'   the first color is for the bars and the second is for the normal curve.
#' @param writeout A logical indicating whether the graph should be written to
#'   your working directory as your indicated \code{imageType}.  If \code{TRUE},
#'   the file name will begin \code{pershist} and will include an index (if more
#'   than one graph is produced) and the \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#' @param ... Additional arguments to be passed to \code{hist} function.
#'
#' @return Plots one histogram for each dimension.
#'
#' @export

pers.hist <- function(results, dim = NULL, palette = "BASS",
                      writeout = FALSE, imageType = "pdf", fileSuffix = NULL,
                      ...) {
  origPar = par(no.readonly = TRUE) # to reset graphical parameters after
  if (palette == "BASS") {
    color <- c("#80b1d3", "gray")
  } else if (palette %in% row.names(brewer.pal.info)) {
    color <- RColorBrewer::brewer.pal(3, palette)
  } else {
    if (all(areColors(palette)) & length(palette)==2) {
      color <- palette
    } else {
      stop('Invalid color palette specified. Use "BASS", an RColorBrewer palette, or a vector containing 2 colors.')
    }
  }
  layout(matrix(1))
  par(mai=c(1.36,1.093333,1.093333,0.56),
      mar=c(5.1,4.1,4.1,2.1))

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else {
    D <- dim
  }

  for (d in D) {
    h <- hist(results$persPars[,d], plot = FALSE)
    x <- seq(min(results$persPars[,d]), max(results$persPars[,d]), length = 200)
    y <- dnorm(x, mean = results$popDist$mean[d],
               sd = sqrt(results$popDist$var.cov[d,d])) * diff(h$mids[1:2]) *
                                                          nrow(results$persPars)

    if (writeout) {
      if (D==1) {
        dd = NULL
      } else {
        dd = d
      }
      graphout = paste0("pershist", dd, fileSuffix, ".", imageType)
      eval(parse(text=paste0(imageType,"('",graphout,"')")))
    }

    hist(results$persPars[,d], main = results$consInfo$short.name[d],
         xlab = "Logits", col = color[1], freq = TRUE, axes = FALSE,
         ylim = c(0, max(max(y), max(h$counts))))
    axis(1, at = c(seq(0, min(0, floor(min(results$persPars[,d]))), by = -2),
                   seq(2, max(2, ceiling(max(results$persPars[,d]))), by = 2)))
    axis(1, at = c(seq(-1, min(-1,floor(min(results$persPars[,d]))), by = -2),
                   seq(1, max(1, ceiling(max(results$persPars[,d]))), by = 2)),
         labels = FALSE)
    axis(2, las = 1)
    Hmisc::minor.tick(nx = 1, ny = 2, tick.ratio = 1)
    lines(x, y, col = color[2], lty = 2, lwd = 2) # add normal curve

    if (writeout) {
      dev.off()
    }

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
#' @param itemOrder Can be "item", "construct", or a numeric vector.  If "item",
#'   items will be ordered as in \code{itemInfo}.  If "construct", items will
#'   be grouped by the levels of the construct.
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

wm <- function(results, dim = NULL, itemOrder = "item", palette = "BASS",
               writeout = FALSE, imageType = "pdf", fileSuffix = NULL) {
  origPar = par(no.readonly = TRUE) # to reset graphical parameters after

  # if itemOrder=="construct", then the WMs MUST be consecutive (1 for each dim)
  if (itemOrder == "construct") {
    consecutive = TRUE
  } else {
    consecutive = results$estSummary$consecutive
  }

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else {
    D <- dim
  }

  if (is.numeric(itemOrder)) {
    itemInfo <- results$itemInfo[itemOrder,]
    itemThres <- results$itemThres[itemOrder,]
  } else {
    if (itemOrder != "item" & itemOrder != "construct") {
      stop('Specify valid itemOrder.  Choose "item", "construct", or provide a numeric vector.')
    }
    itemInfo <- results$itemInfo
    itemThres <- results$itemThres
  }

  if (consecutive) {
    thresList <- list()
      for (i in 1:length(D)) {
        # get the indices for the items on each construct
        thresList[[i]] <- which(itemInfo$cons.ID == results$consInfo$cons.ID[D[i]])
      }
  } else {
    thresList <- list(1:results$estSummary$I)
  }

  if (itemOrder == "construct") {
    # this option means that people are using all of the items, and they won't be in a different order!
    itemXcat <- as.matrix(itemInfo[,6:ncol(itemInfo)])

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
    toGraph[itemXcat] <- c(t(itemThres))[!is.na(c(t(itemThres)))]
  } else {
    toGraph <- itemThres
  }

  if (palette == "BASS") {
    color <- c("black", "#80b1d3")
  } else if (palette %in% row.names(brewer.pal.info)) {
    color <- RColorBrewer::brewer.pal(3, palette)
  } else {
    stop('Please choose an allowed color palette - "BASS" or any RColorBrewer palette.')
  }

  for (i in 1:length(thresList)) {

    if (consecutive) {
      thetas <- results$persPars[,D[i]]
      cons <- consLabel <- paste0(results$consInfo$short.name[D[i]], " ")
      thresLabel <- matrix(rep(itemInfo$item.name, each = nrow(toGraph)),
                           nrow = nrow(toGraph), ncol = ncol(toGraph))
    } else {
      thetas <- results$persPars
      cons <- thresLabel <- NULL
      consLabel <- colnames(results$persPars)
    }

    if (itemOrder == "construct") {
      thres <- toGraph[,thresList[[i]]]
      xLabel <- results$consInfo[D[i], 5:ncol(results$consInfo)]
      thresPos <- c(2,4)
    } else {
      thres <- toGraph[thresList[[i]],]
      xLabel <- itemInfo$item.name[thresList[[i]]]
      thresPos <- 2
    }

    if (writeout) {
      if (itemOrder == "item") {
        fileName <- "-itemorder"
        imgWidth <- min(14, length(thresList[[i]]))
      } else if (itemOrder == "construct") {
        fileName <- "-constructorder"
        imgWidth <- max(6, 2*(ncol(results$consInfo) - 5))
      } else {
        fileName <- "-customorder"
        imgWidth <- min(14, length(thresList[[i]]))
      }

      if (length(thresList)==1) {
        index <- NULL
      } else {
        index <- D[i]
      }

      eval(parse(text = paste0(imageType, "('WM", fileName, index, fileSuffix,
                               ".", imageType, "', width = ", imgWidth,
                               " , height = 7)")))
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


################################################################################
#' Plots the cumulative probability curves from \code{craschR} output.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param itemOrder A numeric vector that specifies which items from the output
#'   should be graphed.  If \code{NULL}, all items will be graphed.
#' @param palette A character string indicating the color scheme.  Can be
#'   "BASS", any RColorBrewer palette, or a vector containing 3 colors.
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
    } else {
      stop('palette must be "BASS", "grey", an RColorBrewer palette, or a character with 3 valid color specifications.')
    }

    if(writeout) {
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
