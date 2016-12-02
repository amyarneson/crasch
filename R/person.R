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
  checkResults(results)
  checkWrite(writeout, fileSuffix)
  checkImageType(imageType)
  checkDim(dim, results$consInfo)

  origPar = par(no.readonly = TRUE) # to reset graphical parameters after
  if (identical(palette, "BASS")) {
    color <- c(bar = "#80b1d3", curve = "gray")
  } else if (length(palette) == 1) {
    if (palette %in% row.names(brewer.pal.info)) {
      color <- RColorBrewer::brewer.pal(3, palette)
    } else {
      stop('Invalid palette argument.')
    }
  } else if (all(areColors(palette)) & length(palette)==2) {
    color <- palette
  } else {
    stop('Invalid palette argument.')
  }

  layout(matrix(1))
  par(mai=c(1.36, 1.093333, 1.093333, 0.56),
      mar=c(5.1, 4.1, 4.1, 2.1))

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
#' Plots a KIDMAP for a given person and construct.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param personID A character or numeric vector indicating the index ID of the
#'   respondent for the plot.  (Do not specify a row number, use the assigned
#'   ID.)
#' @param dim A numeric vector that specifies for which dimension(s) to create
#'   graphic/tables.  If \code{NULL}, output and graphics for each dimension
#'   will be produced.
#' @param probBounds A vector containing the range of probabilities which are
#'   "unsurprising."
#' @param palette The color scheme for the plot. Can be "BASS or any
#'   RColorBrewer palette name (the 3-color palette will be used). Can also
#'   specify a vector with 3 colors in any R-supported form; the first is the
#'   color of the "expected" area, the second the "surprise" area, and the third
#'   the point color.
#' @param writeout A logical indicating whether the graph should be written to
#'   your working directory as your indicated \code{imageType}.  If \code{TRUE},
#'   the file name will begin \code{KIDMAP} and will include an index for the
#'   selected respondent, and the \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'
#' @return KIDMAP plot for specified person.
#'
#' @export

KIDMAP <- function(results, personID, dim = NULL, probBounds = c(.25, .75),
                   palette = "BASS", writeout = FALSE, imageType = "pdf",
                   fileSuffix = NULL) {
  checkResults(results)
  checkWrite(writeout, fileSuffix)
  checkImageType(imageType)
  checkDim(dim, results$consInfo)
  if (!(all(as.character(personID) %in% row.names(results$scoresOrig)) &
       (is.character(personID) | is.numeric(personID)))) {
    stop('Invalid personID argument.')
  }
  if (!(is.numeric(probBounds) &
        length(probBounds) == 2 &
        all(probBounds <= 1 & probBounds >= 0) &
        (probBounds[1] < probBounds[2]))) {
    stop('Invalid probBounds argument.')
  }

  origPar = par(no.readonly = TRUE) # to reset graphical parameters after

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else if (is.numeric(dim)) {
    D <- dim
  } else {
    stop('Invalid dim argument.')
  }

  if (identical(palette, "BASS")) {
    color = c(expected = rgb(red = 128, green = 177, blue = 211, alpha = 127.5,
                             maxColorValue = 255),
              surprise = rgb(red = 255, green = 229, blue = 0, alpha = 127.5,
                             maxColorValue = 255),
              points = "#80b1d3")
  } else if (length(palette) == 1) {
    if (palette %in% row.names(brewer.pal.info)) {
      color <- RColorBrewer::brewer.pal(3, palette)
    } else {
      stop('Invalid palette argument.')
    }
  } else if (all(areColors(palette)) & length(palette) == 3) {
    color <- palette
  } else {
    stop('Invalid palette argument.')
  }

  rowIndex <- which(row.names(results$scoresRecoded) %in% as.character(personID))

  for (d in D) {
    inclItem <- which(results$itemInfo$cons.ID == results$consInfo$cons.ID[d])
    thres <- results$itemThres[inclItem,]

    for (i in rowIndex) {
      respVector <- as.numeric(results$scoresRecoded[i,])
      toPlot <- data.frame(x = rep(1, prod(dim(thres))),
                           thres = c(t(thres)),
                           step = rep(1:ncol(thres), length(inclItem)),
                           score = rep(respVector[inclItem], each = ncol(thres)) )
      row.names(toPlot) <- paste(rep(row.names(thres), each = ncol(thres)),
                               rep(1:ncol(thres), length(inclItem)), sep = "_")
      toPlot = toPlot[complete.cases(toPlot),] # removes steps that don't exist
                                               # AND skipped items
      toPlot$x[toPlot$step <= toPlot$score] = -1
        # all "reached" thresholds have an x-coord of -1, all others 1

      # location of "surprise" lines
        upBd = results$persPars[i, d] - log(probBounds[1] / (1 - probBounds[1]))
        loBd = results$persPars[i, d] - log(probBounds[2] / (1 - probBounds[2]))

      if (writeout) {
        eval(parse(text = paste0(imageType, "('KIDMAP-", i, "_",
                                 results$consInfo$short.name[d], fileSuffix,
                                 ".", imageType, "')")))
      }

      layout(matrix(1,  nrow = 1))
      par(mai = c(1.02, 0.82, 0.82, 0.42), mar = c(5.1, 4.1, 5.3, 2.1))

      plot(1, type = "n", xlim = c(-1.5, 1.5),
           ylim = c(min(toPlot$thres, loBd) - .2, max(toPlot$thres, upBd) + .2),
           axes = FALSE, xlab = "", ylab = "Logits", main = "KIDMAP")
      mtext(paste0("Person: ", row.names(results$scoresRecoded)[i],
                   "\nEst Theta: ", round(results$persPars[i, d], 2)),
            side = 3, line = 0, cex = .8)
      mtext(paste0("Raw: ", round(results$persRaw[i, d], 2), "/",
                   round(results$persMax[i, d], 2),
                   "\nOutfit: ", round(results$persFit[[d]]$outfit[i], 2),
                   " (t=",round(results$persFit[[d]]$outfit_t[i], 2), ")",
                   "\nInfit: ", round(results$persFit[[d]]$infit[i], 2),
                   " (t=",round(results$persFit[[d]]$infit_t[i], 2), ")"),
            side = 1, line = 2, cex = .7)
      mtext("Reached", adj = 0)
      mtext("Not Reached", adj = 1)

      rect(xleft = -1.5, xright = 1.5, ybottom = loBd, ytop = upBd,
           col = color[1], border = NA)
      rect(xleft = -1.5, xright = 0, ybottom = upBd, ytop = 100,
           col = color[2], border = NA)
      rect(xleft = 0, xright = 1.5, ybottom = -100, ytop = loBd,
           col = color[2], border = NA)

      abline(v = 0)
      abline(h = results$persPars[i, d])

      axis(side = 2, at = c(seq(0, floor(min(toPlot$thres, loBd) - .2), by = -1),
                            seq(0, ceiling(max(toPlot$thres, upBd) + .2), by = 1)),
           las = 1)
      axis(side = 4, at = c(seq(0, floor(min(toPlot$thres, loBd) - .2), by = -1),
                            seq(0, ceiling(max(toPlot$thres, upBd) + .2), by = 1)),
           las = 1)

      points(x = toPlot$x, y = toPlot$thres, pch = 21, bg = color[3])

      if (sum(toPlot$x == -1) == 1) {
        text(x = -1, y = toPlot$thres[toPlot$x == -1],
             row.names(toPlot[toPlot$x == -1,]), pos = 4, cex = .5)
      } else if (sum(toPlot$x == -1) > 1) {
        text(x = -1, y = sort(toPlot$thres[toPlot$x == -1]),
             labels = row.names(toPlot)[toPlot$x == -1][order(toPlot$thres[toPlot$x == -1])],
             pos = c(2, 4), cex = .5)
      }

      if (sum(toPlot$x == 1) == 1) {
        text(x = 1, y = toPlot$thres[toPlot$x == 1],
             row.names(toPlot[toPlot$x == 1,]), pos = 2, cex = .5)
      } else if (sum(toPlot$x == 1) > 1) {
        text(x = 1, y = sort(toPlot$thres[toPlot$x == 1]),
             labels = row.names(toPlot)[toPlot$x == 1][order(toPlot$thres[toPlot$x == 1])],
             pos = c(2, 4), cex = .5)
      }

      if (writeout) {
        dev.off()
      }
    }
  }

  par(origPar)
}
