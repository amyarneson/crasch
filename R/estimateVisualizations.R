###############################################################################
#' Create a histogram of person estimates.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim Specify which dimension(s) to create graphic/tables for.  If
#'   \code{NULL}, output and graphics for each dimension will be produced.
#' @param col
#' @param writeout A logical indicated whether the estimate objects should be
#'   written to your working directory as CSVs.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param filePrefix A character string that will be affixed to the beginning
#'   of each file (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#' @param ... Additional arguments to be passed to \code{hist} function.
#'
#' @return Plots a histogram
#'
#' @export

pers.hist <- function(results, dim = NULL, palette = "BASS",
                      writeout = FALSE, imageType = "pdf", filePrefix = NULL, 
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
        dd = paste0(results$consInfo$short.name[d], "-")
      }
      graphout = paste0(filePrefix, dd, "itemscores.", imageType)
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