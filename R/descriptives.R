###############################################################################
#' Create a stacked bar graph showing score level frequencies per item.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim A numeric vector that specifies for which dimension(s) to create
#'   graphic/tables.  If \code{NULL}, output and graphics for each dimension
#'   will be produced.
#' @param freqs A logical indicated whether frequencies or raw counts should be
#'   graphed.
#' @param palette A character string indicating the color scheme for the graph.
#'   Can be "BASS", "grey", or any RColorBrewer palette string.
#' @param writeout A logical indicating whether the graphics/tables should be
#'   written to to your working directory as your specified \code{imageType} and
#'   CSVs.  If \code{TRUE}, the file name will begin \code{itemscores} and will
#'   include an index (if more than one graph is produced) and the
#'   \code{fileSuffix} if provided.
#' @param imageType A character string indicating the format for graphics (if
#'   \code{writeout = TRUE}). Supported types:
#'   \code{c("pdf","svg","jpeg","bmp","tiff","png")}.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#' @param ... Additional arguments to be passed to \code{barplot} function.
#'
#' @return Creates a stacked bar plot and returns a list with the following
#'   entries:
#'   \item{counts}{A matrix with the raw counts of scores given to each
#'   construct level on each item.  \code{NA}s indicate that the score level
#'   was not possible while 0s indicate that the score level was on the outcome
#'   space yet no one scored at that level.}
#'   \item{proportions}{A matrix with proportions of scores given to each
#'   construct level on each item.}
#'   Note that there will be one matrix per dimension.  If a multi-dimensional
#'   analysis was run
#'
#' @export

item.scores <- function(results, dim = NULL, freqs = TRUE, palette = "BASS",
                        writeout = FALSE, imageType = "pdf", fileSuffix = NULL,
                        ...) {

  checkResults(results)
  checkDim(dim, results$consInfo)
  checkWrite(writeout, fileSuffix)
  checkImageType(imageType)

  origPar = par(no.readonly = TRUE) # so graphical parameters can be reset after
  par(mai=c(1.36,1.093333,1.093333,0.56),
      mar=c(5.1,4.1,4.1,2.1),
      xpd=FALSE)
  tables <- list()

  if (is.null(dim)) {
    D <- 1:results$estSummary$D
  } else {
    D <- dim
  }

  for (d in D) {
    cons <- results$consInfo[d,]
    I <- sum(results$itemInfo$cons.ID == cons$cons.ID)
    scores <- reshape(data.frame(as.matrix(results$scoresOrig[,
              which(results$itemInfo$cons.ID == results$consInfo$cons.ID[d])])),
                      varying = list(1:I), idvar = "person",
                      direction = "long", timevar = "item", v.names = "score")
    # replace score values with score names
    K <- ncol(cons) - 3

    if ( freqs ) {
      tograph <- table(scores$score, scores$item, useNA="no")
    } else {
      tograph <- prop.table(table(scores$score, scores$item, useNA="no"),
                            margin = 2)
    }
    dimnames(tograph) <- list(cons[4:ncol(cons)],
                              results$itemInfo$item.name[
                which(results$itemInfo$cons.ID == results$consInfo$cons.ID[d])])

    if ( palette %in% row.names(brewer.pal.info) ) {
      # check that there are enough colors in palette
      if ( K <= RColorBrewer::brewer.pal.info$maxcolors[
        which(row.names(RColorBrewer::brewer.pal.info) == palette)] ) {
        color = RColorBrewer::brewer.pal(K, palette)
      } else {
        stop('Too many levels for chosen palette. Choose a different palette.')
      }
    } else if ( palette == "grey" | palette == "gray" ) {
      color = gray(level = rev(1:nrow(tograph)/nrow(tograph)))
    } else if ( palette == "BASS" ) {
      color = rainbow(n = nrow(tograph), start = 4/6, end = 4/6+.001,
                      alpha = seq(.3,.9,length.out = nrow(tograph)))
    } else {
      stop('Invalid palette argument.')
    }

    table.temp <- list(counts = t(tograph),
                       proportions = t(prop.table(tograph, margin = 2)))
    # replace 0 counts with NAs if level was not possible for that item
    table.temp[[1]][!results$itemInfo[results$itemInfo$cons.ID == cons$cons.ID,
                                      6:ncol(results$itemInfo)]] = NA
    table.temp[[2]][!results$itemInfo[results$itemInfo$cons.ID == cons$cons.ID,
                                      6:ncol(results$itemInfo)]] = NA

    tables[[which(D==d)]] <- table.temp

    if (writeout) {
      if (D==1) {
        dd = NULL
      } else {
        dd = d
      }
      graphout = paste0("itemscores", dd, fileSuffix, ".", imageType)
      tableout = paste0("itemscores", dd, fileSuffix, ".csv")
      write.table(table.temp[[1]], tableout, sep = ",", col.names = NA)
                  blankrow = c("Proportions", rep("", ncol(table.temp[[1]])-1))
                  write.table("Proportions", tableout, sep = ",", append = TRUE,
                              col.names=FALSE)
                  write.table(table.temp[[2]], tableout, sep = ",",
                              append = TRUE, col.names = FALSE)

      eval(parse(text=paste0(imageType, "('", graphout, "')")))
    }
    # plot tograph + extra empty row @ top so there is room for legend!
    barplot(cbind(tograph[,seq(ncol(tograph), 1, -1)], rep(NA, nrow(tograph))),
            col = color, horiz = TRUE, las = 1,
            main = paste0(cons$short.name,", Scores by Item"))
    legend(x = "top", legend = row.names(tograph), pch = 22,
           col = "black", pt.bg = color, horiz = TRUE, pt.cex = 2, bty = "n",
           text.width = max(colSums(tograph))/(nrow(tograph)+2))

    if (writeout) {
      dev.off()
    }

  }

  par(origPar)

  names(tables) = results$consInfo$short.name[D]
  return(tables)
}
