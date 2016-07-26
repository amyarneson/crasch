###############################################################################
# check for correct classes/values of inputs for craschR function
  checkInput <- function(scores, itemInfo, consInfo, varsInfo,
                         estPackage, retainOrig,
                         missingAs0, longFormat,
                         persMethod, consecutive,
                         writeout) {
    stopifnot(is.data.frame(scores) | is.character(scores))
    stopifnot(is.null(itemInfo) | is.data.frame(itemInfo) | is.character(itemInfo))
    stopifnot(is.null(consInfo) | is.data.frame(consInfo) | is.character(consInfo))
    stopifnot(is.null(varsInfo) | is.data.frame(varsInfo) | is.character(varsInfo))
    stopifnot(estPackage %in% c("mirt","TAM"))
    stopifnot(is.logical(retainOrig))
    stopifnot(is.logical(missingAs0))
    stopifnot(is.logical(longFormat))
    stopifnot(persMethod %in% c("EAP","MLE","WLE","MAP"))

    if ( longFormat ) {
      # check for correct column names

      # check for item.ID & cons.ID mismatches/missings

      # check that all item.IDs in itemInfo show up at least once in scores

      # check that item names match in scores and itemInfo files

      # check that all person IDs are unique (may have to do this after reshape,
      #                                       check that the number of columns
      #                                       in wide matches the number of
      #                                       items in itemInfo)

    } else {
      # check that item names match in scores and itemInfo files
      problem = which(itemInfo$item.name != colnames(scores))
      if ( length(problem) > 0 ) {
        stop(paste("Item name mismatch(es) at:",
                   paste(problem,collapse=" ,")),"\n",
             "Check the item names in scores and itemInfo objects.\n\n")
      }
      # check that all person IDs are unique
      if (length(unique(row.names(scores))) != nrow(scores)) {
        stop('Some respondent ID is repeated. All IDs must be unique.')
      }
      # check that there are the same number of items in scores and itemInfo
      if (ncol(scores) != nrow(itemInfo)) {
        stop('Differing number of items in scores and itemInfo.')
      }
    }

    # check for correct column names in information objects
    Ncat <- ncol(consInfo) - 3
    if ( sum(!colnames(itemInfo) ==
               c("item.ID","item.name","cons.ID","item.type","fixed",
                 paste0("cat",1:Ncat))) > 0 ) {
      stop("Check column names in itemInfo object. They must exactly match specified format.\n\n")
    }
    if ( sum(!colnames(consInfo) ==
               c("cons.ID","long.name","short.name",
                 paste0("cat",1:Ncat))) > 0 ) {
      stop("Check column names in consInfo object. They must exactly match specified format.\n\n")
    }

    # check that all item names (within a construct) are unique
    # note that it is possible for an item name to be 'repeated' if it is scored on more than one dimension


    # check that all item IDs are unique
    if (length(unique(itemInfo$item.ID)) != nrow(itemInfo)) {
      stop('Some item.ID is repeated. All item.IDs must be unique.')
    }

    # for true multidimensional analysis, TAM cannot produce anything but EAPs
    if ( estPackage == "TAM" && consecutive == FALSE && nrow(consInfo > 1)  && persMethod != "EAP" ) {
      warning("For true multidimensional analysis using TAM, only EAPs are available. persMethod has defaulted to 'EAP'.")
      persMethod <- "EAP"
    }

    # check for mismatches of function arguments
    if ( nrow(consInfo) == 1 & consecutive ) {
      stop('Unidimensional analysis cannot be consecutive. Use consecutive=FALSE.')
    }
  }

################################################################################
# check for continuity between item/construct/response objects for craschR
# note that this is run after the 'wide' object is created
  checkObjs <- function(wide, itemInfo, consInfo, varsInfo,
                        estPackage, retainOrig,
                        missingAs0, longFormat,
                        persMethod, consecutive,
                        writeout, imageType) {

    # check for item scores deemed 'impossible'
    problem = vapply(1:ncol(wide), function(x) {
      sum(!as.integer(names(table(wide[,x]))) %in%
        which(as.logical(itemInfo[x,6:ncol(itemInfo)])))
    }, as.integer(NA))
    problem = colnames(wide)[which(problem>0)]
    if ( length(problem) > 0 ) {
      stop(paste("Items with non-allowable entries (scores):",
                  paste(problem,collapse=", ")),"\n",
            "Check the scores on these items against the itemInfo object.\n\n")
    }

    # check that only integers (and NAs) are present in scores
    problem = colnames(wide)[sapply(wide,class) != "integer"]
    if ( length(problem) > 0 ) {
      stop(paste("Items with non-integer entries (scores):",
                 paste(problem,collapse=", ")),"\n",
           "All scores must be coded as an integer corresponding to a level of your construct.\n\n")
    }

    # check for same number of items in item & response files
    if ( nrow(itemInfo) != ncol(wide) ) {
      stop('Different number of items in response data and item information input files.\n\n')
    }

    # check that item names in wide are in the same order as itemInfo

  }

################################################################################
# check for items with no variability for craschR
  noVar <- function(data) {
    as.numeric(c(which(apply(data,2,function(x) length(unique(x))) == 1)))
  }

################################################################################
# check that inputs are colors
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

################################################################################
# check that results is correct format

  checkResults <- function(results) {
    if (!(is.list(results) &
         (length(results) == 19 | length(results) == 20) &
         is.matrix(results$itemPars) &
         is.matrix(results$itemSEs) &
         is.matrix(results$itemThres) &
         is.data.frame(results$itemFit) &
         is.data.frame(results$persPars) &
         is.data.frame(results$persRaw) &
         is.data.frame(results$persMax) &
         is.list(results$persFit) &
         is.list(results$estSummary) &
         is.list(results$classicalStats) &
         is.list(results$empties) &
         is.data.frame(results$scoresOrig) &
         is.data.frame(results$scoresRecoded) &
         is.data.frame(results$itemInfo) &
         is.data.frame(results$consInfo) &
         (is.data.frame(results$varsInfo) | is.null(results$varsInfo))) ) {
      stop('Invalid results argument. Provide craschR output.')
    }

    #stopifnot(is.data.frame(results$persSEs)) # not used in any functions
    #stopifnot(is.list(results$popDist)) # not used in any functions as of now
    #stopifnot(is.numeric(results$sepRel)) # not used in any functions as of now

  }

################################################################################
# check that writeout, fileSuffix are correct format

  checkWrite <- function(writeout, fileSuffix) {
    stopifnot(is.logical(writeout),
              is.character(fileSuffix) | is.null(fileSuffix))
    if (writeout == FALSE) {
      message('No output was written to file. If you wish to write to file, use writeout=TRUE.')
    }
  }

################################################################################
# check that imageType is correct format

  checkImageType <- function(imageType) {
    if (!imageType %in% c("pdf","svg","jpeg","bmp","tiff","png")) {
      stop('Invalid imageType argument. Choose from "pdf", "svg", "jpeg", "bmp", "tiff", and "png".')
    }
  }

################################################################################
# check that dim is correct format

  checkDim <- function(dim, consInfo) {
    if (!(is.numeric(dim) | is.null(dim))) {
      stop('Invalid dim argument.')
    }
    if (is.numeric(dim)) {
      if (!(all(dim <= nrow(consInfo)) &
            all(dim %% 1 == 0) &
            all(dim > 0))) {
        stop('Invalid dim argument.')
      }
    }
  }

################################################################################
# check that itemOrder is correct format

  checkItemOrder <- function(itemOrder, itemInfo) {
    if (!(is.numeric(itemOrder) | is.null(itemOrder))) {
      stop('Invalid itemOrder argument.')
    }
    if (is.numeric(itemOrder)) {
      if (!(all(itemOrder <= nrow(itemInfo)) &
            all(itemOrder %% 1 == 0) &
            all(itemOrder > 0))) {
        stop('Invalid itemOrder argument.')
      }
    }
  }
