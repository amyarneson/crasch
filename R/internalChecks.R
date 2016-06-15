###############################################################################
# check for correct classes/values of inputs
  checkInput <- function(scores, itemInfo, consInfo, varsInfo,
                         estPackage, retainOrig,
                         missingAs0, longFormat,
                         persMethod, consecutive,
                         writeout, imageType) {
    stopifnot(is.data.frame(scores) | is.character(scores),
              is.null(itemInfo) | is.data.frame(itemInfo) | is.character(itemInfo),
              is.null(consInfo) | is.data.frame(consInfo) | is.character(consInfo),
              is.null(varsInfo) | is.data.frame(varsInfo) | is.character(varsInfo),
              estPackage %in% c("mirt","TAM"),
              is.logical(retainOrig),
              is.logical(missingAs0),
              is.logical(longFormat),
              persMethod %in% c("EAP","MLE","WLE","MAP"),
              imageType %in% c("pdf","svg","jpeg","bmp","tiff","png") )

    if ( longFormat ) {
      # check for correct column names

      # check for item.ID & cons.ID mismatches/missings

      # check that item names match in scores and itemInfo files

    } else {
      # check that item names match in scores and itemInfo files
      problem = which(!(itemInfo$item.name %in% colnames(scores)))
      if ( length(problem) > 0 ) {
        stop(paste("Item name mismatch(es) at:",
                   paste(problem,collapse=" ,")),"\n",
             "Check the item names in scores and itemInfo objects.\n\n")
      }
    }

    # check for correct column names in information objects
    if ( sum(!colnames(itemInfo)[1:5] ==
               c("item.ID","item.name","cons.ID","item.type","fixed")) > 0 ) {
      stop("Check column names in itemInfo object. They must exactly match specified format.\n\n")
    }
    if ( sum(!colnames(consInfo)[1:3] ==
               c("cons.ID","long.name","short.name")) > 0 ) {
      stop("Check column names in consInfo object. They must exactly match specified format.\n\n")
    }

    # check that all item names (within a construct) are unique
    # note that it is possible for an item name to be 'repeated' if it is scored on more than one dimension


    # check that all item IDs are unique

    # check that all person IDs are unique

    # for true multidimensional analysis, TAM cannot produce anything but EAPs
    if ( estPackage == "TAM" && consecutive == FALSE && nrow(consInfo > 1)  && persMethod != "EAP" ) {
      warning("For true multidimensional analysis using TAM, only EAPs are available. persMethod has defaulted to 'EAP'.")
      persMethod <- "EAP"
    }
  }

###############################################################################
# check for continuity between item/construct/response objects
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
# check for items with no variability
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