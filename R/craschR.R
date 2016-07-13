#' Produce item and person estimates and fit statistics.
#'
#' @param scores A data frame or path to CSV file in crasch format with the
#'   scoring information. The scores are be coded as integers in terms of the
#'   construct levels. Missing responses are coded as \code{NA}.
#' @param itemInfo A data frame or path to CSV file in crasch format with
#'   information on each item. There must be \code{I} rows and as many columns
#'   as specified in the crasch format documentation. No factor variables are
#'   allowed.
#' @param consInfo A data frame or path to CSV file containing labeling
#'   information about the construct(s) in crasch format. Contains 1 row per
#'   construct.
#' @param varsInfo A data frame containing demographic variable information for
#'   respondents. One respondent per row, IDs must match those of
#'   \code{scores} data frame. Numeric, character, and factor variables are
#'   allowed.
#' @param estPackage A string that identifies which IRT package will be used to
#'   conduct estimation of item (and possibly person) parameters. See details.
#' @param retainOrig A logical indicating whether one element of the output
#'   object should include the original result object from \code{mirt} or
#'   \code{TAM}.
#' @param missingAs0 A logical indicating whether \code{NA}s should be recoded
#'   to the lowest level (available on a given item) of the construct.
#'   If \code{FALSE}, missing data will remain as missing data.
#' @param longFormat A logical indicating whether the scores object is in long
#'   format.  If \code{TRUE}, reshaping to wide format will occur internally.
#' @param consecutive A logical indicating whether to perform a 'consecutive'
#'   analysis in which an independent analysis is conducted for each dimension.
#'   If \code{TRUE}, the correlation between any two dimensions will be
#'   constrained to be 0. If \code{FALSE}, multidimensional analysis that
#'   allows non-zero correlations among dimensions will be conducted.
#' @param writeout A logical indicated whether the estimate objects should be
#'   written to your working directory as CSVs.  See Return below for the names
#'   of each file produced.  Each will also include any \code{fileSuffix} that
#'   you specify.
#' @param fileSuffix A character string that will be affixed to the end of each
#'   file name (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#' @param ... Further arguments to be passed to \code{mirt()},
#'   \code{tam.mml()}, \code{pcm.fit()} or other functions. Besides convergence
#'   criterion or other control arguments, this is not recommended.
#'
#' @return A list with the following entries
#'   \item{itemPars}{A matrix with the item parameter estimates.  If
#'   \code{estPackage = "mirt"}, this matrix will be...
#'   \code{estPackage = "TAM"}, this matrix will be in the ConQuest
#'   parameterization with an overall item `delta' and step `tau's.}
#'   (\code{itemGrid.csv})
#'   \item{itemSEs}{A matrix with the standard error estimates corresponding to
#'   the estimates given in \code{itemPars}.} (\code{itemGrid.csv})
#'   \item{itemThres}{A matrix giving the Thurstonian threshold estimates.}
#'   (\code{itemGrid.csv})
#'   \item{itemFit}{A data frame providing the fit statistics for items and
#'   steps.} (\code{itemFit.csv})
#'   \item{persPars}{A data frame containing estimates for person locations for
#'   each dimension.} (\code{persGrid.csv})
#'   \item{persSEs}{A data frame with standard errors for person estimates.}
#'   (\code{persGrid.csv})
#'   \item{persFit}{If non-consecutive (including unidimensional) analysis is
#'   performed, this will be a data frame with person fit statistics.  If
#'   `consecutive' multidimensional analysis is performed (independent analysis
#'   of each dimension), this will be a list with fit statistics for each
#'   dimension.  True multidimensional fit statistics are not available if
#'   \code{TAM} is used.  See details for how these are produced.}
#'   (\code{persFit.csv})
#'   \item{popDist}{A list containing a vector of estimated population means
#'   for each dimension (often these are constrained to be 0) and a variance-
#'   covariance matrix.  Variances appear on the diagonal, covariances
#'   elsewhere.  `Consecutive' analyses fix all covariances to 0.}
#'   (\code{popDist.csv})
#'   \item{sepRel}{A vector containing the separation reliabilities for each
#'   dimension.} (\code{classicalStats.csv})
#'   \item{estSummary}{A list containing information about the estimation
#'   settings and model fit.  Use the \code{names()} function to see what is
#'   provided here.} (\code{estSummary.csv})
#'   \item{classicalStats}{A list containing information about each dimension.
#'   Notably, Cronbach's Alpha and person separation reliabilities are
#'   provided.  Statistics are calculated by recoding all missings to 0 as well
#'   as by only including complete cases.} (\code{classicalStats.csv})
#'   \item{empties} A list of vectors (1 per item) of the categories (if any)
#'   that were deemed possible, but no one scored into them.
#'   \item{estResults}{If \code{retainOrig = TRUE}, this will contain the FULL
#'   output provided by the estimation package.  See the documentation for that
#'   package for full details.  Otherwise, this will be NULL.}
#'   \item{scoresOrig}{}
#'   \item{scoresRecoded}{Wide format score data, recoded to nonnegative
#'   successive integers.}
#'   \item{itemInfo}{}
#'   \item{consInfo}{}
#'   \item{varsInfo}{}
#'
#' @export
#add references -- Constructing Measures, Rasch, PCM
#add details -- specifically about where person parameters come from (TAM or sirt)
#            -- where fit statistics come from
#            -- where item parameters come from

craschR <- function(scores, itemInfo = NULL, consInfo = NULL, varsInfo = NULL,
                    estPackage="mirt", retainOrig = FALSE,
                    missingAs0 = FALSE, longFormat = FALSE,
                    persMethod = "EAP", consecutive = FALSE,
                    writeout = TRUE, imageType = "pdf", fileSuffix = NULL, ...) {

  # if files are supplied for scores,itemInfo,consInfo,varsInfo:
  if ( is.character(scores) ) {
    if ( !grepl(".csv$",scores) ) { stop("scores file must end with '.csv'") }
    scores <- data.frame(read.csv(scores,stringsAsFactors=FALSE,row.names=1))
  }
  if ( is.character(itemInfo) ) {
    if ( !grepl(".csv$",itemInfo) ) { stop("itemInfo file must end with '.csv'") }
    itemInfo <- data.frame(read.csv(itemInfo,stringsAsFactors=FALSE))
  }
  if ( is.character(consInfo) ) {
    if ( !grepl(".csv$",consInfo) ) { stop("consInfo file must end with '.csv'") }
    consInfo <- data.frame(read.csv(consInfo,stringsAsFactors=FALSE))
  }
  if ( is.character(varsInfo) ) {
    if ( !grepl(".csv$",varsInfo) ) { stop("varsInfo file must end with '.csv'") }
    varsInfo <- data.frame(read.csv(varsInfo))
  }

  if ( estPackage == "TAM"  ) { requireNamespace("TAM", quietly = TRUE) }

  startTime = date()

  # addition of any input/argument also requires an update of the functions in
  #   internalChecks.R
  checkInput(scores, itemInfo, consInfo, varsInfo, estPackage, retainOrig,
             missingAs0, longFormat, persMethod, consecutive, writeout,
             imageType)

  # reshape to wide if necessary
  if ( longFormat ) {
    wide <- reshape(scores[c("resp.ID","item.ID","cat")],timevar="item.ID",
                    v.names="cat",idvar="resp.ID",direction="wide")
    row.names(wide) = wide$resp.ID
    wide$resp.ID = NULL
    colnames(wide) = itemInfo$item.name[itemInfo$item.ID==as.numeric(gsub("cat.","",colnames(wide)))]
  } else {
    wide <- scores
  }

  # create standard consInfo if not provided
  # this will default to UNIdimensional
  if ( is.null(consInfo) ) {
    maxScore <- max(wide,na.rm=TRUE)
    consInfo <- data.frame(matrix(paste("Level",1:maxScore),
                                  nrow=1))
    colnames(consInfo) = paste0("cat",1:maxScore)
    consInfo = data.frame(cons.ID = 11111,
                          long.name = "Construct",
                          short.name = "cons",
                          consInfo,
                          stringsAsFactors = FALSE)
  }

  # create standard itemInfo if not provided
  # this will place all items on the first dimension of consInfo
  if ( is.null(itemInfo) ) {
    maxScore <- max(wide,na.rm=TRUE)
    itemInfo <- data.frame(matrix(rep(TRUE,maxScore * I),
                                  nrow=I,ncol=maxScore))
    colnames(itemInfo) = paste0("cat",1:maxScore)
    itemInfo = data.frame(item.ID = 1:I,
                          item.name = as.character(colnames(wide)),
                          cons.ID = rep(consInfo$cons.ID[1],I),
                          item.type = rep(NA,I),
                          fixed = rep(FALSE,I),
                          itemInfo,
                          stringsAsFactors = FALSE)
  }

  # order wide in same order as items are given in itemInfo
  wide = wide[itemInfo$item.name]

  # addition of any input/argument also requires an update of the functions in
  #   internalChecks.R
  #   Note this must run AFTER the wide object is created
  checkObjs(wide, itemInfo, consInfo, varsInfo, estPackage, retainOrig,
            missingAs0, longFormat, persMethod, consecutive, writeout,
            imageType)

  I <- nrow(itemInfo) # number of items
  N <- nrow(wide)     # number of persons
  D <- nrow(consInfo) # number of dimensions

  if ( missingAs0 ) {
    for (i in 1:I) {
      wide[is.na(wide[,i]),i] =
        min(which(as.logical(itemInfo[i,6:ncol(itemInfo)])))
    }
  }
  scoresOrig <- wide
  # recode the scores -- must be sequential nonnegative integers for each item
  # there must be a way to vectorize this???
  for (i in c(1:I)) {
    J = sort(unique(wide[,i]),decreasing=FALSE)
    for (j in c(1:length(J))) {
      wide[,i][wide[,i]==J[j]] = j - 1
    }
  }

  # check for and flag items with no variability
  dropped = noVar(wide)
  if (length(dropped) > 0) {
    warning(length(dropped),
            " item(s) showed no response variability and were dropped from analysis:\n",
            paste(paste("    ",itemInfo$item.name[dropped]," (",
                        itemInfo$item.ID[dropped],")",sep=""),collapse="\n"),
            "\n")
    itemInfo = itemInfo[-dropped,]
    wide = wide[,-dropped]
  }

  # TAM estimation
  if ( estPackage == "TAM" ) {
    Q <- as.matrix(table(ordered(itemInfo$item.ID,levels=itemInfo$item.ID),
                   ordered(itemInfo$cons.ID,levels=consInfo$cons.ID)))
    colnames(Q) = consInfo$short.name[consInfo$cons.ID==as.numeric(colnames(Q))]
    rownames(Q) = itemInfo$item.name[itemInfo$item.ID==as.numeric(rownames(Q))]

    if ( consecutive ) {
      # anchor covariance between any two dimensions to 0
      anch.cov <- cbind(t(combn(1:D,2)),rep(0,choose(D,2)))
      warning('Item locations for items scored on different constructs cannot be directly compared.')
    } else {
      anch.cov <- NULL
    }

    results <- TAM::tam.mml(wide, irtmodel="PCM2", constraint = "cases",
                            Q = Q, variance.fixed = anch.cov,
                            control = list(progress=FALSE))
    # maxK = results$maxK

    # organize item estimates
    itemEsts <- TAM.items(results, itemInfo)

    # organize person estimates
    persEsts <- TAM.pers(results, wide, itemInfo, itemEsts$Thres, consInfo, consecutive, persMethod)

    # calculate fit statistics for both items and persons
    itemFit <- TAM::tam.fit(results,progress = FALSE)$itemfit
    colnames(itemFit) = c("item","outfit","outfit_t","outfit_p","outfit_pholm",
                          "infit","infit_t","infit_p","infit_pholm")
    if ( consecutive && D > 1 ) {
      persFit <- list()
      for (d in 1:D) {
        persFit[[d]] <- data.frame(matrix(nrow=N,ncol=4))
        persFit[[d]][complete.cases(persEsts$Pars),] <-
          sirt::pcm.fit(b = -results$AXsi[which(itemInfo$cons.ID == consInfo$cons.ID[d]),-1],
                        theta = as.matrix(persEsts$Pars[complete.cases(persEsts$Pars),d]),
                        dat = wide[complete.cases(persEsts$Pars),which(itemInfo$cons.ID == consInfo$cons.ID[d])])$personfit[,-1]
      }
      names(persFit) <- consInfo$short.name
    } else if ( D == 1 ) {
      persFit <- data.frame(matrix(nrow=N,ncol=4))
      persFit[complete.cases(persEsts$Pars),] <-
        sirt::pcm.fit(b = -results$AXsi[,-1],
                      theta = as.matrix(persEsts$Pars[complete.cases(persEsts$Pars),1]),
                      dat = wide[complete.cases(persEsts$Pars),])$personfit[,-1]
    } else {
      # ?? multi dimensional person fit ??
      warning('Person fit statistics are not available for non-consecutive multi-dimensional analyses using TAM.')
      persFit <- NULL
    }

    # person population parameter estimates
    persVar = results$variance
    persMean = results$beta
    row.names(persVar) <- colnames(persVar) <- colnames(persMean) <- consInfo$short.name
    row.names(persMean) <- "popMean"

    # separation reliability
    if ( persMethod == "EAP" ) {
      sepRel <- results$EAP.rel
    } else {
      sepRel <- sapply(1:D,function(x) {
      # ( SSD               - MSE                  ) / SSD
        ( var(persPars[,x]) - sum(persSEs[,x]^2)/N ) / var(persPars[,x])
        })
    }
    names(sepRel) = consInfo$short.name

    # estimation summary items
    if ( results$maxK == 2 ) {
      IRTmodel = "Rasch"
    } else if ( results$maxK > 2 ) {
      IRTmodel = "PCM"
    } else {
      IRTmodel = "ERROR"
    }

  # sirt estimation
  } else if ( estPackage == "sirt" ) {

  }

  # empty categories (used in subsequent functions)
  empties <- empty.cats(scoresOrig,
                        t(itemInfo[,6:ncol(itemInfo)]))

  # classical test statistics (one matrix for each dimension)
  classicalStats <- classical.test(wide, itemInfo, consInfo, sepRel)

  endTime = date()

  # need if estpackage==TAM
  estSummary = list(estPackage = estPackage, consecutive = consecutive,
                    IRTmodel = IRTmodel, startTime = startTime,
                    endTime = endTime, N = N, I = I,  D = D,
                    numIter = results$iter, numIntPoints = results$nnodes,
                    deviance = results$deviance, AIC = results$ic$AIC,
                    BIC = results$ic$BIC,
                    missingDataPerc = sum(is.na(wide))/prod(dim(wide))*100)

  if ( writeout ) {
    write.csv(cbind(itemEsts$Pars, itemEsts$SEs, itemEsts$Thres),
              paste0("itemGrid", fileSuffix, ".csv"))
    write.csv(cbind(persEsts$Pars, persEsts$SEs, raw = persEsts$Raw,
                    max = persEsts$Max), paste0("persGrid", fileSuffix, ".csv"))
    write.csv(itemFit, paste0("itemFit", fileSuffix, ".csv"))
    write.csv(persFit, paste0("persFit", fileSuffix, ".csv"))
    write.csv(rbind(persVar, persMean), paste0("popDist", fileSuffix, ".csv"))
    write.csv(classicalStats, paste0("classicalStats", fileSuffix, ".csv"))
    write.csv(as.matrix(estSummary), paste0("estSummary", fileSuffix, ".csv"))
    # organize empty category info into readable table
      rowMax <- max(sapply(empties, length))
      if (rowMax > 0) { # only write if there are empty categories
        empties0 <- data.frame(item.ID = itemInfo$item.ID,
                               item.name = itemInfo$item.name,
                               cons.ID = itemInfo$cons.ID,
                               category = do.call(rbind, lapply(empties,
                                          function(x) {
                                            length(x) <- rowMax
                                            x })))
        empties0 = reshape(empties0, direction = "long", varying = 4:ncol(empties0), idvar = "item.ID")
        empties0 = empties0[complete.cases(empties0),-4]
        write.csv(empties0, paste0("empties", fileSuffix, ".csv"), row.names = FALSE)
      }
  }

  output = list(itemPars = itemEsts$Pars,
                itemSEs = itemEsts$SEs,
                itemThres = itemEsts$Thres,
                itemFit = itemFit,
                persPars = persEsts$Pars,
                persSEs = persEsts$SEs,
                persRaw = persEsts$Raw,
                persMax = persEsts$Max,
                persFit = persFit,
                popDist = list(mean = persMean,
                               var.cov = persVar),
                sepRel = sepRel,
                estSummary = estSummary,
                classicalStats = classicalStats,
                empties = empties,
                scoresOrig = scoresOrig,
                scoresRecoded = wide,
                itemInfo = itemInfo,
                consInfo = consInfo,
                varsInfo = varsInfo)
  if ( retainOrig ) {
    output$estResults = results
  }
  return(output)
}
