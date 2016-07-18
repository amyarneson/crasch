################################################################################
# calculate sum scores -- raw scores and maximum possible scores -- for a
#   response data frame
# type = c("raw","max") based on which sum you want (persons raw score or their
#   possible max score)

sum.scores <- function(data, type = "raw") {
  if ( type == "raw" ) {
    base::rowSums(data,na.rm=TRUE)
  } else if ( type == "max" ) {
    maxVect <- apply(data,2,max,na.rm=TRUE)
    apply(data,1,function(x) {
      sum(maxVect[!is.na(x)])
    })
  }
}

################################################################################
# make the classicalStats object (for craschR)

classical.test <- function(data, itemInfo, consInfo, sepRel) {
  output = list()
  D = nrow(consInfo)
  I = nrow(itemInfo)
  N = nrow(data)
  for (d in 1:D) {
    # recode missings to 0
    dataMiss <- data[, itemInfo$cons.ID == consInfo$cons.ID[d] ]
    dataMiss[is.na(data[, itemInfo$cons.ID == consInfo$cons.ID[d] ])] = 0
    # complete cases only
    dataComp <- data[complete.cases(data), itemInfo$cons.ID == consInfo$cons.ID[d]]
    if (nrow(dataComp)==0) {
      Ncomp = 0
      Icomp <- meanComp <- sdComp <- NA
    } else {
      Ncomp <- nrow(dataComp)
      Icomp <- I
      meanComp <- mean(rowSums(dataComp))
      sdComp <- sd(rowSums(dataComp))
    }

    output[[d]] <- matrix(nrow=7,ncol=3,
                          c(N,           N,                                   Ncomp,
                            I,           I,                                   Icomp,
                            NA,          mean(rowSums(dataMiss)),             meanComp,
                            NA,          sd(rowSums(dataMiss)),               sdComp,
                            sum(is.na(data[, itemInfo$cons.ID == consInfo$cons.ID[d] ]))/prod(dim(data[, itemInfo$cons.ID == consInfo$cons.ID[d] ]))*100, 0, 0,
                            NA,          ltm::cronbach.alpha(dataMiss)$alpha, ltm::cronbach.alpha(dataComp)$alpha,
                            sepRel[d],   NA,                                  NA),
                          byrow=TRUE)
    row.names(output[[d]]) <- c("Number of respondents", "Number of items",
                                "Mean raw score", "SD raw score",
                                "Missing data %", "Cronbach's Alpha",
                                "Person Separation Reliability")
    colnames(output[[d]]) <- c("All Data (IRT)","Missing=0",
                               "Complete Cases only")
  }
  names(output) = consInfo$short.name
  return(output)
}

################################################################################
# create the item parameter objects from TAM output

TAM.items <- function(TAMresults, itemInfo) {
  maxK = TAMresults$maxK
  I = nrow(itemInfo)

  # for polytomous data
  if ( maxK > 2 ) {
    itemPars <- itemSEs <- matrix(ncol=I,nrow=maxK) #will transpose later
    # fill in the step parameters
    # the $item object from TAM output will help to figure out which cells
    #    need to be filled in
    fill = !is.na(TAMresults$item[,5:(3+maxK)])
    # remove last TRUE from each row (since that parameter is constrained)
    fill = apply(fill,1, function(x) {
      x[max(which(x))] = FALSE
      x
    })

    # fill in overall estimates
    itemPars[1,] = TAMresults$xsi[1:I,1]
    # fill in step estimates
    itemPars[2:maxK,][fill] = TAMresults$xsi[(I+1):nrow(TAMresults$xsi),1]
    # fill in 0 for tau1 if dichotomous item
    itemPars[2,][is.na(itemPars[2,])] = 0
    # fill in the constrained estimate
    itemPars = apply(itemPars,2, function(x) {
      x[max(which(!is.na(x)))+1] = -sum(x,na.rm=TRUE)
      x
    })
    #take out the double 0!
    itemPars[3,][itemPars[2,]==0] = NA

    # fill in SEs for overall estimates
    itemSEs[1,] = TAMresults$xsi[1:I,2]
    # fill in SEs for step estimates
    itemSEs[2:maxK,][fill] = TAMresults$xsi[(I+1):nrow(TAMresults$xsi),2]

    # make tables readable (transpose)
    itemPars = t(itemPars)
    itemSEs = t(itemSEs)
    colnames(itemPars) = c("d",paste0("tau",1:(maxK-1)))
    colnames(itemSEs) = c("SE_d",paste0("SE_tau",1:(maxK-1)))
    rownames(itemPars) <- rownames(itemSEs) <- itemInfo$item.name

    # make.thresholds uses CQ parameterization: delta_ij = delta_i + tau_ij
    itemThres <- WrightMap::make.thresholds(itemPars[,1] + itemPars[,2:maxK])
    colnames(itemThres) = paste0("tt_",c(1:ncol(itemThres)))

    # for dichotomous data:
  } else if ( maxK == 2 ) {
    itemPars <- TAMresults$xsi[,1]
    colnames(itemPars) = c("d")
    itemSEs <- TAMresults$xsi[,2]
    colnames(itemSEs) = c("SE_d")

    itemThres <- matrix(WrightMap::make.thresholds(itemPars[,1]),nrow=I,ncol=1)
    colnames(itemThres) = c("tt_1")
    rownames(itemThres) = rownames(itemPars)
  }

  return(list(Pars = itemPars,
              SEs = itemSEs,
              Thres = itemThres))
}

################################################################################
# create the person parameter objects from TAM output

TAM.pers <- function(TAMresults, data, itemInfo, itemThres, consInfo,
                     consecutive, persMethod) {
  D = nrow(consInfo)
  N = nrow(data)
  if ( persMethod=="EAP" ) {
    persPars <- data.frame(TAMresults$person[,seq(6,4+2*D,2)])
    persSEs <- data.frame(TAMresults$person[,seq(7,5+2*D,2)])
  } else {
    # remember this can only be used with unidimensional data
    # this is addressed in the internalCheck functions
    persPars <- sirt::IRT.mle(data, irffct = calc.pcm,
                              arg.list = list("all.thres"=itemThres),
                              type = persMethod, progress = FALSE)
    # remove Inf and -Inf estimates that are created if method="MLE" or "WLE"
    if ( min(persPars[,1])==-Inf | max(persPars[,1])==Inf ) {
      persPars[c(which(persPars[,1]==-Inf),which(persPars[,1]==Inf)),] = NA
    }
    persSEs <- matrix(persPars[,2])
    persPars <- matrix(persPars[,1])
  }
  colnames(persPars) = c(paste0(as.character(consInfo$short.name),"_",persMethod))
  colnames(persSEs) = c(paste0("SE_",as.character(consInfo$short.name)))
  rownames(persPars) <- rownames(persSEs) <- rownames(data)
  # raw scores
  if ( consecutive && D > 1 ) {
    persRaw <- persMax <- data.frame(matrix(nrow=N,ncol=D))
    for ( d in 1:D ) {
      persRaw[,d] = sum.scores(data[,which(itemInfo$cons.ID == consInfo$cons.ID[d])],
                               type = "raw")
      persMax[,d] = sum.scores(data[,which(itemInfo$cons.ID == consInfo$cons.ID[d])],
                               type = "max")
    }
    colnames(persRaw) <- colnames(persMax) <- consInfo$short.name
    row.names(persRaw) <- row.names(persMax) <- row.names(data)
  } else {
    persRaw <- data.frame(TAMresults$person$score)
    persMax <- data.frame(TAMresults$person$max)
  }

  return(list(Pars = persPars,
              SEs = persSEs,
              Raw = persRaw,
              Max = persMax))
}

################################################################################
# define the function necessary for IRT.mle()

calc.pcm <- function(theta,ii,all.thres) {
  attributes(all.thres)$dimnames = NULL
  bounds = t(sapply(theta, function (x) {
    c(1,boot::inv.logit(x-all.thres[ii,])[!is.na(all.thres[ii,])],0)
  }))
  probs = bounds[,1:(ncol(bounds)-1)] - bounds[,2:ncol(bounds)]
  return(probs)
}

################################################################################
# flag and return all empty categories (possible, but no one scored there)

empty.cats <- function(wide,
                       itemScoreInfo) {
  I = ncol(wide)
  item.list = list()

  for (i in 1:I) {
    item.list[[i]] <- which(!which(itemScoreInfo[,i]) %in% wide[,i])
  }

  return(item.list)
}

################################################################################
# get category probabilities

catProbs <- function(theta, itemThres) {
  I <- nrow(itemThres)
  cumulProbs <- matrix(c(rep(1, I),
                         boot::inv.logit(theta - itemThres),
                         rep(NA, I)),
                       nrow = I,
                       ncol = ncol(itemThres) + 2)
  # need to put 0s in at the end of each vector (the first NA of each row)
  for (i in 1:nrow(cumulProbs)) {
    cumulProbs[i, min(which(is.na(cumulProbs[i,])))] = 0
  }
  # transform from cumulative to point
  apply(cumulProbs, 1, function(y) { -diff(y) } )
}

################################################################################
# get expected scores based on
