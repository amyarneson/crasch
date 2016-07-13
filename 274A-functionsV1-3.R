## BASS Functions -- 274A versions

####################################################################################################
# Return the split-halves reliability coefficient
# requires the mirt package and a previous UNIDIMENSIONAL mirt()

split.halves <- function(data=wide,item.set1,item.set2,
                         thres=thresholds,
                         TAM.object=results,method=Method) {
  require(TAM)
  require(mirt)

  TAM1 <- tam(data[,item.set1],control=list(progress=FALSE))
  if (method=="EAP") {
    pers1 <- TAM1$person[,6]
  } else {
    calc.pcm <- function(theta,ii,all.thres) {
      require(boot)
      attributes(all.thres)$dimnames = NULL
      bounds = t(sapply(theta, function (x) {
        c(1,inv.logit(x-all.thres[ii,])[!is.na(all.thres[ii,])],0)
      }))
      probs = bounds[,1:(ncol(bounds)-1)] - bounds[,2:ncol(bounds)]
      return(probs)
    }
    pers1 = IRT.mle(data[,item.set1],irffct=calc.pcm,arg.list=list("all.thres"=thres),type=method)
    # remove Inf and -Inf estimates that are created if method="MLE" or "WLE"
    if (min(pers1[,1])==-Inf|max(pers1[,1]==Inf)) {
      pers1[c(which(pers1[,1]==-Inf),which(pers1[,1]==Inf)),] = NA
    }
  }

  TAM2 <- tam(data[,item.set2],control=list(progress=FALSE))
  if (method=="EAP") {
    pers2 <- TAM2$person[,6]
  } else {
    calc.pcm <- function(theta,ii,all.thres) {
      require(boot)
      attributes(all.thres)$dimnames = NULL
      bounds = t(sapply(theta, function (x) {
        c(1,inv.logit(x-all.thres[ii,])[!is.na(all.thres[ii,])],0)
      }))
      probs = bounds[,1:(ncol(bounds)-1)] - bounds[,2:ncol(bounds)]
      return(probs)
    }
    pers2 = IRT.mle(data[,item.set2],irffct=calc.pcm,arg.list=list("all.thres"=thres),type=method)
    # remove Inf and -Inf estimates that are created if method="MLE" or "WLE"
    if (min(pers2[,1])==-Inf|max(pers2[,1]==Inf)) {
      pers2[c(which(pers2[,1]==-Inf),which(pers2[,1]==Inf)),] = NA
    }
  }

  r = cor(pers1,pers2,method="pearson",use="complete.obs")

  corr.r = as.numeric( (2*r) /(1 + r) )

  return(corr.r)
}

####################################################################################################
# Mean ability trajetories (spaghetti plot / trellis plots)

mean.traj <- function(BASSR.steps=item.analysis[[2]],
                      outfile="meantraj",imgtype=Image.Type,writeout=Autosave) {
  require(RColorBrewer)

  if (writeout) {
    eval(parse(text=paste0(imgtype,"('",outfile,".",imgtype,"')")))
  }
  toplot <- BASSR.steps[,c(1,4,9)]
  interaction.plot(as.numeric(toplot[,2]),toplot[,1],as.numeric(toplot[,3]),
                   legend=F,col=brewer.pal(min(nrow(toplot),12),"Paired"),lty=1,lwd=2,
                   ylab="Mean Ability",xlab="Numeric Score",
                   main="Mean Ability Trajectories by Item")
  axis(1,at=seq(0,max(as.numeric(toplot[,2]),na.rm=TRUE)),labels=FALSE)

  if (writeout) { dev.off() }
}

####################################################################################################
# Calculate Spearman's rho
# which.step = c("avg","high")

Sp.rho <- function(item.thres=thresholds,which.step=1,exp.ord) {
  #needs to be a matrix,can't choose a step that doesn't exist,only for dichotomous or 'equivalently scored' data
  stopifnot(is.matrix(item.thres),which.step<=ncol(item.thres),!anyNA(item.thres))
  cor(exp.ord,as.numeric(rank(item.thres[,which.step])),method="spearman")
}

####################################################################################################
# Complete 'naive' DIF a la 274A
# 95% comparative CIs follow Goldstein and Healy (1995)
# data is in wide form
# groups is a vector (logical, factor, 0/1) that indicates BINARY group membership
# CI is the desired confidence interval %

simple.DIF <- function(data=wide,groups,CI=95,var.name,
                       outfile="DIF",imgtype=Image.Type,writeout=Autosave) {
  stopifnot(length(table(groups))==2)
  require(TAM)

  if (is.factor(groups)) {
    groups0 = as.logical(as.numeric(groups) - 1)[!is.na(groups)]
  } else if (is.numeric(groups)) {
    stopifnot(max(groups)==1,min(groups)==0)[!is.na(groups)]
    groups0 = as.logical(groups)
  }

  if (sum(groups0)<8 | sum(!groups0)<8) {
    stop('Groups not large enough to perform estimation.')
  }

  group1 = as.character(groups[groups0][1])
  group2 = as.character(groups[!groups0][1])
  data1 = data[groups0,]
  data2 = data[!groups0,]

  # check that all items IN BOTH GROUPS have the same categories represented
  test1 = apply(data1,2,function(x) { sort(unique(x)) })
  test2 = apply(data2,2,function(x) { sort(unique(x)) })
  include = sapply(1:ncol(data),function(x) {
    identical(test1[[x]],test2[[x]])
  })

  if (sum(include)<4) {
    stop('Not enough information to complete simple DIF analysis. Too many items had to be dropped.')
  }

  # run results only on items that 'match' between groups
  results1 = tam.mml(data1[,include],irtmodel="PCM",constraint="cases",control=list(progress=FALSE))
  results2 = tam.mml(data2[,include],irtmodel="PCM",constraint="cases",control=list(progress=FALSE))

  out = cbind(results1$xsi,results2$xsi)
  # rename the rows so not to get confused with Cats in cons.info
  # these are labeled with the NUMERIC SCORE, not the construct level
  rownames(out) = gsub('Cat',"",rownames(out))
  # find the appropriate z multiplier following Goldstein & Healy (1995)
  out$z = qnorm(1-(100-CI)/200) * sqrt(out[,2]^2 + out[,4]^2) / (out[,2] + out[,4])
  out$low1 = out[,1] - out$z * out[,2]
  out$high1 = out[,1] + out$z * out[,2]
  out$low2 = out[,3] - out$z * out[,4]
  out$high2 = out[,3] + out$z * out[,4]

  p = nrow(out) # number of item*step estimates to plot

  if (writeout) {
    eval(parse(text=paste0(imgtype,"('",outfile,"-",var.name,".",imgtype,"',width=max(14,",p/2,"),height=7",")")))
  }
  par(def.par)

  # plot the estimates for group 1
  layout(matrix(c(1,2),nrow=2),heights=c(8,1))
  plot(x=seq(1,3*p-2,3),y=out[,1],
       axes=FALSE,col="#80b1d3",pch=20,
       xlim=c(0,3*p),ylim=c(min(c(out[,6],out[,8])),max(c(out[,7],out[,9]))+1),
       xlab="",ylab="delta_ij",main=paste("Item Estimates by",var.name,"Group"))
  arrows(seq(1,3*p-2,3),out[,6],seq(1,3*p-2,3),out[,7],
         length=0.05,angle=90,code=3,col="#80b1d3")
  # plot the estimates for group 2
  points(x=seq(2,3*p-1,3),y=out[,3],col="indianred2",pch=20)
  arrows(seq(2,3*p-1,3),out[,8],seq(2,3*p-1,3),out[,9],
         length=0.05,angle=90,code=3,col="indianred2")
  # separate the items with vertical lines
  abline(v=seq(0,3*p,3),col="grey",lty=2)
  # labels & axes
  text(x=seq(1,3*p-2,3),par("usr")[3]-.2,labels=rownames(out),srt=45,pos=1,xpd=TRUE)
  axis(2,at=seq(floor(2*min(c(out[,6],out[,8])))/2-.5,ceiling(2*max(c(out[,7],out[,9])))/2+.5),labels=FALSE)
  axis(2,at=seq(-6,6))
  par(mai=rep(0,4))
  plot.new()
  legend(x="center",legend=c(group1,group2),
         col=c("#80b1d3","indianred2"),lwd=1)

  if (writeout) { dev.off() }

  flagged = rownames(out)[out[,7]<out[,8] | out[,6]>out[,9]]
  if (length(flagged)==0) { flagged = "none" }

  skipped = colnames(data)[!include]
  if (length(skipped)==0) { skipped = "none" }

  cat('Items (by step) showing DIF: ',paste(flagged,collapse=", "),"\n",
      'Items not analyzed: ',paste(skipped,collapse=", "),sep="")

  ret <- list(flagged=flagged,skipped=skipped,results=out)
  return(ret)
}
