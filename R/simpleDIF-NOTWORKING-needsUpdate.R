################################################################################
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
