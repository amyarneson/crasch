## BASS Functions -- 274A versions
## date: November 2015
## version: 1.3
## author(s): Amy E. Arneson (aea@berkeley.edu)

# DO NOT MAKE CHANGES TO THIS FILE WITHOUT FIRST CONSULTING WITH THE GSI!

####################################################################################################
# Create a standard error of measure (SEM) graph (unidimensional)

SEM.graph <- function(data=wide,theta=pers.pars[,1],
                      SE.theta=pers.pars[,2],
                      construct=as.character(C.Name),main="Standard Error of Measurement",
                      sub="(Complete Cases)",complete.only=TRUE,
                      outfile="SEMgraphCOMP",imgtype=Image.Type,writeout=Autosave) {
  if (complete.only) {
    stopifnot(sum(complete.cases(data))>0)
    theta=theta[complete.cases(data)]
    SE.theta=SE.theta[complete.cases(data)]
  } else {
    stopifnot(anyNA(wide))
    sub="(All Cases)"
    outfile="SEMgraphALL"
  }

  if (writeout) {
    eval(parse(text=paste0(imgtype,"('",outfile,".",imgtype,"')")))
  }
  par(def.par)
  layout(matrix(1,nrow=1))
  plot(theta,SE.theta,main=main,
       xlab="Logits",ylab="SE",axes=FALSE,
       pch=21,bg="#80b1d3")
  mtext(paste(construct,sub))
  axis(1,at=seq(floor(min(theta)),ceiling(max(theta)),by=1),labels=FALSE)
  axis(1,at=seq(0,min(-2,floor(min(theta))),by=-2))
  axis(1,at=seq(2,max(ceiling(max(theta)),2),by=2))
  axis(2,at=seq(floor(min(SE.theta)*10)/10,ceiling(max(SE.theta)*10)/10,by=.1),labels=FALSE)
  axis(2,at=seq(floor(min(SE.theta)*5)/5,ceiling(max(SE.theta)*5)/5,by=.2),las=1)

  if(writeout) { dev.off() }
}

####################################################################################################
# Create an infit MNSQ plot with 'acceptable range' highlighted
# items is an ordered vector of item names (factor)

infitMNSQ.graph <- function(items=factor(fit$itemfit$parameter[1:nrow(item.info)],
                                         levels=c(as.character(fit$itemfit$parameter[1:nrow(item.info)])),
                                         ordered=TRUE),
                            infitMNSQ=fit$itemfit$Infit[1:nrow(item.info)],main="Item Fit",
                            outfile="infitMNSQ",imgtype=Image.Type,writeout=Autosave) {

  if(writeout) {
    eval(parse(text=paste0(imgtype,"('",outfile,".",imgtype,"')")))
  }
  par(def.par)
  layout(matrix(1,nrow=1))
  dotchart(x=rev(infitMNSQ),rev(items),
           main=main,xlab="Infit MNSQ",ylab="",
           xlim=c(min(.7,min(infitMNSQ)),max(1.4,max(infitMNSQ))),
           pch=".")
  rect(xleft=3/4,xright=4/3,ybottom=0,ytop=(length(infitMNSQ)+1),
       col=rgb(red=128,green=177,blue=211,alpha=127.5,maxColorValue=255),border=NA)
  abline(v=1,col="darkgrey",lty=3)
  abline(v=3/4,col="#80b1d3",lty=3)
  abline(v=4/3,col="#80b1d3",lty=3)
  points(rev(infitMNSQ),items,pch=21,bg="#80b1d3")
  axis(1,at=c(seq(1,min(.7,min(infitMNSQ)),by=-.1),seq(1,max(1.4,max(infitMNSQ)),by=0.1)))

  if(writeout) { dev.off() }
}

####################################################################################################
# Create a KIDMAP

#pers.params = c(theta.hat,SE.theta.hat,raw.score,max.raw,outfit,outfit.t,infit,infit.t)
  #row.names = c("person.ID")
#item.params is a MATRIX with items as rows and steps as columns (row.names = item.names)
  #columns MUST correspond to scores (i.e. score of 1 means column 1)
#p.surprise is an ordered vector of the probabilities that create the "expected" boundary box

KIDMAP <- function(pers.row,
                   item.thres=as.matrix(thresholds),
                   p.surprise=c(.25,.75),
                   outfile="KIDMAP",imgtype=Image.Type,writeout=Autosave) {

  pers.params=pers.pars[pers.row,]
  pers.scores=wide[pers.row,]
  outfile = paste0(outfile,pers.row)

  item.thres0 = as.matrix(item.thres[!is.na(pers.scores),])
  pers.scores0 = pers.scores[!is.na(pers.scores)]

  to.plot = matrix(c(rep(1,prod(dim(item.thres0))),item.thres0),ncol=2,byrow=F)
  #all 'reached' categories will have an x-coordinate of -1
  #all 'unreached' will have an x-coord of 1
  for (i in 1:nrow(item.thres0)) {
    if (pers.scores0[i]>0) {
      for (j in 1:as.numeric(pers.scores0[i])) {
        to.plot[(j-1)*nrow(item.thres0)+i,1] = -1
      }
    }
  }
  row.names(to.plot) = as.vector(t(sapply(matrix(row.names(item.thres0),nrow=nrow(item.thres0)),
                                          paste,c(1:ncol(item.thres0)),sep="_")))
  to.plot = to.plot[!is.na(to.plot[,2]),]

  #surprise lines
  surp.high = as.numeric(pers.params[1]) - log(p.surprise[1]/(1-p.surprise[1]))
  surp.low = as.numeric(pers.params[1]) - log(p.surprise[2]/(1-p.surprise[2]))

  y.low = min(to.plot[,2],surp.low,na.rm=FALSE)-.2
  y.high = max(to.plot[,2],surp.high,na.rm=FALSE)+.2

  if (writeout) {
    eval(parse(text=paste0(imgtype,"('",outfile,".",imgtype,"')")))
  }
  par(mai=def.par$mai)
  layout(matrix(1,nrow=1))
  par(mar=c(5.1,4.1,5.3,2.1))
  plot(1,type="n",xlim=c(-1.5,1.5),ylim=c(y.low,y.high),
       axes=FALSE,xlab="",ylab="Logits",
       main="KIDMAP")
  mtext(paste0("Person: ",row.names(pers.params),
               "\nEst Theta: ",round(pers.params[1],2)),
        side=3,line=0,cex=.8)
  mtext(paste0("Raw: ",round(pers.params[3],2),"/",round(pers.params[4],2),
               "\nOutfit: ",round(pers.params[5],2)," (t=",round(pers.params[6],2),")",
               "\nInfit: ",round(pers.params[7],2)," (t=",round(pers.params[8],2),")"),
        side=1,line=2,cex=.7)
  mtext("Reached",adj=0)
  mtext("Not Reached",adj=1)
  rect(xleft=-1.5,xright=1.5,ybottom=surp.low,ytop=surp.high,
       col=rgb(red=128,green=177,blue=211,alpha=127.5,maxColorValue=255),border=NA)
  rect(xleft=-1.5,xright=0,ybottom=surp.high,ytop=100,
       col=rgb(red=255,green=229,blue=0,alpha=127.5,maxColorValue=255),border=NA)
  rect(xleft=0,xright=1.5,ybottom=-100,ytop=surp.low,
       col=rgb(red=255,green=229,blue=0,alpha=127.5,maxColorValue=255),border=NA)
  abline(v=0)
  abline(h=pers.params[1])
  Axis(side=2,at=c(seq(0,floor(y.low),by=-1),seq(0,ceiling(y.high),by=1)),las=1)
  Axis(side=4,at=c(seq(0,floor(y.low),by=-1),seq(0,ceiling(y.high),by=1)),las=1)
  points(to.plot,pch=21,bg="#80b1d3")

  if (length(names(to.plot[to.plot[,1]==-1,1]))==1) {
    text(-1,to.plot[to.plot[,1]==-1,2],names(to.plot[to.plot[,1]==-1,1]),pos=4,cex=.5)
  }

  if (length(names(to.plot[to.plot[,1]==1,1]))==1) {
    text(1,to.plot[to.plot[,1]==1,2],names(to.plot[to.plot[,1]==1,1]),pos=2,cex=.5)
  }

  if (length(names(to.plot[to.plot[,1]==-1,1]))>1) {
    lft.L = sort(to.plot[to.plot[,1]==-1,2])[seq(1,length(sort(to.plot[to.plot[,1]==-1,2])),2)]
    lft.R = sort(to.plot[to.plot[,1]==-1,2])[seq(2,length(sort(to.plot[to.plot[,1]==-1,2])),2)]
    text(rep(-1,length(lft.L)),lft.L,labels=names(lft.L),pos=2,cex=.5)
    text(rep(-1,length(lft.R)),lft.R,labels=names(lft.R),pos=4,cex=.5)
  }

  if (length(names(to.plot[to.plot[,1]==1,1]))>1) {
    rgt.L = sort(to.plot[to.plot[,1]==1,2])[seq(1,length(sort(to.plot[to.plot[,1]==1,2])),2)]
    rgt.R = sort(to.plot[to.plot[,1]==1,2])[seq(2,length(sort(to.plot[to.plot[,1]==1,2])),2)]
    text(rep(1,length(rgt.L)),rgt.L,labels=names(rgt.L),pos=4,cex=.5)
    text(rep(1,length(rgt.R)),rgt.R,labels=names(rgt.R),pos=2,cex=.5)
  }

  if (writeout) { dev.off() }
}

####################################################################################################
# Cumulative probability curves (weird shading)
# REQUIRES library(boot)

# item.params = ordered vector of step parameters (deltas)
# IF empirical=FALSE: theta = vector of where to label probabilities on the graph
# IF empirical=TRUE: theta = vector of estimated person abilities to graph
# min.cell is

CPC.graph <- function(data=wide,all.thres=thresholds,theta=pers.pars[,1],
                      items=row.names(thresholds),empirical=TRUE,min.cell=8,
                      col="blue",outfile="CPC",imgtype=Image.Type,writeout=Autosave) {
  require(boot)

  for (i in 1:nrow(all.thres)) {
    item.thres = all.thres[i,!is.na(all.thres[i,])]
    item.name = items[i]
    K = length(item.thres)

    thres = c(item.thres,Inf)

    if (writeout) {
      eval(parse(text=paste0(imgtype,"('",outfile,i,".",imgtype,"')")))
    }

    par(def.par)
    layout(matrix(1,nrow=1))
    plot(1,type="n",xlim=c(-6,6),ylim=c(0,1),axes=FALSE,xlab="Logits",ylab="Probability",
         main="Cumulative Category Probability Curves")
    mtext(as.character(item.name))
    axis(1,at=seq(-6,6,2))
    axis(1,at=seq(-5,5,2),labels=FALSE)
    axis(2,at=seq(0,1,.2),las=1)
    axis(2,at=seq(.1,.9,.2),labels=FALSE)

    if (empirical) {
      theta = round(theta/.5)*.5 #group at nearest .5 value for simplicity & to deal with sparse cells
      emp.pts = t(apply(prop.table(table(theta,wide[,i]),margin=1),1,function(x) {cumsum(rev(x))}))[,seq((K+1),1,by=-1)]
      cell.cts = t(apply(table(theta,wide[,i]),1,function(x) {cumsum(rev(x))}))[,seq((K+1),1,by=-1)]
      color = brewer.pal(max(K,3),"Paired")

      for (k in 1:K) {
        curve(inv.logit(x-item.thres[k]),from=-6,to=6,add=TRUE,col=color[k],lwd=2)
        points(sort(unique(theta))[cell.cts[,(k+1)]>=min.cell],emp.pts[cell.cts[,(k+1)]>=min.cell,(k+1)],col=color[k],pch=20)
      }
    } else {
      if (col=="gray") {
        if (K==2) {
          color = gray(level=c(.75,.6))
        } else {
          color = gray(level=seq(from=.75,to=.25,length.out=K))
        }
        linecol = "black"
        fillcol = "gray"
      } else if (col=="blue") {
        if (K==2) {
          color = rgb(red=128,green=177,blue=211,alpha=c(140,255),maxColorValue=255)
        } else {
          color = rgb(red=128,green=177,blue=211,alpha=seq(80,255,length.out=K),maxColorValue=255)
        }
        linecol = "gray"
        fillcol = "#80b1d3"
      }

      for (k in 1:K) {
        x <- c(seq(from=-6,to=6,length.out=500))
        y1 <- c(inv.logit(x-thres[k]))
        y2 <- c(inv.logit(x-thres[k+1]))
        polygon(c(x,rev(x)),c(y1,rev(y2)),col=color[k],border=NA)
        curve(inv.logit(x-item.thres[k]),from=-6,to=6,add=TRUE)
      }

      abline(v=theta,col=linecol,lty=2,lwd=2)

      for (a in 1:length(theta)) {
        bounds = inv.logit(theta[a] - item.thres)
        L = placement = rep(NA,length(bounds)+1)
        for (k in 0:K) {
          if (k==0) {
            up.bd = 1
          } else {
            up.bd = bounds[k]
          }
          if (k==K) {
            lo.bd = 0
          } else {
            lo.bd = bounds[k+1]
          }
          L[k+1] = paste0("p(",k,")=",round(up.bd-lo.bd,2))
          placement[k+1] = mean(c(up.bd,lo.bd))
        }
        points(x=rep(theta[a],length(bounds)),y=bounds,pch=21,bg=fillcol)
        text(x=rep(theta[a],length(bounds)+1),y=placement,labels=L,pos=4,cex=.5)
      }
    }
    if (writeout) { dev.off() }
  }
}


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
