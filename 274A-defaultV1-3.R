#source("274A-functionsV1-3.R")

# set graphical parameters for reset within each graphing function
def.par = par(mai=c(1.36,1.093333,1.093333,0.56),
              mar=c(5.1,4.1,4.1,2.1),
              xpd=FALSE)

# create the itemsXscores object
itemsXscores <- Filter(function(x)!all(is.na(x)),
                       itemInfo[itemInfo$cons.ID==
                                  consInfo[1,1],c(6:ncol(itemInfo))])
row.names(itemsXscores) = as.character(itemInfo$item.name)

# graphic output and accompanying tables

ICC.graph()
CPC.graph()
SEM.graph()

infitMNSQ.graph() #overall item fit
if (maxK>2) { #step fit (for polytomous items only)
  infitMNSQ.graph(items=factor(fit$itemfit$parameter[(nrow(item.info)+1):length(fit$itemfit$parameter)],
                               levels=c(as.character(fit$itemfit$parameter))[(nrow(item.info)+1):length(fit$itemfit$parameter)],
                               ordered=TRUE),
                  infitMNSQ=fit$itemfit$Infit[(nrow(item.info)+1):length(fit$itemfit$parameter)],
                  main="Step Fit",outfile="infitMNSQ-steps")
}

report.summary()
