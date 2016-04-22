#source("274A-functionsV1-3.R")

C.Name <- as.character(cons.info$Short.Name)
if(length(t(cons.info)[5:nrow(t(cons.info)),1])>13) {
  color = "gray" }  else {
    color = "pastel" }

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

pers.hist()

item.analysis = Rasch.itemanalysis()

if (Autosave) { pdf("WM.pdf",width=min(14,I),height=7) }
  # Wright Map in test order
  wrightMap(thetas=pers.pars[,1],
            thresholds=thresholds,
            show.thr.lab=maxK>2, #removes the "1" label if all items are dichotomous
            thr.lab.pos=2,
            main.title="Wright Map (instrument order)",
            axis.items="",
            label.items=item.info$Item.Name,
            label.items.srt=45,
            dim.names=C.Name,
            thr.sym.pch=21,thr.sym.col.fg="black",thr.sym.col.bg="#80b1d3")
if (Autosave) { dev.off()
                pdf("WM-constructorder.pdf",width=max(6,2*(ncol(cons.info)-5)),height=7) }
  # Wright Map in construct map order (grouped)
  cons.order = cbind(cat=factor(item.analysis[[2]][,2],levels=cons.info[1,5:ncol(cons.info)],ordered=TRUE),
                     item=factor(item.analysis[[2]][,1],levels=item.info[,3],ordered=TRUE),
                     thres=as.numeric(item.analysis[[2]][,7]))
  cons.order = reshape(as.data.frame(cons.order[complete.cases(cons.order),]),direction="wide",idvar="cat",v.names="thres",timevar="item")
  cons.order = cons.order[order(cons.order[,1]),-1]
  row.names(cons.order) = levels(factor(factor(item.analysis[[2]][as.numeric(item.analysis[[2]][,4])>0,2],levels=cons.info[1,5:ncol(cons.info)],ordered=TRUE)))
    # double use of factor() removes any possible unused levels
    # (item.analysis[[2]][,4]>0) only uses levels that were scored and aren't the lowest level
  colnames(cons.order) = levels(factor(factor(item.analysis[[2]][,1],levels=item.info[,3],ordered=TRUE)))
  wrightMap(thetas=pers.pars[,1],
            thresholds=cons.order,
            thr.lab.text = matrix(rep(colnames(cons.order),each=nrow(cons.order)),nrow=nrow(cons.order),ncol=ncol(cons.order)),
            label.items=row.names(cons.order),
            main.title="Wright Map (construct order)",
            axis.items="",
            label.items.srt=45,
            dim.names=C.Name,
            thr.sym.pch=21,thr.sym.col.fg="black",thr.sym.col.bg="#80b1d3")
if (Autosave) { dev.off() }

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
