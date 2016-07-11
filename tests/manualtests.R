# manual run-throughs of craschR using AMY
scores = AMYwide
itemInfo = AMYitem
consInfo = AMYcons
varsInfo = AMYvars
estPackage="TAM"
retainOrig = FALSE
missingAs0 = FALSE
longFormat = FALSE
persMethod = "EAP"
consecutive = TRUE
writeout = FALSE
imageType = "pdf"

scores = "~/OneDrive/Development/crasch/sample data files/M2 - amy/AMYwide.csv"
itemInfo = "~/OneDrive/Development/crasch/sample data files/M2 - amy/AMYitem.csv"
consInfo = "~/OneDrive/Development/crasch/sample data files/M2 - amy/AMYcons.csv"
varsInfo = "~/OneDrive/Development/crasch/sample data files/M2 - amy/AMYvars.csv"

test <- craschR(scores=AMYwide,itemInfo=AMYitem,consInfo=AMYcons,varsInfo=AMYvars,estPackage="TAM",consecutive=TRUE,retainOrig=TRUE,writeout=FALSE)
test <- craschR(scores,itemInfo,consInfo,varsInfo,estPackage="TAM",consecutive=TRUE,retainOrig=FALSE,writeout=FALSE)

ADP <- craschR(scores=ADPwide,itemInfo=ADPitem,consInfo=ADPcons,varsInfo=ADPvars,estPackage="TAM",retainOrig=FALSE,consecutive=FALSE,writeout=FALSE)
SUP <- craschR(scores=SUPwide,itemInfo=SUPitem,consInfo=SUPcons,varsInfo=SUPvars,estPackage="TAM",retainOrig=FALSE,consecutive=FALSE,writeout=FALSE)
AMY <- craschR(scores=AMYwide,itemInfo=AMYitem,consInfo=AMYcons,varsInfo=AMYvars,estPackage="TAM",retainOrig=FALSE,consecutive=TRUE,writeout=FALSE)

# 6 dimensional tests
ADM <- craschR(scores=ADMlong,itemInfo=ADMitem,consInfo=ADMcons,estPackage="TAM",longFormat=TRUE,retainOrig=FALSE,consecutive=TRUE,writeout=FALSE)
scores=ADMlong
itemInfo=ADMitem
consInfo=ADMcons
estPackage="TAM"
longFormat=TRUE
retainOrig=FALSE
consecutive=TRUE
writeout=FALSE

# 2-dimensional post-est functions


# today:
ADP <- craschR(scores = ADPwide, itemInfo = ADPitem, consInfo = ADPcons,
               varsInfo = ADPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)
item.scores(ADP)
item.scores(ADP, freqs = FALSE)
pers.hist(ADP)
CPC.graph(ADP)

AMY <- craschR(scores = AMYwide, itemInfo = AMYitem, consInfo = AMYcons,
               varsInfo = AMYvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = TRUE, writeout = FALSE)
item.scores(AMY)
pers.hist(AMY)
wm(AMY)
wm(AMY, itemOrder = "construct")
wm(AMY, itemOrder = 1:4) # errors/warnings -- look into this next time!
ICC.graph(AMY)
CPC.graph(AMY)
CPC.graph(AMY, observed = TRUE, palette = "PuBu")
info.graph(AMY)
info.graph(AMY, type = "IIC")
info.graph(AMY, type = "TIC")
info.graph(AMY, type = "IIC", dim = 2) # error in this one
info.graph(AMY, dim = 1, type = "TIC", completeOnly = FALSE)
item.analysis(AMY)
