SUP <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
               varsInfo = SUPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)
ADP <- craschR(scores = ADPwide, itemInfo = ADPitem, consInfo = ADPcons,
               varsInfo = ADPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)

AMY <- craschR(scores = AMYwide, itemInfo = AMYitem, consInfo = AMYcons,
               varsInfo = AMYvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = TRUE, writeout = FALSE)

wm(AMY)
wm(AMY, byCat = TRUE)
ICC.graph(AMY)
CPC.graph(AMY)
CPC.graph(AMY, observed = TRUE, palette = "PuBu")
CPC.graph(AMY, palette = "PuBu")
info.graph(AMY)
info.graph(AMY, type = "IIC")
info.graph(AMY, type = "TIC")
info.graph(AMY, type = "IIC", dim = 1)
info.graph(AMY, dim = 1, type = "TIC", completeOnly = FALSE)
info.graph(AMY, dim = 2)
item.analysis(AMY)
