SUP <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
               varsInfo = SUPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)
ADP <- craschR(scores = ADPwide, itemInfo = ADPitem, consInfo = ADPcons,
               varsInfo = ADPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)

AMY <- craschR(scores = AMYwide, itemInfo = AMYitem, consInfo = AMYcons,
               varsInfo = AMYvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = TRUE, writeout = FALSE)
