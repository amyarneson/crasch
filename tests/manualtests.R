SUP <- craschR(scores = SUPwide, itemInfo = SUPitem, consInfo = SUPcons,
               varsInfo = SUPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)
ADP <- craschR(scores = ADPwide, itemInfo = ADPitem, consInfo = ADPcons,
               varsInfo = ADPvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = FALSE, writeout = FALSE)

AMY <- craschR(scores = AMYwide, itemInfo = AMYitem, consInfo = AMYcons,
               varsInfo = AMYvars, estPackage = "TAM", retainOrig = TRUE,
               consecutive = TRUE, writeout = FALSE)


ADP1 <- craschR(scores = ADPwide[,c(1,2,4,7,8,10,12)],
                itemInfo = ADPitem[c(1,2,4,7,8,10,12),], consInfo = ADPcons,
                estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                writeout = FALSE)
ADP2 <- craschR(scores = ADPwide[,c(3,5,6,9,11,13)],
                itemInfo = ADPitem[c(3,5,6,9,11,13),], consInfo = ADPcons,
                estPackage = "TAM", retainOrig = TRUE, consecutive = FALSE,
                writeout = FALSE)
