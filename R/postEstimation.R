###############################################################################
#' Creates item analysis tables.
#'
#' @param results The output from a run of \code{craschR}. (link?)
#' @param dim Specify which dimension(s) to create graphic/tables for.  If
#'   \code{NULL}, output and graphics for each dimension will be produced.
#' @param writeout A logical indicated whether the estimate objects should be
#'   written to your working directory as CSVs.
#' @param filePrefix A character string that will be affixed to the beginning
#'   of each file (if \code{writeout = TRUE}). Use this if you are conducting
#'   multiple analyses in the same working directory and do not wish for your
#'   existing files to be overwritten.
#'   
#' @return A list with the following entries:
#'   \item{byItem}{A matrix with classical statistics and descriptives by item.}
#'   \item{byStep}{A matrix with classical statistics and descriptives by step.}
#' 
#' @export

item.analysis <- function(results, writeout = FALSE, filePrefix= NULL) {
  
  # create the byItem table (for TAM output)
  # note that the item name will be the row name
  byItem = cbind(Item_no = 1:results$estSummary$I,
                 Construct = results$consInfo$short.name[
                   match(results$itemInfo$cons.ID, results$consInfo$cons.ID)],
                 Count = colSums(!is.na(results$scoresRecoded)),
                 Missing = colSums(is.na(results$scoresRecoded)),
                 Estimate = results$itemPars[,1],
                 SE_est = results$itemSEs[,1],
                 results$itemFit[1:nrow(results$itemPars), c(2:4,6:8)]
  )
  
  #persProp table needed for point biserials
  persProp = results$persRaw / results$persMax
  
  #byStep table for PCM
  if (ncol(results$itemThres) > 1) {
    K_total = sum(colSums(results$itemInfo[,6:ncol(results$itemInfo)]))
    byStep = data.frame(Item = vector(mode = "character", length = K_total), 
                        Construct = vector(mode = "character", length = K_total),
                        Cat = vector(mode = "character", length = K_total), 
                        Count = as.vector(rep(NA, K_total), mode = "numeric"), 
                        Score = as.vector(rep(NA, K_total), mode = "numeric"),
                        Estimate = as.vector(rep(NA, K_total), mode = "numeric"), 
                        SE_est = as.vector(rep(NA, K_total), mode = "numeric"), 
                        Threshold = as.vector(rep(NA, K_total), mode = "numeric"), 
                        PtBiserial = as.vector(rep(NA, K_total), mode = "numeric"),
                        MeanPersLoc = as.vector(rep(NA, K_total), mode = "numeric"), 
                        SD_PersLoc = as.vector(rep(NA, K_total), mode = "numeric"),
                        stringsAsFactors = FALSE)
    
    start <- 1
    for (i in 1:results$estSummary$I) {
      cons = which(results$consInfo$cons.ID == results$itemInfo$cons.ID[i]) # row index
      K_i = sum(as.logical(results$itemInfo[i, 6:ncol(results$itemInfo)]))
      fill = c(start:(start + K_i - 1))
      # item, construct, and category (construct specific) names
        byStep$Item[fill] = rep(results$itemInfo$item.name[i], K_i)
        byStep$Construct[fill] = rep(results$consInfo$short.name[cons], K_i)
        byStep$Cat[fill] = as.matrix(results$consInfo[cons,4:ncol(results$consInfo)][which(results$itemInfo[i,6:ncol(results$itemInfo)]==1)])
      # figure out which categories are empty
        if (length(results$empties[[i]]) > 0) {
          byStep[fill[(byStep$Cat[fill] %in% results$consInfo[cons,4:ncol(results$consInfo)][results$empties[[i]]] )], 4] = 0
          # skip all empty categories for remaining statistics
          fill = fill[!(byStep$Cat[fill] %in% results$consInfo[cons,4:ncol(results$consInfo)][results$empties[[i]]])]
        }
      # counts per category
        byStep$Count[fill] = table(results$scoresRecoded[,i])
      # recoded scores
        byStep$Score[fill] = 0:(length(fill) - 1)
      # location estimates and standard errors
        byStep$Estimate[fill[-1]] = results$itemPars[i,2:(length(fill))]
        byStep$SE_est[fill[-c(1,length(fill))]] = results$itemSEs[i,2:(length(fill) - 1)]
      # thresholds
        byStep$Threshold[fill[-1]] = results$itemThres[i,1:(length(fill) - 1)]
      # point biserials
        byStep$PtBiserial[fill] = sapply(fill,function(x) {
          -ltm::biserial.cor(persProp[!is.na(results$scoresRecoded[,i]),cons],
                             results$scoresRecoded[!is.na(results$scoresRecoded[,i]),i]==byStep$Score[x])
        })
      # person locations
        byStep$MeanPersLoc[fill] = sapply(fill,function(x) {
          mean(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons], 
               na.rm = TRUE)
        })
        byStep$SD_PersLoc[fill] = sapply(fill,function(x) {
          sd(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons],
             na.rm = TRUE)
        })
        
        start = start + K_i
    }
  } else if (ncol(results$itemThres == 1)) {
      # create the byStep table (for dichotomous items)
      K_total = 2 * results$estSummary$I
      byStep = data.frame(Item = vector(mode = "character", length = K_total), 
                          Construct = vector(mode = "character", length = K_total),
                          Cat = vector(mode = "character", length = K_total), 
                          Count = as.vector(rep(NA, K_total), mode = "numeric"), 
                          Score = as.vector(rep(NA, K_total), mode = "numeric"),
                          Estimate = as.vector(rep(NA, K_total), mode = "numeric"), 
                          SE_est = as.vector(rep(NA, K_total), mode = "numeric"), 
                          Threshold = as.vector(rep(NA, K_total), mode = "numeric"), 
                          PtBiserial = as.vector(rep(NA, K_total), mode = "numeric"),
                          MeanPersLoc = as.vector(rep(NA, K_total), mode = "numeric"), 
                          SD_PersLoc = as.vector(rep(NA, K_total), mode = "numeric"),
                          stringsAsFactors = FALSE)
      
      for(i in 1:results$estSummary$I) {
        fill = c(2*i - 1, 2*i)
        # item and category names
          byStep$Item[fill] = rep(results$itemInfo$item.name[i], 2)
          byStep$Construct[fill] = rep(results$consInfo$short.name[cons], 2)
          byStep$Cat[fill] = as.matrix(results$consInfo[cons,4:ncol(results$consInfo)][which(results$itemInfo[i,6:ncol(results$itemInfo)]==1)])
        # there will be no empty categories (if there were, tam() would have failed)
        # counts per category
          byStep$Count[fill] = table(results$scoresRecoded[,i])
        # recoded scores
          byStep$Score[fill] = 0:1
        # location estimates and standard errors
          byStep$Estimate[fill[2]] = results$itemPars[i,1]
          byStep$SE_est[fill[2]] = results$itemSEs[i,1]
        # thresholds
          byStep$Threshold[fill[2]] = results$itemThres[i]
        # point biserials
          byStep$PtBiserial[fill] = sapply(fill,function(x) {
            -ltm::biserial.cor(persProp[!is.na(results$scoresRecoded[,i]),cons],
                               results$scoresRecoded[!is.na(results$scoresRecoded[,i]),i]==byStep$Score[x])
          })
        # person locations
          byStep$MeanPersLoc[fill] = sapply(fill,function(x) {
            mean(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons], 
                 na.rm = TRUE)
          })
          byStep$SD_PersLoc[fill] = sapply(fill,function(x) {
            sd(results$persPars[results$scoresRecoded[!is.na(results$scoresRecoded[,i]), i] == byStep$Score[x], cons],
               na.rm = TRUE)
          })

      }
    }
  
  if(writeout) {
    write.csv(byItem, paste0(filePrefix,"item-byitem.csv"))
    write.csv(byStep, paste0(filePrefix,"item-bystep.csv"))
  }
  
  list(byItem,byStep)
}