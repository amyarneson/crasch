## 274A annotated sample script for unidimensional analysis

####################################################################################################
# RUN MODEL & OBTAIN DEFAULT OUTPUT
source("274A-defaultV1-3.R")

####################################################################################################
# POST-ESTIMATION OUTPUT
#
# KIDMAPS
#   code to find all underfitting respondents (their rows)
    which(pers.pars$infit > 4/3)
    which(pers.pars$infit == max(pers.pars$infit))
#   code to find a particular person ID
    row.names(pers.pars)[2]
#   run the KIDMAP, the input is the ROW of the person you want to create it for
#   You can only make one at a time. Change the input and rerun for others.
  KIDMAP(2)
#
# SPLIT-HALVES RELIABILITY COEFFICIENT
#   You need to write vectors for item.set1 & item.set2 (there can be no overlap)
#     For consecutive numbers (1-10,etc.), you can use this syntax:
#       1:10
#     For non-consecutive numbers (3,6,7,10), you must use this syntax:
#       c(3,6,7,10)
#     Do not use quotation marks and keep the parentheses as is
  split.rel = split.halves(item.set1=c(1,2,4,7,8,10,12),
                           item.set2=c(3,5,6,9,11,13))
  split.rel
#
# SPEARMAN'S RHO
#   This is only applicable to FULLY DICHOTOMOUS data or EQUIVALENTLY SCORED data
#   Spearman = Sp.rho(exp.ord=c(1,2,3,4,5),which.step=1)
#
# MEAN ABILITY TRAJECTORIES
#   You might use this graph for item-level validity.
  mean.traj()
####################################################################################################
# CUSTOMIZED OUTPUT
#
# Model-based CPC graphs with theta lines, marked with probabilities (no empirical dots)
#   You can put a single line on the graph with the following syntax:
#     theta = -1.2)
#   Or multiple lines by writing a vector
#     theta = c(-1,1)
  CPC.graph(theta=c(0),empirical=FALSE,outfile="CPC-th")
# Note that without specifying a new -outfile- name, your existing CPC graphs will be overwritten
#
# SEM graphs with all cases
#   You may want to see a SEM graph with all cases (especially if you had no complete cases)
  SEM.graph(complete.only=FALSE)
#
####################################################################################################
# DEMOGRAPHICS, D.I.F., & EXTERNAL CORRELATIONS
# These analyses are optional and only possible if you have collected demographics on your
  demogs <- read.csv("demogs.csv",stringsAsFactors=TRUE,header=TRUE,row.names=1)
  View(demogs) # See if/how R renamed variables, values, etc.
  DIF1 = simple.DIF(groups=demogs$ELA.SS,var.name="ELA/SS")
  DIF2 = simple.DIF(groups=demogs$CORE,var.name="Core/NonCore")
#
# To correlate your estimates with an external/demographic (numeric,continuous) variable, you can
#   use the R function cor().  People in your demogs file need to be ordered exactly as they are in
#   your -wide- data object.
  cor.test(pers.pars[,1],demogs$NAMEOFYOURVARIABLE)
#
####################################################################################################
# CITATIONS
citation() # R citation information
citation("TAM") # estimation, item fit statistics, person (EAP only) fit statistics
citation("sirt") # person fit statistics (MLE, WLE, MAP)
citation("WrightMap") # WrightMap graphic
# You do not need to cite the BASS functions, but you can IF AND ONLY IF you use the *graphics*.
# (The tables just summarize output of other functions/packages, so no citation is necessary.)
# Arneson, A. E.  Berkeley Assessment System Software R Functions version 1.  http://bearcenter.berkeley.edu.
