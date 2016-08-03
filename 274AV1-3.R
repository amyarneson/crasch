## 274A annotated sample script for unidimensional analysis

# SPEARMAN'S RHO
#   This is only applicable to FULLY DICHOTOMOUS data or EQUIVALENTLY SCORED data
#   Spearman = Sp.rho(exp.ord=c(1,2,3,4,5),which.step=1)

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
