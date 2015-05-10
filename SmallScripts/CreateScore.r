#Create Wealth Score
PossVals <-read.csv("./Constants/PossesionValues.csv")
WealthScore =rowSums(lps[,c(PossVarName,LiveStockVarName)]*PossVals$Value)
lps<-cbind(lps,WealthScore)
