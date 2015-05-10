codes<-read.csv("./Constants/Codes.csv",stringsAsFactors=F)
vlist <- NULL
lv   <- NULL
lb <- NULL
lastV <-  codes$Variable.Values[2]
lvlbs <- list()
for(i in 2:length(codes[,1])) {

  if(code$Variable.Values!="") {
    lastV <-codes$Variable.Values
    lvlbs[lastV,"levels"] <- vector()
    lvlbs[lastV,"lables"] <- vector()
  }
  vlist <- c(vlist,code$Variable.Values)
  lvlbs[lastV,"levels"] <- c(    lvlbs[lastV,"levels"],code$X)
  lvlbs[lastV,"lables"] <- c(    lvlbs[lastV,"lables"],code$X.1)

}
##names(lvlbs) <- vlist;
