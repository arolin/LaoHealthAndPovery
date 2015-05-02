vals=c()
NACount=c()
Levels=c()
for (n in 1:length(names(lpsraw))) {
  f<-factor(lps[,n])
  s<-"";
  s='"';#paste(n,',"',names(lps)[n],'","NA{',sum(is.na(f)),"}\",\"",sep="") 
  #if (length(levels(f))<11) {
    for (l in levels(f)){
      s<-paste(s,l,"{",sum(f==l,na.rm=TRUE),"} ",sep="")
    }
  #}
  s<-paste(s,'"',sep="")
  #cat(s)
  vals<-c(vals,s)
  NACount=c(NACount,sum(is.na(f)))
  Levels=c(Levels,s)
}

TableCheck<-read.csv("Constants/ColNames.csv")
TableCheck<-cbind(names(lpsraw),TableCheck,NACount,Levels)

write.csv(TableCheck,"TableCheck.csv")

