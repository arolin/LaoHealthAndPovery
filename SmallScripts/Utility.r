
##Save Freqency of questions as files
mCheck<-function(Q=q4,N=NULL) {
  print(names(Q))
  qa<-t(sapply(Q,function(X){
         qn<-colnames(lpsraw)[which(colnames(lps)==X)[1]]
         s<-"";

 #        s='"'
         ##sapply over groups
         s <- sapply(lgroups,function(G)
           {
             f<-factor(lps[G,X])
             for (l in levels(f)){
               s<-paste(s,l,"=",sum(f==l,na.rm=TRUE),"\n",sep="")
             }
#             s<-paste(s,'"',sep="")
             return(c(sum(!is.na(lps[G,X])),s))
           })

         gcn<-sapply(names(lgroups),function(G){return (c(paste(G,"RespCount"),paste(G,"Vals")))})
         dim(gcn) <- 2*length(lgroups)
         s <- c(qn,s)
         names(s)<-c("QName",gcn)
         write.csv(t(s),paste("./output/",qn,"_",X,"_Frequncies.csv",sep=""))         
       }))

}




#quick freq extractor
qCheck<-function(Q=q4,N=NULL) {
  print(names(Q))
  qa<-t(sapply(Q,function(X){
         qn<-colnames(lpsraw)[which(colnames(lps)==X)[1]]
         s<-"";
 #        s='"'
         ##sapply over groups
         s <- sapply(lgroups,function(G)
           {
             f<-factor(lps[G,X])
             for (l in levels(f)){
               s<-paste(s,l,"=",sum(f==l,na.rm=TRUE),"\n",sep="")
             }
#             s<-paste(s,'"',sep="")
             return(c(sum(!is.na(lps[G,X])),s))
           })
         return(c(qn,s))
       }))
  gcn<-sapply(names(lgroups),function(G){return (c(paste(G,"RespCount"),paste(G,"Vals")))})
  dim(gcn) <- 2*length(lgroups)
  print(gcn)
  colnames(qa)<-c("QName",gcn)
  write.csv(qa,paste("./output/Q_",N,"_Frequncies.csv",sep=""))
}






##Takes a frequency table with factors and formats it into percent by column sums
PercentifyTable<-function(V,lv=NULL,lb=NULL) {
  NCols  <- length(V[1,])
  PT<- t(t(V)/rowSums(t(V)))
  PT<-sapply(PT,function(X){lapply(X,function(X){sprintf(X*100,fmt="%.1f%%")})})
  dim(PT)<-c(length(PT)/NCols,NCols)
  colnames(PT)<-colnames(V)
  rownames(PT)<-rownames(V)
  return(PT)
}

##multiply by mult (1 or 100) and add %sign 
ToPercents <- function(T,mult=100,dec=1) {
  TP<-sapply(T[,1:dim(T)[2]],function(X){lapply(X,function(X){sprintf(mult*X,fmt=paste('%.',dec,'f%%',sep=""))})})
  dim(TP)<-c(dim(T)[1],dim(T)[2])
  colnames(TP) <- colnames(T)
  rownames(TP) <- rownames(T)
  return(TP)
}

##write
SaveTables <- function(T,N,C) {
  write.csv(T,file=paste("./output/",N,".csv",sep=""));
  cat(print(xtable(T,caption=C)),file=paste("./tex/",N,".tex",sep=""));
}
