
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
  ##cat(print(xtable(qa)),file=paste("./tex/Q_",N,"_Frequncies.tex",sep=""))
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
#  T <- rbind(c(C,rep("",dim(T)[2]-1)),T)
  print(T)
  write.csv(T,file=paste("./output/",N,".csv",sep=""));
  sink (file=paste("./tex/",N,".tex",sep=""),type=c("output"))
  try(print(xtable(T,caption=C)));
  sink(file=NULL)
}


interleave <- function(v1,v2)
{
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
}


interleave2d <- function(v1,v2)
{
  mat <- matrix(nrow=dim(v1)[1],ncol=dim(v1)[2]*2)
  dim(v2) <- dim(v1)
  for (c in 1:dim(v1)[2]){
    mat[,c*2-1] <- v1[,c];
    mat[,c*2] <- v2[,c];
  }
  rownames(mat) <- rownames(v1)
  return(mat)
}

factHist <- function (X,...) {
    c <- as.numeric(levels(factor(X)));
    b <- interleave(c,c+1);
    return (suppressWarnings(hist (X,b,na.rm=na.rm,freq=TRUE,plot=FALSE,right=FALSE)));
}


findOutliers <- function (X,lim) {
  if (lim<0) {
    return(rep(F,length(X)));
  } else {

    return (X>(mean(X)+lim*sd(X)));
  }
}

Codes <- read.csv(file="Constants/Codes.csv")
LastQ <- ""
Codes$Question <- as.character(Codes$Question)
for (q in 1:length(Codes$Question)) {
  if (Codes$Question[q]=="") {
    Codes$Question[q] <- LastQ;
  }else{
    LastQ <- Codes$Question[q];
  }
}
Codes$Question <- factor(Codes$Question)
Codes$Label <- as.character(Codes$Label)
CodeLables <- lapply(levels(Codes$Question),function(l) {sapply(which (Codes$Question==l),function(Q){return(Codes[Q,c("Value","Label")])})})
names(CodeLables) <- levels(Codes$Question)

#quick freq extractor
qCheck2<-function(Q=q4,N=NULL,groups=lgroups) {
  print(names(Q))
  qa<-t(sapply(Q,function(X){
                 qn<-colnames(lpsraw)[which(colnames(lps)==X)[1]]
                 s<-"";
                                        #        s='"'
                 ##sapply over groups
                 s <- sapply(groups,function(G)
                   {
                     N <- which(names(CodeLables)==qn)
                     if (length(N)>0) {
                       f<-factor(lps[G,X],levels=CodeLables[[N]][1,],labels=CodeLables[[N]][2,])
                     }else{
                       f<-factor(lps[G,X])
                     }
             
                     for (i in 1:length(levels(f))){
                       l=levels(f)[i]; nl="\r\n";
                       if (i==length(levels(f))) nl="";
                       s<-paste(s,sum(f==l,na.rm=TRUE),"----",l,nl,sep="")
                     }
                                        #             s<-paste(s,'"',sep="")
                     return(c(sum(!is.na(lps[G,X])),s))
                   })
                 return(c(qn,s))
               }))
  gcn<-sapply(names(groups),function(G){return (c(paste(G,"Resp Count"),paste(G,"Vals")))})
  ## dim(gcn) <- 2*length(groups)
  ## print(gcn)
  colnames(qa)<-c("QName",gcn)
  write.csv(qa,paste("./output/Q_",N,"_Frequncies.csv",sep=""))
##  cat(print(xtable(qa)),file=paste("./tex/Q_",N,"_Frequncies.tex",sep=""))
}


## {
##   Q <- colnames(IndiHealth)
##   qa<-t(sapply(Q,function(X){
##                  qn<-colnames(lpsraw)[which(colnames(lps)==X)[1]]
##                  s<-"";
##                                         #        s='"'
##                  ##sapply over groups
##                  s <- sapply(groups,function(G)
##                    {
##                      N <- which(names(CodeLables)==qn)
##                      if (length(N)>0) {
##                        f<-factor(lps[G,X],levels=CodeLables[[N]][1,],labels=CodeLables[[N]][2,])
##                      }else{
##                        f<-factor(lps[G,X])
##                      }
             
##                      for (i in 1:length(levels(f))){
##                        l=levels(f)[i]; nl="\r\n";
##                        if (i==length(levels(f))) nl="";
##                        s<-paste(s,sum(f==l,na.rm=TRUE),"----",l,nl,sep="")
##                      }
##                                         #             s<-paste(s,'"',sep="")
##                      return(c(sum(!is.na(lps[G,X])),s))
##                    })
##                  return(c(qn,s))
##                }))
##   gcn<-sapply(names(groups),function(G){return (c(paste(G,"Resp Count"),paste(G,"Vals")))})
##   ## dim(gcn) <- 2*length(groups)
##   ## print(gcn)
##   colnames(qa)<-c("QName",gcn)
##   write.csv(qa,paste("./output/Q_",N,"_Frequncies.csv",sep=""))
## ##  cat(print(xtable(qa)),file=paste("./tex/Q_",N,"_Frequncies.tex",sep=""))
## }




  
FmtPer <- function(R) {R[is.nan(R)] <- 0;sprintf(100*R,fmt="%.1f%%")}

interleaveBl  <- function (X) {
  interleave(X,rep("",length(X)))
}

intPer <- function(X,D) {
  interleave2d(X,FmtPer(t(t(X)/D)))
}
