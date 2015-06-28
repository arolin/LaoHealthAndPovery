
PlotOutPOOP <- function(Var,Cap) {
  OutPat$OOP  <- with(OutPat,get(Var)!=0);
  OutPat$OOP[is.na(OutPat$OOP)] <- T
  OutPat2 <- OutPat[,c("Facility","Group","Total","OOP")]
  OutPat2$Facility <- as.character(OutPat2$Facility)
  SkipFacility <- c("HV","NH","RH","TH","PC","PP")
  OutPat2 <- subset(OutPat,!(as.character(Facility) %in% SkipFacility))
  OutPat2$Facility <- factor(OutPat2$Facility)
  BF <- as.data.frame(with(OutPat2,
                           table(Facility,
                                 Group,
                                 OOP)))
  colnames(BF) <- c("Facility","Group","OOP","Count")
  BF$OOP <- as.logical(BF$OOP)
  BF$Percent <- 0
  BF$NOOP <- 0
  BF[BF$OOP,]$NOOP <- BF[ BF$OOP,]$Count;
  BF[!BF$OOP,]$NOOP <- BF[ BF$OOP,]$Count;
  BF[BF$OOP,]$Percent <-  100* BF[ BF$OOP,]$Count /(BF[BF$OOP,]$Count+BF[!BF$OOP,]$Count)
  BF[!BF$OOP,]$Percent <- 100* BF[!BF$OOP,]$Count /(BF[BF$OOP,]$Count+BF[!BF$OOP,]$Count)
  p <- ggplot(data=OutPat2);
  p <- p + geom_bar(aes(x=Facility,fill=!OOP))
  p <- p + scale_fill_manual("Payment:",values=c("darkgray","lightgreen"),labels=c("OOP","No-OOP"))
  p <- p + facet_grid(.~Group)
  p <- p + geom_text(data=BF,
                     aes(x=Facility,y=NOOP,
                         label=ifelse(!OOP,
                                      sprintf(Percent,fmt="%.1f%%"),
                                      ""))
                    ,vjust=-1,size=3)                   
  p <- p + labs(title=Cap)
  p <- p + theme(legend.position="none")
  print(p)
}
  ## BF <- ddply(OutPat2[,cv],.(Facility,Group),.fun= function(xx,col) {
  ##   c(Mean = smean.cl.boot(xx[,col])[1],
  ##     Lower= smean.cl.boot(xx[,col])[2],
  ##     Upper= smean.cl.boot(xx[,col])[3])},V
  ## )


PlotOutPOOPCost <- function(Var,Limit,Cap) {
  SkipFacility <- c("HV","NH","RH","TH","PC","PP")
  OutPat$OOP <- OutPat[,Var]!=0
  OutPat2 <- subset(OutPat, !(Facility %in% SkipFacility) & OOP & get(Var) < Limit)
  OutPat2$Facility <- as.character(factor(OutPat2$Facility))
  cv <- c("Facility","Group",Var)
  V <- Var
  bylist <- list(factor(OutPat2$Facility),factor(OutPat2$Group))
  BF <- aggregate(OutPat2[,V],by=bylist,FUN=smean.cl.boot)
  BF <- data.frame(Facility=BF$Group.1,
                   Group=BF$Group.2,
                   Mean=BF$x[,1],
                   Lower=BF$x[,2],
                   Upper=BF$x[,3])
  OutPat <- subset(OutPat, !(Facility %in% SkipFacility)
                   & OOP
                   & get(Var) < Limit)
  p <- ggplot(data=OutPat,aes(fill=Group))
  p <- p + geom_histogram( aes_string(x=Var))
  p <- p + facet_grid(Group~Facility)
  p <- p + geom_vline(data=BF,aes(xintercept=Mean))
  p <- p + geom_vline(data=BF,aes(xintercept=Upper),color="Orange")
  p <- p + geom_vline(data=BF,aes(xintercept=Lower),color="Orange")
  p <- p + geom_text(data=BF,aes(x=Mean,y=2,label=format(Mean,digits=0,big.mark=",",scientific=F)),vjust=-1,hjust=-.75,size=3)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p <- p + scale_x_continuous(name="Cost in LAK",labels=comma)
  p <- p + labs(title=paste(Cap,"\n<",format(Limit,big.mark=',',scientific=F)))
  p <- p + coord_flip()
  p <- p + theme(legend.position="none")
  p <- p + theme(plot.title = element_text(size = rel(.75)))
  return(p)
}


PlotInPOOP <- function(Var,Cap) {
  InPat$OOP  <- with(InPat,get(Var)!=0);
  InPat$OOP[is.na(InPat$OOP)] <- T
  InPat2 <- InPat[,c("Facility","Group","Total","OOP")]
  InPat2$Facility <- as.character(InPat2$Facility)
  SkipFacility <- c("HV","NH","RH","TH","PC","PP")
  InPat2 <- subset(InPat,!(as.character(Facility) %in% SkipFacility))
  InPat2$Facility <- factor(InPat2$Facility)
  BF <- as.data.frame(with(InPat2,
                           table(Facility,
                                 Group,
                                 OOP)))
  colnames(BF) <- c("Facility","Group","OOP","Count")
  BF$OOP <- as.logical(BF$OOP)
  BF$Percent <- 0
  BF$NOOP <- 0
  BF[BF$OOP,]$NOOP <- BF[ BF$OOP,]$Count;
  BF[!BF$OOP,]$NOOP <- BF[ BF$OOP,]$Count;
  BF[BF$OOP,]$Percent <-  100* BF[ BF$OOP,]$Count /(BF[BF$OOP,]$Count+BF[!BF$OOP,]$Count)
  BF[!BF$OOP,]$Percent <- 100* BF[!BF$OOP,]$Count /(BF[BF$OOP,]$Count+BF[!BF$OOP,]$Count)
  p <- ggplot(data=InPat2);
  p <- p + geom_bar(aes(x=Facility,fill=!OOP))
  p <- p + scale_fill_manual("Payment:",values=c("darkgray","lightgreen"),labels=c("OOP","No-OOP"))
  p <- p + facet_grid(.~Group)
  p <- p + geom_text(data=BF,
                     aes(x=Facility,y=NOOP,
                         label=ifelse(!OOP,
                                      sprintf(Percent,fmt="%.1f%%"),
                                      ""))
                    ,vjust=-1,size=3)                   
  p <- p + labs(title=Cap)
  p <- p + theme(legend.position="none")
  print(p)
}
  ## BF <- ddply(InPat2[,cv],.(Facility,Group),.fun= function(xx,col) {
  ##   c(Mean = smean.cl.boot(xx[,col])[1],
  ##     Lower= smean.cl.boot(xx[,col])[2],
  ##     Upper= smean.cl.boot(xx[,col])[3])},V
  ## )


PlotInPOOPCost <- function(Var,Limit,Cap) {
  SkipFacility <- c("HV","NH","RH","TH","PC","PP")
  InPat$OOP <- InPat[,Var]!=0
  InPat2 <- subset(InPat, !(Facility %in% SkipFacility) & OOP & get(Var) < Limit)
  InPat2$Facility <- as.character(factor(InPat2$Facility))
  cv <- c("Facility","Group",Var)
  V <- Var
  bylist <- list(factor(InPat2$Facility),factor(InPat2$Group))
  BF <- aggregate(InPat2[,V],by=bylist,FUN=smean.cl.boot)
  BF <- data.frame(Facility=BF$Group.1,
                   Group=BF$Group.2,
                   Mean=BF$x[,1],
                   Lower=BF$x[,2],
                   Upper=BF$x[,3])
  InPat <- subset(InPat, !(Facility %in% SkipFacility)
                   & OOP
                   & get(Var) < Limit)
  p <- ggplot(data=InPat,aes(fill=Group))
  p <- p + geom_histogram( aes_string(x=Var))
  p <- p + facet_grid(Group~Facility)
  p <- p + geom_vline(data=BF,aes(xintercept=Mean))
  p <- p + geom_vline(data=BF,aes(xintercept=Upper),color="Orange")
  p <- p + geom_vline(data=BF,aes(xintercept=Lower),color="Orange")
  p <- p + geom_text(data=BF,aes(x=Mean,y=2,label=format(Mean,digits=0,big.mark=",",scientific=F)),vjust=-1,hjust=-.75,size=3)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p <- p + scale_x_continuous(name="Cost in LAK",labels=comma)
  p <- p + labs(title=paste(Cap,"\n<",format(Limit,big.mark=',',scientific=F)))
  p <- p + coord_flip()
  p <- p + theme(legend.position="none")
  p <- p + theme(plot.title = element_text(size = rel(.75)))
  return(p)
}
