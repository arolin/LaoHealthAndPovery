if (!exists("MDM")) {
  MDM =F
}

format(rbind(OutP_CU5_mean=
        with(subset(OutPat,Age<5),tapply(Total,list(Group=Group),FUN=mean)),
      OutP_CU5_median=
        with(subset(OutPat,Age<5),tapply(Total,list(Group=Group),FUN=median)),
      OutP_mean=
        with(subset(OutPat,Age>=5),tapply(Total,list(Group=Group),FUN=mean)),
      OutP_median=
        with(subset(OutPat,Age>=5),tapply(Total,list(Group=Group),FUN=median)),
      InP_CU5_mean=
        with(subset(InPat,Age<5),tapply(Total,list(Group=Group),FUN=mean)),
      InP_CU5_median=
        with(subset(InPat,Age<5),tapply(Total,list(Group=Group),FUN=median)),
      InP_mean=
        with(subset(InPat,Age>=5),tapply(Total,list(Group=Group),FUN=mean)),
      InP_median=
        with(subset(InPat,Age>=5),tapply(Total,list(Group=Group),FUN=median))),digits=0,scientific=F)

OverallCosts <- rbind(
    OutP_CU5=
(with(subset(OutPat,Total<500000 & Age<5),tapply(Total,list(Group=Group),FUN=mean)) +
   with(subset(OutPat,Total<500000 & Age<5),tapply(Total,list(Group=Group),FUN=median)))/2,
   OutP=
(with(subset(OutPat,Total<500000 & Age>=5),tapply(Total,list(Group=Group),FUN=mean)) +
   with(subset(OutPat,Total<500000 & Age>=5),tapply(Total,list(Group=Group),FUN=median)))/2,
   InP_CU5=
(with(subset(InPat,Total<1000000 & Age<5),tapply(Total,list(Group=Group),FUN=mean)) +
   with(subset(InPat,Total<1000000 & Age<5),tapply(Total,list(Group=Group),FUN=median)))/2,
   InP=
(with(subset(InPat,Total<1000000 &Age>=5),tapply(Total,list(Group=Group),FUN=mean)) + 
   with(subset(InPat,Total<1000000 & Age>=5),tapply(Total,list(Group=Group),FUN=median)))/2)

if (MDM){
  print(xtable(format(OverallCosts,big.mark=",",digits=0,scientific=F),"Overall Cost Estimates"),type='html')
}else{
  print(format(OverallCosts,big.mark=",",digits=0,scientific=F))
}


CountOverall <- format(rbind(
                           OutP_CU5=
                             with(subset(OutPat,Total<50000 & Age<5),tapply(Total,list(Group=Group),FUN=length)),
                           OutP=
                             with(subset(OutPat,Total<50000 & Age>=5),tapply(Total,list(Group=Group),FUN=length)),
                           InP_CU5=
                             with(subset(InPat,Total<100000 & Age<5),tapply(Total,list(Group=Group),FUN=length)),
                           InP=
                             with(subset(InPat,Total<100000 & Age>=5),tapply(Total,list(Group=Group),FUN=length)))
                      ,digits=0,scientific=F
                       )

if(MDM) {
  print(xtable(CountOverall,"Number of costs in estimate"),type='html')
}else{
  print(CountOverall)
}

write.csv(OverallCosts,file="./output/OverallCosts.csv")

overlay <- cbind.data.frame(t(OverallCosts),digits,Group=names(OverallCosts[2,]))
overlayT <- cbind.data.frame(t(format(OverallCosts,digits=0,big.mark=',',scientific=F)),Group=names(OverallCosts[2,]))


pmod <- function(p,Mod,tit) {
  p <- p + geom_histogram(aes(fill=Group))
  p <- p + geom_vline(data=overlay,
                      aes_string(xintercept=Mod))
  p <- p + coord_flip()
  p <- p + geom_text(data=overlayT,aes_string(x=0,y=0,label =Mod), size=3,angle = 0, vjust=.5,hjust = -1, color="black")
  p <- p + facet_grid(.~Group)
  p <- p + scale_x_continuous(name="Cost in LAK",labels=comma)
  p <- p + labs(title=tit)
  print(p)
}
pmod(ggplot(data=subset(InPat,Total<1000000  & Age>=5),aes(x=Total)),"InP","Inpatient Cost Model")
pmod(ggplot(data=subset(InPat,Total<1000000  & Age<5),aes(x=Total)),"InP_CU5","Inpatient CU5 Cost Model")
pmod(ggplot(data=subset(OutPat,Total<500000  & Age>=5),aes(x=Total)),"OutP","Outpatient Cost Model")
pmod(ggplot(data=subset(OutPat,Total<500000  & Age<5),aes(x=Total)),"OutP_CU5","Outpatient CU5 Cost Model")






     
