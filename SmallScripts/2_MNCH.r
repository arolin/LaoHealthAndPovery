## 1.2.1. Health/MNCH status of the mothers and children under 24 months
library("xtable")
## For all households (N=930) and indicative only for the 3 sub-groups (HEF Pre-id, HEF Geographic, Non-HEF) 
## Make analysis for 930

## Section 4: 	Mother <24 month child Maternal status & Free Maternity

## List of frequency tables for all questions

source("./Constants/Q4.r")
source("./Constants/Q5.r")

Mothers<-!is.na(lps[,"Mother_ID"])

NumMothers <-sapply (lgroups,function(X){sum(X&Mothers)})
write.csv(NumMothers,"./output/NumMothers.csv")
cat(print(xtable(as.data.frame(NumMothers),caption="Number of mothers with ID cards")),file="./tex/NumMothers.tex")

#Questions on ANC: 4.2, 4.3, 4.4

########################################
##4.2 - Have Mosquito net
lps$Mother_Mosquito_net_last_night <-factor(lps$Mother_Mosquito_net_last_night,labels=c("Normal","Treated","None"))
MNet <- table(lps$Mother_Mosquito_net_last_night[Mothers],lps$Group[Mothers])
All<-rowSums(MNet)
MNet<-cbind(MNet,All)
MNet<- t(t(MNet)/rowSums(t(MNet)))
 

fMNet<-sapply(MNet[,1:4],function(X){lapply(X,function(X){sprintf(X*100,fmt="%.1f%%")})})
dim(fMNet)=c(3,4);
colnames(fMNet)<-colnames(MNet)
rownames(fMNet)<-rownames(MNet)
fMNet
write.csv(fMNet,"./output/MosquitoNet.cvs")
cat(print(xtable(fMNet,caption="Mosquito Net Ussage")),file="./tex/MosquitoNet.tex")
##4.2 - Have Mosquito net
########################################

########################################
##4.3 Mother_have_a_pregnancy_monitoring_card


NiceTable<-function(V,lv=NULL,lb=NULL) {
  if (!is.null(lb)) {
    tv<-factor(lps[,V],levels=lv,labels=lb)
  }else {
    tv<-lps[,V];
  }
  NT <- table(tv[Mothers],lps$Group[Mothers])
  All<-rowSums(NT)
  NT<-cbind(NT,All)
  NT<- t(t(NT)/rowSums(t(NT)))
  fNT<-sapply(NT,function(X){lapply(X,function(X){sprintf(X*100,fmt="%.1f%%")})})
  dim(fNT)<-c(length(fNT)/4,4)
  colnames(fNT)<-colnames(NT)
  rownames(fNT)<-rownames(NT)
  r<-list(NT,fNT)
  names(r)<-c("tab","ftab")
  return(r)
}


PregCard <- NiceTable("Mother_have_a_pregnancy_monitoring_card",lv=c(1,2),lb=c("Yes","No"))
write.csv(PregCard$ftab,file="./output/Have_Pregnancy_Monitoring_Card.csv")
cat(print(xtable(PregCard$ftab,caption="Mother Has Pregnancy Monitoring Card")),file="./tex/PregCard.tex")


##4.4
Q4_4<-c("Mother_How_many_ANC_verbal_ANC1","Mother_How_many_ANC_verbal_ANC2","Mother_How_many_ANC_verbal_ANC3","Mother_How_many_ANC_verbal_ANC4","Mother_How_many_ANC_verbal_ANC5")
M4_4 <- !is.na(lps[,Q4_4[1]]);
sum(M4_4)
ANC <- sapply (Q4_4,function(X){sapply(lgroups,function(G){sum(lps[G&M4_4,X]==1)})})
ANC <- t(ANC);
colnames(ANC) <- names(lgroups)
rownames(ANC) <- sapply(1:5,function(X){paste("Had at least",X,"ANC visit")})

write.csv(ANC,file="./output/Q4_4_ANC_count.csv")
cat(print(xtable(ANC,caption="Number of AnteNatal Care Visis")),file="./tex/Q4_4_ANC.tex")

##Questions on TT: 4.7
Q4_7 <- "Mother_TT_immun_before_or_during_pregnancy"
TTPreg <- NiceTable(Q4_7,lv=c(1,2),lb=c("Yes","No"))
TTPreg$ftab <- rbind.data.frame(TTPreg$ftab,sapply(lgroups,function(G){as.character(sum(!is.na(lps[G,Q4_7])))}));
rownames(TTPreg$ftab)[3] <- "Num Resp"
TTPreg$ftab <- as.data.frame(TTPreg$ftab)
TTPreg$ftab
write.csv(as.matrix(TTPreg$ftab),file="./output/Q4_7_Mother_TT_immun_before_or_during_pregnancy.csv");
cat(print(xtable(TTPreg$ftab,caption="Mother TT immun before or during pregnancy")),file="./tex/Q4_7_Mother_TT_immun_before_or_during_pregnancy.tex")

##Q4_7 detail
Q4_7D <- c("Mother_TT_immun_Card1","Mother_TT_immun_Card2","Mother_TT_immun_Card3","Mother_TT_immun_Card4","Mother_TT_immun_Card5")
Q4_7Ds <- !is.na(lps[,Q4_7D[1]])
TTPregCard <- t(sapply(Q4_7D,function(X){sapply(lgroups,function(G){sum(lps[G,X]==1,na.rm=T)})}))


write.csv(TTPregCard,file="./output/Q4_7_Detail_TT_Pregnancy_Card.csv")
cat(print(xtable(TTPregCard,caption="Pregnancy TT Imunization Card Records")),file="./tex/Q4_7_Detail_PregCard.tex")


##Questions on delivery by SBA: 4.16
Q4_16 <- "Mother_Birth_facility_type"
Q4_16_codes  <- c("Private Hospital","District Hospital","Heath Clinic","Private clinic","Home") ##,"Other","In the Forest","Hospital in Thailand")
Q4_16s<- !is.na(lps[,Q4_16])

##all mothers respond!
sum(Q4_16s)
Q4_16Others  <- lps[,Q4_16]==6;
##lps[Q4_16Others,Q4_16] <- lps[Q4_16Others,"Mother_Birth_facility_other_specify"]
sum(Q4_16Others)
#No others!
lps[,Q4_16]<-factor(lps[,Q4_16],labels=Q4_16_codes)
BirthFacility <- t(sapply(Q4_16_codes,function(X){sapply(lgroups,function(G){sum(lps[G,Q4_16]==X,na.rm=T)})}))
colSums(BirthFacility)
BirthFacility <- PercentifyTable(BirthFacility)
write.csv(BirthFacility,file="./output/Q4_16_BirthFacility.csv")
cat(print(xtable(BirthFacility,caption=("Reported Delivery Facility"))),file="./tex/Q4_16_BirthFacility.tex")



##Questions on satisfaction: 4.31
Q4_31 <- "Mother_Birth_Wait_long"
Q4_31s <- !is.na(lps[,Q4_31])
lps[,Q4_31] <- factor(lps[,Q4_31],labels=c("Yes","No"));
sum(Q4_31s)
##All responded
WaitLong  <- sapply(lgroups,function(G){sapply(c("Yes","No"),function(X){sum(lps[G,Q4_31]==X,na.rm=T)})})
WaitLongP <- PercentifyTable(WaitLong)

write.csv(WaitLongP,file="./output/Q4_31_WaitLong.csv");
cat(print(xtable(WaitLongP,caption=("Waited Long"))),file="./tex/Q4_32_WaitLong.tex")

Q4_31_time <- "Mother_Birth_Wait_time"
Q4_31_times = !is.na(lps[,Q4_31_time]) & lps[,Q4_31_time]!=0
png(filename="./tex/Q4_31_waittimes.png")
hist(lps[Q4_31_times,Q4_31_time],breaks=150)
dev.off()

## Questions on child fully immunized: 5.6
##   5.5 Do you have the monitoring (yellow) card or booklet...
##   q5_5
## Mother_Child_Have_monitoring_booklet
Q5_5<-"Mother_Child_Have_monitoring_booklet"
Q5_5s<-!is.na(lps[,Q5_5])
sum(Q5_5s,na.rm=T)
lps[,Q5_5] <- factor(lps[,Q5_5],labels=c("Yes","No"))
lps[Q5_5s,Q5_5] 
HaveBooklet  <- sapply(lgroups,function(G){sapply(c("Yes","No"),function(X){sum(lps[G,Q5_5]==X,na.rm=T)})})
HaveBookletP <- PercentifyTable(HaveBooklet)
write.csv(HaveBookletP,file="./output/Q5_5_Mother_Child_Have_monitoring_booklet.csv")
cat(print(xtable(HaveBookletP,caption="Mothers has child monitoring booklet")),file="./tex/Q5_5_Mother_Child_Have_monitoring_booklet.tex")



## Questions on satisfaction: 5.17, 5.22, 5.27
Q5_17 <- "Mother_Child_Last_diarhea_Received_Helpful"
Q5_22 <- "Mother_Child_Last_respiratory_First_treatment_Helpful"
Q5_27 <- "Mother_Child_Last_fever_First_treatment_Helpful"
q <- c(Q5_17,  Q5_22,  Q5_27)
qs <-  !is.na(lps[,q])

Q5Helpfull <-  t(sapply(q,function(Q){sapply(lgroups,function(G){sum(!is.na(lps[G,Q]))})}))
SaveTables(Q5Helpfull,"Q5_17.22.27_Num_Reporting","Num Reporting on Literature Helpfullness by Group")
Q5Helpfull

Q5Helpfull <- t(sapply(q,function(Q){sapply(lgroups,function(G){sum(lps[G,Q]==1,na.rm=T)/sum(!is.na(lps[G,Q]))})}))
Q5HelpfullP <- ToPercents(Q5Helpful)

SaveTables(Q5HelpfulP,"Q5_17.22.27_Helpfull","Percent Reporting Literature was Helpfull by Group")
