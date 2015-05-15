## 1.2.1. Health/MNCH status of the mothers and children under 24 months
library("xtable")
## For all households (N=930) and indicative only for the 3 sub-groups (HEF Pre-id, HEF Geographic, Non-HEF) 
## Make analysis for 930

## Section 4: 	Mother <24 month child Maternal status & Free Maternity

## List of frequency tables for all questions

source("./Constants/Q4.r")
source("./Constants/Q5.r")

qCheck(q4,"4")
qCheck(q5,"5")

Mothers<-!is.na(lps[,"Mother_ID"])

NumMothers <-sapply (lgroups,function(X){sum(X&Mothers)})
SaveTables(as.data.frame(NumMothers),"Q4_NumMothers.csv","Num of mothers with ID cards")
NumMothers


haveCard <- lps[,"Mother_ANC_Card_1_Date"]!=""
lagroups <- list(lgroups[[1]],lgroups[[1]]&haveCard,lgroups[[2]],lgroups[[2]]&haveCard,lgroups[[3]],lgroups[[3]]&haveCard,lgroups[[4]])
names(lagroups) <- c("PreID","PreID\n+card","GeoID","GeoID\n+card","NoAssist","NoAssist\n+card","All")
sapply(lagroups,sum)
qCheck2(q4,"4_Card_NoCard",lagroups)

#Questions on ANC: 4.2, 4.3, 4.4

########################################
##4.2 - Have Mosquito net
lps$Mosquito_net_last_night <-factor(lps$Mother_Mosquito_net_last_night,labels=c("Normal","Treated","None"))
MNet <- table(lps$Mosquito_net_last_night[Mothers],lps$Group[Mothers])
All<-rowSums(MNet)
MNet<-cbind(MNet,All)
MNet<- t(t(MNet)/rowSums(t(MNet)))

 

fMNet<-sapply(MNet[,1:4],function(X){lapply(X,function(X){sprintf(X*100,fmt="%.1f%%")})})
dim(fMNet)=c(3,4);
colnames(fMNet)<-colnames(MNet)
rownames(fMNet)<-rownames(MNet)
fMNet
SaveTables(fMNet,"Q4_2_MosquitoNets","Mosquito net usage reports");
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
SaveTables(PregCard$ftab,"Q4_3_Have_Pregnancy_Monitoring_Card","Percent of mothers with pregnancy monitoring cards")
PregCard
HaveANCCard   <- lps[,"Mother_have_a_pregnancy_monitoring_card"]==1
sapply(lgroups,function(G){sum(HaveANCCard[Mothers&G],na.rm=T)})

##4.4
Q4_4<-c("Mother_How_many_ANC_verbal_ANC1","Mother_How_many_ANC_verbal_ANC2","Mother_How_many_ANC_verbal_ANC3","Mother_How_many_ANC_verbal_ANC4","Mother_How_many_ANC_verbal_ANC5")
M4_4 <- !is.na(lps[,Q4_4[1]]);
sum(M4_4)
ANC <- sapply (Q4_4,function(X){sapply(lgroups,function(G){sum(lps[G&M4_4,X]==1)})})
ANC <- t(ANC);
colnames(ANC) <- names(lgroups)
rownames(ANC) <- sapply(1:5,function(X){paste("Had at least",X,"ANC visit")})
SaveTables(ANC,"Q4_4_ANC_verbal_count","Number of AnteNatal Care Visis")
ANC

sum(!is.na(lps[,"Mother_How_many_ANC_verbal_ANC1"]))
sum(lps[,"Mother_ANC_Card_1_Date"]!="")
q4_4_card <- c("Mother_ANC_Card_1_Date","Mother_ANC_Card_2_Date","Mother_ANC_Card_3_Date","Mother_ANC_Card_4_Date","Mother_ANC_Card_5_Date")

ANCCard <- sapply(lgroups,function(G){sapply(q4_4_card,function(N){sum(lps[G,N]!="" & lps[G,N]!="00.00.0000",na.rm=T)})})
sapply(lgroups,function(G){sapply(q4_4_card,function(N){sum(is.na(lps[G,"Mother_How_many_ANC_verbal_ANC1"]) & lps[G,N]!="" & lps[G,N]!="00.00.0000",na.rm=T)})})
SaveTables(ANCCard,"Q4_4_ANC_Card_count","Count of ANC visits by card")

q4_2_1 <- "Mother_been_to_ANC"
sapply(lgroups,function(G){sum(lps[G,q4_2_1]==1,na.rm=T)})

haveCard <- lps[,q4_4_card]!="" & lps[,q4_4_card]!="00.00.0000"
haveCard <- haveCard[,1]
haveCard[is.na(haveCard)]=F
q4_3 <- "Mother_have_a_pregnancy_monitoring_card"
sapply(lgroups,function(G){sum(lps[G,q4_3]==1,na.rm=T)})

sum(haveCard&Mothers)
sum(Mothers)
lps[which(haveCard[,1])[1],q4_4_card]
sum(!is.na(lps[,"Mother_How_many_ANC_verbal_ANC1"]))
##Q4_5
q4_5 <- c("Mother_ANC_Where_Provincial_Hospital","Mother_ANC_Where_District_Hospital","Mother_ANC_Where_Health_centre","Mother_ANC_Where_Village_mobile_clinic","Mother_ANC_Where_Private_clinic","Mother_ANC_Where_TBA","Mother_ANC_Where_Other")
Q4_5_ANCLocations <- sapply(lgroups,function(G){sapply(q4_5,function(L){sum(!is.na(lps[G,L]))})})
write.csv(Q4_5_ANCLocations,file="./output/Q4_5_ANCLocations.csv")
Q4_5_ANCLocations_Card <- sapply(lagroups,function(G){sapply(q4_5,function(L){sum(!is.na(lps[G,L]))})})
Q4_5_ANCLocations_Card
write.csv(Q4_5_ANCLocations_Card,file="./output/Q4_5_ANCLocations_Card.csv")





##Questions on TT: 4.7
Q4_7 <- "Mother_TT_immun_before_or_during_pregnancy"
TTPreg <- NiceTable(Q4_7,lv=c(1,2),lb=c("Yes","No"))
TTPreg$ftab <- rbind.data.frame(TTPreg$ftab,sapply(lgroups,function(G){as.character(sum(!is.na(lps[G,Q4_7])))}));
rownames(TTPreg$ftab)[3] <- "Num Resp"
TTPreg$ftab <- as.data.frame(TTPreg$ftab)
TTPreg$ftab
SaveTables(as.matrix(TTPreg$ftab),"Q4_7_Mother_TT_immun_before_or_during_pregnancy","Mother TT immun before or during pregnancy");

##Q4_7 detail
Q4_7D <- c("Mother_TT_immun_Card1","Mother_TT_immun_Card2","Mother_TT_immun_Card3","Mother_TT_immun_Card4","Mother_TT_immun_Card5")
Q4_7Ds <- !is.na(lps[,Q4_7D[1]])
TTPregCard <- t(sapply(Q4_7D,function(X){sapply(lgroups,function(G){sum(lps[G,X]==1,na.rm=T)})}))
SaveTables(TTPregCard,"Q4_7_Detail_TT_Pregnancy_Card","Pregnancy TT Imunization Card Records");
t(t(TTPregCard)/NumMothers)
PercentifyTable(TTPregCard)


for(i in 1:5) {TTPregCard2 <- rbind(NumMothers-TTPregCard[i,],TTPregCard[i,]);print(chisq.test(TTPregCard2[,1:3])$p.value);}


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
BirthFacility <- t(sapply(Q4_16_codes,function(X){sapply(lgroups,function(G){sum(lps[G&haveCard,Q4_16]==X,na.rm=T)})}))
BirthFacility


    colSums(BirthFacility)
BirthFacility <- PercentifyTable(BirthFacility)
SaveTables(BirthFacility,"Q4_16_BirthFacility","Reported Delivery Facility Type")
BirthFacility


##Questions on satisfaction: 4.31
Q4_31 <- "Mother_Birth_Wait_long"
Q4_31s <- !is.na(lps[,Q4_31])
lps[,Q4_31] <- factor(lps[,Q4_31],labels=c("Yes","No"));
sum(Q4_31s)
##All responded
WaitLong  <- sapply(lgroups,function(G){sapply(c("Yes","No"),function(X){sum(lps[G,Q4_31]==X,na.rm=T)})})
WaitLong

WaitLongP <- PercentifyTable(WaitLong)
SaveTables(WaitLongP,"Q4_31_WaitLong","Waited Long")

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
SaveTables(HaveBookletP,"Q5_5_Mother_Child_Have_monitoring_booklet","Mothers has child monitoring booklet");



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
Q5HelpfullP <- ToPercents(Q5Helpfull)

SaveTables(Q5HelpfullP,"Q5_17.22.27_Helpfull","Percent Reporting Literature was Helpfull by Group")

gM <- sapply (lps$Mother_ID[Mothers],function(M){paste("q2_2_",M,sep="")})
lpsM <- lps[Mothers,]
MotherIndv <- sapply(1:sum(Mothers),function(i){which(IndiHealth$SRow==lpsM$Serial[i])[lpsM$Mother_ID[i]]})
IndiHealth$Gender[MotherIndv]
IndiHealth$Age[MotherIndv]
mean(IndiHealth$Age[MotherIndv])
median(IndiHealth$Age[MotherIndv])

png("./tex/HistMaternalAge.png")
hist(IndiHealth$Age[MotherIndv],80,xlab="Age",ylab="Num of Mothers",main="Distribution of Mothers' Ages")
dev.off()
min(IndiHealth$Age[MotherIndv])


qM <- sapply(gM,function(N){length(which(names(lps)==N))!=1})

nullM <- sapply(1:length(gM),function(i){is.null(lpsM[i,gM[i]])})
sapply(c(1:length(gM))[!nullM],function(i){lpsM[i,gM[i]]})


