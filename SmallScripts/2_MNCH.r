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

lgNames <-  interleave(names(lgroups),rep("",length(lgroups)))

haveCard <- lps[,"Mother_ANC_Card_1_Date"]!=""
lagroups <- list(lgroups[[1]],lgroups[[1]]&haveCard,lgroups[[2]],lgroups[[2]]&haveCard,lgroups[[3]],lgroups[[3]]&haveCard,lgroups[[4]])
names(lagroups) <- c("PreID","PreID\n+card","GeoID","GeoID\n+card","NoAssist","NoAssist\n+card","All")
sapply(lagroups,sum)
qCheck2(q4,"4_Card_NoCard",lagroups)



levels(factor(lps$Village[haveCard]))
levels(factor(lps$Village[Mothers]))
levels(factor(lps$Village[Mothers&!haveCard]))

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
ANCCard

ALLANC <- rbind(interleave(NumMothers,rep("",length(NumMothers))),
                interleave2d (ANCCard + ANC,FmtPer( t(t(ANCCard + ANC)/NumMothers))))
ALLANC
colnames(ALLANC) <- lgNames
rownames(ALLANC) <- c("Num Mothers",
                      "At Least 1 ANC Visits",
                      "At Least 2 ANC Visits",
                      "At Least 3 ANC Visits",
                      "At Least 4 ANC Visits",
                      "At Least 5 ANC Visits")
ALLANC
SaveTables(ALLANC,"Q4_4_Verbal_and_Card","ANC visit rates for verbal and card verified responsed")


VerbalANC <- rbind(interleave(NumMothers,rep("",length(NumMothers))),
                interleave2d (ANC,FmtPer( t(t(ANC)/NumMothers))))
colnames(VerbalANC) <- lgNames
rownames(VerbalANC) <- c("Num Mothers",
                      "At Least 1 ANC Visits",
                      "At Least 2 ANC Visits",
                      "At Least 3 ANC Visits",
                      "At Least 4 ANC Visits",
                      "At Least 5 ANC Visits")
VerbalANC
SaveTables(VerbalANC,"Q4_4_Verbal","ANC visit rates for verbal only")

CardANC <- rbind(interleave(NumMothers,rep("",length(NumMothers))),
                interleave2d (ANCCard,FmtPer( t(t( ANCCard)/NumMothers))))
colnames(CardANC) <- lgNames 
rownames(CardANC) <- c("Num Mothers",
                      "At Least 1 ANC Visits",
                      "At Least 2 ANC Visits",
                      "At Least 3 ANC Visits",
                      "At Least 4 ANC Visits",
                      "At Least 5 ANC Visits")
CardANC
SaveTables(CardANC,"Q4_4_Card","ANC visit rates for card verified responses")









q4_2_1 <- "Mother_been_to_ANC"
sapply(lgroups,function(G){sum(lps[G,q4_2_1]==1,na.rm=T)})

haveCard <- lps[,q4_4_card]!="" & lps[,q4_4_card]!="00.00.0000"
haveCard <- haveCard[,1]
haveCard[is.na(haveCard)]=F
q4_3 <- "Mother_have_a_pregnancy_monitoring_card"
sapply(lgroups,function(G){sum(lps[G,q4_3]==1,na.rm=T)})

sum(haveCard&Mothers)
sum(Mothers)
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
sapply(lgroups,function(G){sum(lps[G,Q4_7]==1,na.rm=T)})
SaveTables(as.matrix(TTPreg$ftab),"Q4_7_Mother_TT_immun_before_or_during_pregnancy","Mother TT immun before or during pregnancy");

##Q4_7 detail
Q4_7D <- c("Mother_TT_immun_Card1","Mother_TT_immun_Card2","Mother_TT_immun_Card3","Mother_TT_immun_Card4","Mother_TT_immun_Card5")
Q4_7Ds <- !is.na(lps[,Q4_7D[1]])
TTPregCard <- t(sapply(Q4_7D,function(X){sapply(lgroups,function(G){sum(lps[G,X]==1,na.rm=T)})}))
SaveTables(TTPregCard,"Q4_7_Detail_TT_Pregnancy_Card","Pregnancy TT Imunization Card Records");
t(t(TTPregCard)/NumMothers)
PercentifyTable(TTPregCard)
TTPregCard

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

q4_16 <- "Mother_Birth_facility_type"
q4_16Fact <- factor(lps[,q4_16],levels=CodeLables[["q4_16"]][1,],labels=c("PH","DH","HC","PC","Home","Other"))
levels(q4_16Fact)
sapply(lgroups,function(G)(table(q4_16Fact[G])))
q4_18 <- "Mother_Birth_Cost_delivery"
q4_18s <- !is.na(lps[,q4_18])
paid <- lps[,q4_18]!=0

PaidForDelivery <- sapply(lgroups,function(G)(table(q4_16Fact[G&paid])))
PaidForDelivery2 <- sapply(lagroups,function(G)(table(q4_16Fact[G&paid])))
PaidForDelivery
PaidForDelivery2
write.csv(PaidForDelivery2,"./output/Q4_18_&4_16_Paid_for_delivery_by_care_center.csv")





q8 <- names(lps)[which(names(lps)=="HEF_Knowledge")[1]:which(names(lps)=="Adult_Admitted_Wait_reason")[1]]
qCheck2(q8,"8")

##Q8.8 Analysis of HEF HHs who had to pay for admissions by type of facility (PH, DH, HC): PH =18/55, DH=30/102, HC=6/35
q8_8 <- "Adult_Admitted_Cost_OOP"
admitLocTypes <- c("Provincial_Hospital","District_Hospital","Health_centre","Private_clinic","Other")
admitLocs <- paste("Adult_Admitted_Location_",admitLocTypes,sep="")
hgroups <- lgroups[c(1,2,4)]
AdmittedOOP <- sapply(hgroups,function(G){sapply(admitLocs,function(L){admits <- !is.na(lps[,L]);sum(lps[admits&G,q8_8]==1,na.rm=T)})})

sapply(admitLocs,function(L){sum(!is.na(lps[,L]))})


Admitted    <- sapply(hgroups,function(G){sapply(admitLocs,function(L){sum(!is.na(lps[G,L]))})})
AdmittedOOP <- rbind(AdmittedOOP,colSums(AdmittedOOP))
AdmittedOOP
Admitted <- rbind(Admitted,colSums(Admitted))
Admitted
Q8_8Tab <- interleave2d(AdmittedOOP,FmtPer(AdmittedOOP/Admitted))
colnames(Q8_8Tab) <- c("PreID","%OOP","GeoID","%OOP","All","%OOP")
rownames(Q8_8Tab) <- c(admitLocTypes,"Total")
Q8_8Tab
SaveTables(Q8_8Tab,"Q8_8_OOP_Rates_by_admission_facility","OOP Payement rates by addmission facility type.")


q8_9 <- "Adult_Admitted_Food_allowance"
q8_9s  <- !is.na(lps[,q8_9])
lps[q8_9s,"Adult_Admitted_Food_allowance"]
AdmittedFood <- sapply(hgroups,function(G){sapply(admitLocs,function(L){admits <- !is.na(lps[,L]);sum(lps[admits&G,q8_9]==2,na.rm=T)})})
AdmittedFood <- rbind(AdmittedFood,colSums(AdmittedFood))
AdmittedFood
Admitted
Q8_9Tab <- interleave2d(AdmittedFood,FmtPer(AdmittedFood/Admitted))
colnames(Q8_9Tab) <- c("PreID","%Got Allowance","GeoID","%Got Allowance","All","%Got Allowance")
rownames(Q8_9Tab) <- c(admitLocTypes,"Total")
Q8_9Tab
SaveTables(Q8_9Tab,"Q8_9_Food_allowaance_Rates_by_admission_facility","Food allowance rates by addmission facility type.")


 AdmittedFood <- sapply(hgroups,function(G){sapply(admitLocs,function(L){admits <- !is.na(lps[,L]);sum(lps[admits&G,q8_9]==2,na.rm=T)})})
AdmittedFood <- rbind(AdmittedFood,colSums(AdmittedFood))
AdmittedFood
Admitted
Q8_9TabNo <- interleave2d(Admitted-AdmittedFood,FmtPer((Admitted-AdmittedFood)/Admitted))
colnames(Q8_9TabNo) <- c("PreID","%No Allowance","GeoID","%No Allowance","All","%No Allowance")
rownames(Q8_9TabNo) <- c(admitLocTypes,"Total")
Q8_9TabNo
SaveTables(Q8_9TabNo,"Q8_9_No_food_allowance_Rates_by_admission_facility","Food allowance rates by addmission facility type.")



Codes[which(Codes$Question=="q8_15"),]
q8_15 <- "Adult_Admitted_Outside_HEF_3km_radius"
q8_15s <- !is.na(lps[,q8_15])
lps[q8_15s,q8_15]
AdmittedTransp <- sapply(hgroups,function(G){
                           sapply(admitLocs,function(L){
                                    admits <- !is.na(lps[,L]);
                                    sum(lps[admits&G,q8_15]==2,na.rm=T)})})
AdmittedTransp <- rbind(AdmittedTransp,colSums(AdmittedTransp))
AdmittedTransp
AdmittedTranspD <- sapply(hgroups,function(G){
                           sapply(admitLocs,function(L){
                                    admits <- !is.na(lps[,L]);
                                    sum(lps[admits&G,q8_15]==1 | lps[admits&G,q8_15]==2,na.rm=T)})})
class(AdmittedTranspD[1,])
AdmittedTranspD <- rbind(AdmittedTranspD,colSums(AdmittedTranspD))
AdmittedTranspD
Q8_15Tab <- interleave2d(AdmittedTransp,FmtPer(AdmittedTransp/AdmittedTranspD))
colnames(Q8_15Tab) <- c("PreID","%Got Allowance","GeoID","%Got Allowance","All","%Got Allowance")
rownames(Q8_15Tab) <- c(admitLocTypes,"Total")
Q8_15Tab
SaveTables(Q8_15Tab,"Q8_15_Transportation_allowance_Rates_by_admission_facility","Transportation allowance rates by addmission facility type.")


MF2 <- paste("HH_Illness_2_",admitLocTypes[1:4],"_cost_Medical_Fee",sep="")
MF2[3] <-    "HH_Illness_2_Health_Center_cost_Medical_Fee"
MF2[4] <-    "HH_Illness_2_Private_Clinic_cost_Medical_Fee"

MF3 <- paste("HH_Illness_3_",admitLocTypes[1:4],"_cost_Medical_Fee",sep="")
MF3[3] <-    "HH_Illness_3_Health_Center_cost_Medical_Fee"
MF3[4] <-    "HH_Illness_3_Private_Clinic_cost_Medical_Fee"





MF2 <- paste("HH_Illness_2_",admitLocTypes[1:4],"_cost_Medical_Fee",sep="")
MF2[3] <-    "HH_Illness_2_Health_Center_cost_Medical_Fee"
MF2[4] <-    "HH_Illness_2_Private_Clinic_cost_Medical_Fee"

MF3 <- paste("HH_Illness_3_",admitLocTypes[1:4],"_cost_Medical_Fee",sep="")
MF3[3] <-    "HH_Illness_3_Health_Center_cost_Medical_Fee"
MF3[4] <-    "HH_Illness_3_Private_Clinic_cost_Medical_Fee"


q8_8s <- !is.na(lps[,q8_8])
OOPs <- lps[ q8_8s,q8_8]==1
Q8_8CheckTab <- c()
Q8_8CheckTab  <- t(sapply(MF2,function(F){
                            Zeros   <- lps[OOPs,F]==0;
                            DKs <- lps[OOPs,F]==98 |lps[OOPs,F]==99
                            Paid <- !is.na(lps[OOPs,F]) & !Zeros &!DKs
                            c(Zero = sum(Zeros,na.rm=T),
                                 DK = sum(DKs,na.rm=T),
                                 Paid = sum(Paid))}))
rownames(Q8_8CheckTab) <- admitLocTypes[1:4]
Q8_8CheckTab <- cbind(Q8_8CheckTab,Total=rowSums(Q8_8CheckTab))
Q8_8CheckTab <- rbind(Q8_8CheckTab,Total=colSums(Q8_8CheckTab))
Q8_8CheckTab
## SaveTables(Q8_8CheckTab,"Q8_8_x_Q3_7_Medical_fee_report_for_non_OOP","")
## SaveTables(Q8_8CheckTab,"Q8_8_x_Q3_7_Medical_fee_report_for_OOP","")
##SaveTables(Q8_8CheckTab,"Q8_8_x_Q3_5_Medical_fee_report_for_non_OOP","")
 SaveTables(Q8_8CheckTab,"Q8_8_x_Q3_5_Medical_fee_report_for_OOP","")



HC <- c("National_Hospital","Provincial_Hospital","Health_Center","Private_Clinic")
Cost <- paste("HH_Illness_2_",HC,"_cost_Overall_average",sep="")

q8_8s <- !is.na(lps[,q8_8])
OOPs <- lps[ q8_8s,q8_8]==1
Q8_8CheckTab <- c()
Q8_8CheckTab  <- t(sapply(Cost,function(F){
                            Zeros   <- lps[OOPs,F]==0;
                            DKs <- lps[OOPs,F]==98 |lps[OOPs,F]==99
                            Paid <- !is.na(lps[OOPs,F]) & !Zeros &!DKs
                            c(Zero = sum(Zeros,na.rm=T),
                                 DK = sum(DKs,na.rm=T),
                                 Paid = sum(Paid))}))
rownames(Q8_8CheckTab) <- admitLocTypes[1:4]
Q8_8CheckTab <- cbind(Q8_8CheckTab,Total=rowSums(Q8_8CheckTab))
Q8_8CheckTab <- rbind(Q8_8CheckTab,Total=colSums(Q8_8CheckTab))
Q8_8CheckTab

q8_9 <- c("Adult_Admitted_Cost_Drugs_supplies_in_hospital_LAK","Adult_Admitted_Cost_Drugs_supplies_outtside_hospital_LAK","Adult_Admitted_Cost_Service_charge_LAK","Adult_Admitted_Cost_Other_LAK","Adult_Admitted_Cost_Average_LAK")



q4_20 <- "Mother_Birth_Hospital_Food_allowance"
q4_20s <- !is.na( lps[,q4_20])
PHB <- lps$Mother_Birth_facility_type==1
PHB[is.na(PHB)]=F
DHB  <- lps$Mother_Birth_facility_type==2
DHB[is.na(DHB)]=F
DHB_G <- sapply(lgroups,function(G){sum(DHB[G],na.rm=T)})
DHB_G
FA_DHB <- sapply(lgroups,function(G){sum(lps[G&DHB,q4_20]==1,na.rm=T)})
FA_DHB
PHB_G <- sapply(lgroups,function(G){sum(PHB[G],na.rm=T)})
PHB_G
FA_PHB <- sapply(lgroups,function(G){sum(lps[G&PHB,q4_20]==1,na.rm=T)})
FA_PHB
Q4_20Tab <- rbind(interleave(DHB_G,rep("",4)),
                 interleave(FA_DHB,FmtPer(FA_DHB/DHB_G)),
                 interleave(PHB_G,rep("",4)),
                 interleave(FA_PHB,FmtPer(FA_PHB/PHB_G)))
rownames(Q4_20Tab) <- c("District Hospital Births",
                       "Food allowance recieved(%DH Births)",
                       "Provincial Hospital Births",
                       "Food allowance recieved(%PH Births)")
SaveTables(Q4_20Tab,"Q4_20_Food_allowances_for_hospital_births","")
Q4_20Tab

q4_25 <- "Mother_Birth_Delivery_was_at_hospital_more_3k"
q4_25s <- !is.na(lps[,q4_25])
PHB_T <- sapply(lgroups,function(G){sum(lps[PHB&G,q4_20]!=3,na.rm=T)})
PHB_A <- sapply(lgroups,function(G){sum(lps[PHB&G,q4_20]==1,na.rm=T)})
DHB_T <- sapply(lgroups,function(G){sum(lps[DHB&G,q4_20]!=3,na.rm=T)})
DHB_A <- sapply(lgroups,function(G){sum(lps[DHB&G,q4_20]==1,na.rm=T)})
Q4_25Tab <- rbind(interleave(DHB_T,rep("",length(lgroups))),
                  interleave(DHB_A,FmtPer(DHB_A/DHB_T)),
                  interleave(PHB_T,rep("",length(lgroups))),
                  interleave(PHB_A,FmtPer(PHB_A/PHB_T)))
rownames(Q4_25Tab) <- c("District Hospital Births >3km",
                       "Transportation allowance received(%DH Births >3k)",
                       "Provincial Hospital Births",
                       "Transportation allowance received(%PH Births >3k)")
Q4_25Tab
SaveTables(Q4_25Tab,"Q4_25_Tranposrtation_allowance_for_births_>3k","")


q4_34 <- "Mother_Breast_feed_time"
q4_34C <- Codes[which(Codes$Question=="q4_34"),]
q4_34A <- sapply(lgroups,function(G){tabulate(lps[G&Mothers,q4_34])})
q4_34A <- interleave2d(q4_34A,FmtPer(t(t(q4_34A)/NumMothers)))
q4_34A <- rbind(interleave(NumMothers,rep("",length(lgroups))),q4_34A)
rownames(q4_34A) <- c("Num Mothers",q4_34C$Label)
colnames(q4_34A) <- lgNames
q4_34A
SaveTables(q4_34A,"Q4_34_Mother_Breast_feed_time","Time to breast feeding")

q4_35 <- "Mother_Time_retur_after_delivery"
Q4_35Tab <- c()
lnq <- c("PH","DH","HC","PC","Home","Other")
rnms <- c();
for(Ty in c(1,2,3,4,6)) {
  a <- sapply(lgroups,function(G){
                sapply(2:4,function(bht){
                         tgr <- lps$Mother_Birth_facility_type==Ty;
                         tgr[is.na(tgr)]=F;
                         sum(lps[G&tgr,q4_35]==bht)
                       })
              })
  D <- sapply(lgroups,function(G){sum ( lps$Mother_Birth_facility_type[G]==Ty,na.rm=T)});
  a <- interleave2d(a,FmtPer(t(t(a)/D)))
  dim(D) <- c(1,4)
  rnms <- c(rnms,paste(lnq[Ty],"births"),paste(lnq[Ty],Codes$Label[which(Codes$Question=="q4_35")][2:4]))
  print(a);
  print(D);
  print(  Q4_35Tab)
  Q4_35Tab <- rbind(Q4_35Tab,interleave(D,rep("",4)),a)
}
rownames(Q4_35Tab) <- rnms;
colnames(Q4_35Tab) <- interleave(names(lgroups),rep("",length(lgroups)))
Q4_35Tab
SaveTables(Q4_35Tab,"Q4_35_Time_to_return_after_delivery","Time to return after delivery")


q4_41  <- "Mother_Familily_planning_Using"
Q4_41Tab <- sapply(lgroups,function(G){
                     sapply(c(1,3:11),function(P){
                              sum(lps[G,q4_41]==P,na.rm=T)
                            })
                   })
Q4_41Tab <- rbind(sapply(lgroups,function(G){sum(lps[G,"Mother_Familily_planning_Want_child"]==1,na.rm=T)}),
                  sapply(lgroups,function(G){sum(lps[G,"Mother_Familily_planning_Want_child"]==2,na.rm=T)}),
                  Q4_41Tab)
Q4_41Tab <- interleave2d(Q4_41Tab,FmtPer(t(t(Q4_41Tab)/colSums(Q4_41Tab ))))
rownames(Q4_41Tab) <- c("No- Not pregnant, want child","No- Not pregnant, don't want child",Codes$Label[which(Codes$Question=="q4_41")][c(1,3:11)])
colnames(Q4_41Tab) <- interleave(names(lgroups),rep("",length(lgroups)))
Q4_41Tab
SaveTables(Q4_41Tab,"Q4_41_Family_Planning_Use","Family planning use")
q4_43 <- "Mother_Familily_planning_Want_child"


q5_5 <- "Mother_Child_Have_monitoring_booklet"
Q5_5Tab <- rbind(sapply(lgroups,function(G){sum(lps[G,q5_5]==1,na.rm=T)}),
                 sapply(lgroups,function(G){sum(lps[G,q5_5]==2,na.rm=T)}))
Q5_5Tab <- rbind(interleave(NumMothers,rep("",length(lgroups))),
                 interleave2d(Q5_5Tab,FmtPer(t(t(Q5_5Tab)/NumMothers))))
rownames(Q5_5Tab) <- c("Num Mothers","Have Monitoring Book","Don't Have Monitoring Book")
Q5_5Tab
SaveTables(Q5_5Tab,"Q5_5_Mothers_Have_Monitoring_Book","Mothers have monitoring book")



  
Codes[which(Codes$Question=="q5_6_1_1"),]
q5_6_1_1 <- "Mother_Child_Immu_BCG"
bcgCard <- "Mother_Child_Immu_BCG_date"
Q5_6Tab <- table(lps[,q5_6_1_1],lps$Group)
Q5_6Tab <- rbind(Q5_6Tab,sapply(lgroups,function(G){sum(lps[G,bcgCard]!="",na.rm=T)}))
Q5_6Tab <- rbind(Q5_6Tab,Total=colSums(Q5_6Tab))
Q5_6Tab <- cbind(Q5_6Tab,All=rowSums(Q5_6Tab))
Q5_6Tab <- interleave2d(Q5_6Tab,FmtPer(t(t(Q5_6Tab)/NumMothers)))
rownames(Q5_6Tab) <-c( Codes$Label[which(Codes$Question=="q5_6_1_1")],"Card","Total")
colnames(Q5_6Tab) <- interleave(names(lgroups),rep("",length(lgroups)))
Q5_6Tab
SaveTables(Q5_6Tab,"Q5_6_1_BCG_Immunization_Rates","BCG Immunization")
which(names(lps)=="Mother_Child_Immu_BCG")


for (i in 1:10) {
  qnm <- paste("q5_6_1_",i,sep="")
  qn <- which(names(lpsraw)==qnm)[1]
  qn2 <- which(names(lpsraw)==paste("q5_6_2_",i,sep=""))[1]
  Q5_6NTab  <- c()
  Q5_6NTab <- table(lps[,qn],lps$Group)
  Q5_6NTab <- cbind(Q5_6NTab,All=rowSums(Q5_6NTab))
  Q5_6NTab <- rbind(Q5_6NTab,sapply(lgroups,function(G){sum(lps[G,qn2]!="" & lps[G,qn2]!="00.00.0000",na.rm=T)}))
  Q5_6NTab <- rbind(Q5_6NTab,sapply(lgroups,function(G){sum(lps[G,qn2]=="00.00.0000",na.rm=T)}))
  Q5_6NTab <- rbind(Q5_6NTab,Total=colSums(Q5_6NTab))
  Q5_6NTab <- interleave2d(Q5_6NTab,FmtPer(t(t(Q5_6NTab)/NumMothers)))
  rownames(Q5_6NTab) <-c(paste("Verbal", Codes$Label[which(Codes$Question=="q5_6_1_1")]),"Card Yes","Card No","Total")
  colnames(Q5_6NTab) <- interleave(names(lgroups),rep("",length(lgroups)))
  Q5_6NTab <- Q5_6NTab[c(4,1,5,2,3,6),]
  SaveTables(Q5_6NTab,paste("Q5_6",i,names(lps)[qn],"Rates",sep="_"),paste(names(lps)[qn],"Immunization Rates"))
  print(paste(names(lps)[qn],"Immunization Rates"))
  print(Q5_6NTab)
}


yncard <- function(qn,qn2,Base,Suffix) {
  Q5_6NTab  <- c()
  Q5_6NTab <- sapply(lgroups,function(G){sapply(c(1:3),function(l){sum(lps[G,qn]==l,na.rm=T)})})
  Q5_6NTab <- rbind(Q5_6NTab,sapply(lgroups,function(G){sum(lps[G,qn2]!="" & lps[G,qn2]!="00.00.0000",na.rm=T)}))
  Q5_6NTab <- rbind(Q5_6NTab,sapply(lgroups,function(G){sum(lps[G,qn2]=="00.00.0000",na.rm=T)}))
  Q5_6NTab <- rbind(Q5_6NTab,Total=colSums(Q5_6NTab))
  Q5_6NTab <- interleave2d(Q5_6NTab,FmtPer(t(t(Q5_6NTab)/NumMothers)))
  rownames(Q5_6NTab) <-c(paste("Verbal", Codes$Label[which(Codes$Question=="q5_6_1_1")]),"Card Yes","Card No","Total")
  colnames(Q5_6NTab) <- interleave(names(lgroups),rep("",length(lgroups)))
  Q5_6NTab <- Q5_6NTab[c(4,1,5,2,3,6),]
  SaveTables(Q5_6NTab,paste(Base,names(lps)[qn],Suffix,sep="_"),paste(names(lps)[qn],Suffix))
  print(names(lps)[qn])
  print(Q5_6NTab)
}

shift <- which(lps[,"Mother_Child_Immu_Date_MR_12-23"]!="")!=which(lps[,"Mother_Child_Vitamin_A_Date"]!="")
sTo <- which(lps[,"Mother_Child_Immu_Date_MR_12-23"]!="")[shift]
sFrom <- which(lps[,"Mother_Child_Vitamin_A_Date"]!="")[shift]
sNo <- which(lps[,"Mother_Child_Vitamin_A_Date"]!="")[!shift]
lps[,"VitADate"] <- ""
lps[sTo,"VitADate"] <- lps[sFrom,"Mother_Child_Vitamin_A_Date"]
lps[sNo,"VitADate"] <- lps[sNo,"Mother_Child_Vitamin_A_Date"]
lps[,"MebDate"] <- "";
lps[sTo,"MebDate"] <- lps[sFrom,"Mother_Child_Mebendazole_Date"]
lps[sNo,"MebDate"] <- lps[sNo,"Mother_Child_Mebendazole_Date"]

yncard(which(names(lps)=="Mother_Child_Vitamin_A")[1],
       which(names(lps)=="VitADate")[1],
       "Q5_7","Rates")

yncard(which(names(lps)=="Mother_Child_Mebendazole")[1],
       which(names(lps)=="MebDate")[1],
       "Q5_7","Rates")



q5_8_1 <- "Mother_Child_Growth_monitoring_12_months"
lvs <- as.numeric(levels(factor(lps[,q5_8_1])))
Visits <- sapply(lgroups,function(G){
                   sapply(lvs,function(N) {
                            sum(lps[G,q5_8_1]>N,na.rm=T)
                          })
                 })
Visits <- rbind( sapply(lgroups,function(G){ sum(lps[G,q5_8_1]==0,na.rm=T)}),Visits)
Visits <- interleave2d(Visits,FmtPer(t(t(Visits)/NumMothers)))
rownames(Visits) <- c("No Visits",paste(lvs+1,"or more visits"))
Visits <- Visits[1:length(lvs),]
colnames(Visits) <- lgNames
Visits
SaveTables(Visits,"Q5_8_1_Number_of_growth_monitoring_visits","Number of growth monitoring visits.")


q5_9 <- "Mother_Child_Still_breastfeeding"
Q5_9Tab <- table(lps[,q5_9],lps$Group)
Q5_9Tab <- cbind(Q5_9Tab,rowSums(Q5_9Tab))
Q5_9Tab <- interleave2d(Q5_9Tab,FmtPer(t(t(Q5_9Tab)/NumMothers)))
rownames(Q5_9Tab) <- c("Yes","No")
colnames(Q5_9Tab) <- lgNames
Q5_9Tab
SaveTables(Q5_9Tab,"Q5_9_Still_Breastfeeding","Still breastfeeding")


by1 <- 5000
to1 <- 20000
breaks1 <- cbind(seq(1,1+to1-by1,by1),seq(by1,to1,by1))
by2 <- 40000
to2 <- 200000
breaks2 <- cbind(seq(1+to1,1+to2-by2,by2),seq(to1+by2,to2,by2))
by3 <- 100000
to3 <- 500000
breaks3 <- cbind(seq(1+to2,1+to3-by3,by3),seq(to2+by3,to3,by3))
breaks <- rbind(c(0,1),breaks1,breaks2,breaks3)
breaks


q5_18 <- "Mother_Child_Last_diarhea_Cost"
Tab <- sapply(lgroups,function(G){
         apply(breaks,1,function(B){
                 sum(lps[G,q5_18]>=B[1] & lps[G,q5_18]<=B[2],na.rm=T)
               })
       })
colSums(Tab)
Tab <- interleave2d(Tab,FmtPer(t(t(Tab)/sapply(lgroups,function(G){sum(!is.na(lps[G,q5_18]))}))))
rownames(Tab) <- apply(breaks,1,function(B){paste("[",sprintf(B[1],fmt="%7.0f"),"-",sprintf(B[2],fmt="%7.0f"),"]LAK")})
colnames(Tab) <-  lgNames
Tab
SaveTables(Tab,"Q5_18_Cost_of_treating_childs_last_diarhea","Cost of diarhea treatment")


q5_23 <- "Mother_Child_Last_respiratory_First_treatment_Cost"
Tab <- sapply(lgroups,function(G){
         apply(breaks,1,function(B){
                 sum(lps[G,q5_23]>=B[1] & lps[G,q5_23]<=B[2],na.rm=T)
               })
       })
colSums(Tab)
Tab <- interleave2d(Tab,FmtPer(t(t(Tab)/sapply(lgroups,function(G){sum(!is.na(lps[G,q5_23]))}))))
rownames(Tab) <- apply(breaks,1,function(B){paste("[",sprintf(B[1],fmt="%7.0f"),"-",sprintf(B[2],fmt="%7.0f"),"]LAK")})
colnames(Tab) <-  lgNames
Tab
SaveTables(Tab,"Q5_23_Mother_Child_Last_respiratory_First_treatment_Cost","Cost of respiratory treatment")

q5_28 <- "Mother_Child_Last_fever_First_treatment_Cost"
Tab <- sapply(lgroups,function(G){
         apply(breaks,1,function(B){
                 sum(lps[G,q5_28]>=B[1] & lps[G,q5_28]<=B[2],na.rm=T)
               })
       })
colSums(Tab)
Tab <- interleave2d(Tab,FmtPer(t(t(Tab)/sapply(lgroups,function(G){sum(!is.na(lps[G,q5_28]))}))))
rownames(Tab) <- apply(breaks,1,function(B){paste("[",sprintf(B[1],fmt="%7.0f"),"-",sprintf(B[2],fmt="%7.0f"),"]LAK")})
colnames(Tab) <-  lgNames
Tab
SaveTables(Tab,"Q5_28_Mother_Child_Last_fever_First_treatment_Cost","Cost of fever treatment")







