#Access and health spending analysis
source("./SmallScripts/Utility.r")
source("./SmallScripts/DefineGroups.r")
library("xtable")


Qs<-c("Illness","NumIllnesses","Overnight_Health_Centre","Nights_Health_Centre","Overnight_District_Hospital","Nights_District_Hospital","Overnight_Provincial_Hospital","Nights_Provincial_Hospital","Overnight_National_Hospital","Nights_National_Hospital","Overnight_Private_Clinic","Nights_Private_Clinic","Care_At-_No_care","Care_At-_Home-made_medicine","Care_At-_Village_modern_medical_practitioner","Care_At-_Village_health_volunteer","Care_At-_Traditional_healer","Care_At-_Health_centre","Care_At-_District_hospital","Care_At-_Provincial_hospital/Regional","Care_At","Care_At_-_National_hospital","Care_At_-_Private_pharmacy","Care_At_-_Private_clinic","Care_At_-_Abroad","Care_At_-_Illegal_medical_practitioner","Care_At_-_Other","Care_At_-_Do_not_remember","Care_At_-_Do_not_know","Care_At_-_Military_Hospital","Care_At_-_Religious_Healer")

OvernightFTypes  <- c(
    "Health_Centre",
    "District_Hospital",
    "Provincial_Hospital",
    "National_Hospital",
    "Private_Clinic");

##question columns
HadCareQs <- sapply (OvernightFTypes,function(X){paste("Overnight",X,sep="_")})
CareLenQs <- sapply (OvernightFTypes,function(X){paste("Nights",X,sep="_")})
##sanitize data --replace NA with 0 --probably not needed
sapply(CareLenQs,function(Q) {IndiHealth[!is.na(IndiHealth[,Q]),Q]==1})

##get care flags and count by facility
HadCare <- sapply (HadCareQs,function(X){IndiHealth[,X]==1})
colSums(HadCare,na.rm=T)

##inspect care distribution
OvernightCare  <- sapply(ligroups,function(G){
                           sapply(HadCareQs,function(Q){
                                    sum(IndiHealth[G,Q]==1,na.rm=T)})
                         })


## OvernightCareF <- rbind(interleaveBl(NumIndivids),
##                         intPer(OvernightCare,NumIndivids))
## rownames(OvernightCareF) <- c("Num Individuals",rownames(OvernightCare))
## colnames(OvernightCareF) <- lgNames
## OvernightCareF
## SaveTables(OvernightCareF,"Q2_8.10.12.14.16_Inpatient_Care_Rates","")


##################################################
InpCareCountQNs <- c(8,10,12,14,16)
InpCareNames <- c("health centre","district hospital","provincial hospital","national hospital","private clinic")
InpCareCountQs <-

InpCareByHH <- t(sapply(InpCareCountQNs,function(C){
                        InpCareCountQs <- paste("q2",C,1:30,sep="_");
                        InpEvents <- apply(lps[,InpCareCountQs],1,function(X){sum(X==1,na.rm=T)})
                        sapply(lgroups,function(G){
                                 sum(InpEvents[G])
                               })
                      }))

print(InpCareByHH)
print(NumGroups)


InpCareByHHF <- intPer(InpCareByHH,NumGroups)
InpCareByHHF
rownames(InpCareByHHF) <- InpCareNames
InpCareByHHF
SaveTables(InpCareByHHF,"Q2_8.10.12.14.16_Inpatient_HH_Care_Rates","")


InpCareCountQs <-sapply(InpCareCountQNs,function(C){ paste("q2",C,1:30,sep="_")})
InpEvents <- apply(lps[,InpCareCountQs],1,function(X){sum(X==1,na.rm=T)})
InpEvents
HHAdmitFreq <- sapply(lgroups,function(G){
         sapply(as.numeric(levels(factor(InpEvents)))+1,function(L) {
         sum( InpEvents[G]>=L)})
       })
HHAdmitFreq <- intPer(HHAdmitFreq,NumGroups)
rownames(HHAdmitFreq) <- paste("At least",1:11,"Inpatient/HH")
colnames(HHAdmitFreq) <- lgNames;
HHAdmitFreq
SaveTables(HHAdmitFreq,"Q2_8.10.12.14.16_Inpatient_HH_Admit_Freq","")


InpNAdmitType <- apply(IndiHealth[,HadCareQs],1,function(X){sum(X==1,na.rm=T)})
InpNFacility <- sapply(ligroups,function(G){
                         sapply(as.numeric(levels(factor(InpNAdmitType)))+1,function(L){
                                  sum(InpNAdmitType[G]>=L)})
                       })
InpNFacilityF <- rbind(interleaveBl(NumIndivids),
                       intPer(InpNFacility,NumIndivids))
rownames(InpNFacilityF) <- c("Num Individuals",paste("At least",1:3,"facilities"))
colnames(InpNFacilityF) <- lgNames
InpNFacilityF
SaveTables(InpNFacilityF,"Q2_8.10.12.14.16_Inpatine_Num_Care_Types_Per_Indv","")


InpEvents <- sapply(ligroups,function(G){
         sapply(HadCareQs,function(C){
                  sum(IndiHealth[G,C]==1,na.rm=T)})
       })
InpEvents <- rbind(InpEvents,Total=colSums(InpEvents))
InpEvents
InpLeng <- sapply(ligroups,function(G){
                    sapply(CareLenQs,function(C){
                             sum(IndiHealth[G,C],na.rm=T)})
                  })
InpLeng <- rbind(InpLeng,Total=colSums(InpLeng))
InpLeng
AveragSayLen <- InpLeng/InpEvents
AveragSayLen <- interleave2d(InpEvents,AveragSayLen)
rownames(AveragSayLen) <- rownames(InpLeng)
colnames(AveragSayLen) <- lgNames
AveragSayLen
SaveTables(AveragSayLen,"Q2_8.10.12.14.16_Inpatient_Avg_Stay_Len","")




InpCareCount <- sapply(lgroups,function(G){
                         sapply(InpCareCountQNs,function(C){
                                  sum(apply(lps[G,paste("q2",C,1:30,sep="_")],1,function(X){
                                        sum(X==1,na.rm=T)>1
                                      }))
                                })
                       })
InpCareCount
intPer(InpCareCount,NumGroups)

rownames(InpCareCount) <- InpCareNames
InpCareCountF <- rbind(interleave(NumIndivids,rep("",4)),
                       interleave2d(InpCareCount,sprintf(fmt="%.3f",t(t(InpCareCount)/NumIndivids))))
rownames(InpCareCountF) <- c("Num Individuals",InpCareNames)
InpCareCount
SaveTables(InpCareCountF,"Q2_8-16_X_Inpatient_Care_Utilization_Rates","Care Utilization rates by individuals in the past year");







SaveTables (OvernightCare,"Q2_Inpatiant_Care_Counts","Number of inpatient care events by Group and Facility")
NumIndivids
OvernightCarePbyG <- t(t(OvernightCare)/NumIndivids)
OvernightCarePbyG <- rbind(OvernightCarePbyG,Total=colSums(OvernightCarePbyG))
OvernightCarePbyG <- ToPercents(OvernightCarePbyG)
SaveTables (OvernightCarePbyG,"Q2_Inpatiant_Care_PercentByGroup","Percent of individuals seeking inpatient care by Group and Facility")
OvernightCarePbyC <- PercentifyTable(OvernightCare)
SaveTables (OvernightCarePbyC,"Q2_Inpatiant_Care_PercentByCenter","Distribution of facility usage by group")


OvernightCareLen  <- sapply(ligroups,function(G){sapply(CareLenQs,function(Q){sum(IndiHealth[G,Q],na.rm=T)})})
OvernightCareLen
OvernightCare



AvgStayLen <- OvernightCareLen/OvernightCare;
format(AvgStayLen,digits=3)
SaveTables(AvgStayLen,"Q2_Inpatiant_AverageStay_Lenght","Average number of nights per stay by Group and Facility Type")

for (lenQ in CareLenQs){
  NightsUsed <- sapply(ligroups[1:3],function(G){
                         sapply(levels(factor(IndiHealth[,lenQ])),function(L) {
                                  sum(IndiHealth[G,lenQ]==L,na.rm=T)})
                       })
  print(lenQ)
  print(NightsUsed)
  try(print(chisq.test(  t(NightsUsed))$p.value))
}
                


##Check care length sanity
CareLengths <- sapply(OvernightFTypes,function(Q){
                        cq <- paste("Overnight_",Q,sep="");
                        lq <- paste("Nights_",Q,sep="");
                        hc <- !is.na(IndiHealth[,cq])
                        levels(factor(IndiHealth[hc,lq]));
                      })


##Outpatient Care
opc<-c("Care_At_No_care","Care_At_Homemade_medicine","Care_At_Village_modern_medical_practitioner","Care_At_Village_health_volunteer","Care_At_Traditional_healer","Care_At_Health_centre","Care_At_District_hospital","Care_At_Provincial_hospital/Regional","Care_At_National_hospital","Care_At_Private_pharmacy","Care_At_Private_clinic","Care_At_Abroad","Care_At_Illegal_medical_practitioner","Care_At_Military_Hospital","Care_At_Religious_Healer")
opcnames <- c("No Care","Homemade medicine","Village modern medical practitioner","Village health volunteer","Traditional healer","Health centre","District hospital","Provincial hospital","National hospital","Private pharmacy","Private clinic","Care received abroad","Unlicensed medical practitioner","Military hospital","Religious healer")
sapply(opc,function(C){which(names(IndiHealth)==C)})
IndiHealth[,32]

opcnames


OutPatientEvents <- t(sapply(opc,function(C){apply(ligroups,2,function(G){sum(!is.na(IndiHealth[G,C]))})}))
rownames(OutPatientEvents) <- opcnames
## Find Sick
SickCount <- apply(IndiHealth,1,function(H){sum(!is.na(H[opc]))})
NumNotSick  <- sapply(ligroups,function(G){sum(SickCount[G]==0)})
## Order the events
order(OutPatientEvents[2:length(OutPatientEvents[,1]),"All"],decreasing=T)+1
OutPatientEvents <- rbind(Not_Sick=NumNotSick,
                          OutPatientEvents[1,],
                          OutPatientEvents[order(OutPatientEvents[2:length(OutPatientEvents[,1]),"All"],decreasing=T)+1,]
                          )
rownames(OutPatientEvents)[2] <- "No Care"
colSums(OutPatientEvents)
OutPatientEvents

order(OutPatientEvents[,"All"])


OutPatientEventsT <- rbind(OutPatientEvents,Total_Care_Events=colSums(OutPatientEvents))
SaveTables(OutPatientEventsT,"Q2_Outpatient_event_count","Individual care event counts (max one per individual)")
OutPatientEvents <- OutPatientEvents[order(OutPatientEvents[,"All"]),]## Order the events

print("yeah?")

OutPatientEventsF <- rbind(interleaveBl(NumIndivids),
                           intPer(OutPatientEvents,NumIndivids))
rownames(OutPatientEventsF) <- c("Num Individuals",rownames(OutPatientEvents))
colnames(OutPatientEventsF) <- lgNames
OutPatientEventsF
SaveTables(OutPatientEventsF,"Q2_7_Outpatient_events_per_individual","")

OutPatientEventsF <- rbind(interleaveBl(NumGroups),
                           intPer(OutPatientEvents,NumGroups))
rownames(OutPatientEventsF) <- c("Num Individuals",rownames(OutPatientEvents))
colnames(OutPatientEventsF) <- lgNames
OutPatientEventsF
SaveTables(OutPatientEventsF,"Q2_7_Outpatient_events_per_HH","")



OutPatientUsageDist <- ToPercents(t(t(OutPatientEventsT)/NumIndivids))
OutPatientUsageDist
SaveTables(OutPatientUsageDist,"Q2_Outpatient_service_utilitzation_rates","Percent of group utilizing services")
OutPatientEventsP  <- PercentifyTable(OutPatientEvents)
SaveTables(OutPatientEventsP,"Q2_Outpatient_usage_distribution","Percent of outpatient services by service type within each group");

limgroups <- vector('logical',length(IndiHealth[,1]))
for (i in 1:length(limgroups)) {
   limgroups[i]<- lps$Gender[IndiHealth$SRow[i]]=="M";
}
limgroups

## sum(limgroups,na.rm=T)
## OutPatientEventsM <- t(sapply(opc,function(C){sapply(ligroups,function(G){sum(!is.na(IndiHealth[G&limgroups,C]))})}))
## fresp <- sapply(lgroups,function(G){sum(G&limgroups)})
## OutPatientEventsM <- rbind(OutPatientEventsM,TotalEvents=colSums(OutPatientEventsM),NumIndivs=mresp)
## write.csv(OutPatientEventsM,"./output/Q2_OutpatientEvents_For_Male_Respondents.csv")
## OutPatientEventsM


## OutPatientEventsF <- t(sapply(opc,function(C){apply(ligroups,2,function(G){sum(!is.na(IndiHealth[G&!limgroups,C]))})}))
## fresp <- sapply(lgroups,function(G){sum(G&!limgroups)})
## OutPatientEventsF <- rbind(OutPatientEventsF,TotalEvents=colSums(OutPatientEventsF),NumIndivs=fresp)
## write.csv(OutPatientEventsF,"./output/Q2_OutpatientEvents_For_Female_Respondents.csv")

## g <- 3;
## TBT <- c(OutPatientEventsF["TotalEvents",g],OutPatientEventsF["NumIndivs",g],OutPatientEventsM["TotalEvents",g],OutPatientEventsM["NumIndivs",g])
## dim(TBT) <- c(2,2)
## chisq.test(TBT)




NI <- sapply(lgroups,function(G){sum(lps$FamSize[G])})





cmps <- c(1,2,1,3,2,3)
dim(cmps) <- c(2,3)
cmps <- as.data.frame(cmps)
cmps
sigt <- t(sapply(1:length(OutPatientEvents[,1]),function(i){
                   T2 <- rbind(NumIndivids-OutPatientEvents[i,],OutPatientEvents[i,])
                   sapply(cmps,function(C){
                            chisq.test(T2[,C])$p.value
                          })
                 }))
colnames(sigt) <- c("PreID vs GeoID","PreID vs NoAssist","NoAssit vs GeoID")
rownames(sigt) <- rownames(OutPatientEvents)
formatC(sigt,digits=3,format="f"      )
SaveTables(sigt,"Q2_Outpatient_usage_distribution_significance","Statistical significance of difference in usage rates by group and service")



TotUsagePerIndv <- rowSums(!is.na(IndiHealth[,opc[2:length(opc)]]))
IndiOutUsed <- apply(IndiHealth[,opc],1,function(X){sum(!is.na(X))>0})
sapply(ligroups,function(G){sum(IndiOutUsed[G])})


levels <- levels(factor(TotUsagePerIndv))
UsagePerIndv <- sapply(ligroups,function(G){sapply(levels[2:length(levels)],function(L){sum(TotUsagePerIndv[G]==L)})})
UsagePerIndv <- rbind(sapply(ligroups,function(G){sum(!is.na(IndiHealth[G,"Care_At_No_care"]))}),UsagePerIndv)
rownames(UsagePerIndv)[1] <- "No Care"
SaveTables(UsagePerIndv,"Q2_Outpatient_NumServices_Used","Percent of individuals in a group reporting N number of different service types used in a year")
UsagePerIndv
ToPercents(t(t(UsagePerIndv)/NumIndivids))

HH <- sapply(1:930,function(S){which(IndiHealth$SRow==S)})
HHOutPatientEvents <- sapply(opc,function(C){sapply(HH,function(H){sum(!is.na(IndiHealth[H,C]))})})
HHOutPatientEventCount <- t(sapply(opc,function(C){sapply(lgroups,function(G){sum(HHOutPatientEvents[G,C])})}))
HHOutPatientEventCount


lim<-8E7
lim<-2E7
breaks<-seq(0,lim,lim/30)
h1<-hist(lps$HH_Cash_income[lps$HH_Cash_income<lim & lgroups[[1]]],breaks=breaks)
h2<-hist(lps$HH_Cash_income[lps$HH_Cash_income<lim & lgroups[[2]]],breaks=breaks)
h3<-hist(lps$HH_Cash_income[lps$HH_Cash_income<lim & lgroups[[3]]],breaks=breaks)
plot(h1,col=rgb(1,0,0,.25))
plot(h2,col=rgb(0,1,0,.25),add=T)
plot(h3,col=rgb(0,0,1,.25),add=T)

lim<-2E7
sapply(lgroups,function(G){sum(lps$HH_Cash_income[G]<lim)})


## source("./SmallScripts/Count_Consultation_Users.r")
## names(ConsultTable)
## ConsultTable[1,]

## cgroups  <- sapply(c("PreID","GeoID","NoAssist"),function(G){ConsultTable$Group==G})
## cgroups  <- cbind(cgroups,ALL=T)
## cIndv    <- as.data.frame(t(sapply(as.data.frame(t(ConsultTable)),function(C){IndiHealth[which(IndiHealth$SRow==C["Serial"])[C["ID"] ],]})))
## ConsultTable <- cbind(ConsultTable,cIndv)


ill <- !is.na(lps$HH_Illness_1_report_individual_number)
lpsIll <- lps[ill,]
qnIll <- paste("q2_5_",lpsIll$HH_Illness_1_report_individual_number,sep="")
qN <- sapply(qnIll,function(N){length(which(names(lps)==N))!=1})
qnIll[qN]
lpsIll <- lpsIll[!qN,]
qnIll <- qnIll[!qN]
qN <- sapply(qnIll,function(N){length(which(names(lps)==N))!=1})
sum(qN)
sum(is.na(sapply(1:sum(!qN),function(i) {lpsIll[i,qnIll[i]]})))
lpsIll$Serial[which(sapply(1:sum(!qN),function(i) {lpsIll[i,qnIll[i]]}==2))]


gM <- sapply (lps$Mother_ID[Mothers],function(M){paste("q2_2_",M,sep="")})
qM <- sapply(gM,function(N){length(which(names(lps)==N))!=1})
lpsM <- lps[Mothers,]
nullM <- sapply(1:length(gM),function(i){is.null(lpsM[i,gM[i]])})
sapply(c(1:length(gM))[!nullM],function(i){lpsM[i,gM[i]]})

wasIll <- sapply(1:30,function(N){paste("q2_5_",N,sep="")});
numInds_PerFamily <- apply(as.matrix(lps[,wasIll]),1,function(X){sum(!is.na(X))}) 
wasIll_PerFamilly <- apply(as.matrix(lps[,wasIll]),1,function(X){sum(X==1,na.rm=T)}) 
wasIll_PerFamilly
levels <- levels(factor(wasIll_PerFamilly))

sapply(lgroups,function(G){sum(wasIll_PerFamilly[G]/sum(G))})


SumGroup <- function(PatFrame,G,Var,Center=T) {
  if (Center!=T) {
    Center  <-  PatFrame$CareCenter == Center;
  } 
  Zeros <- sum(PatFrame[G&Center,Var]==0);
  DK <-PatFrame[,Var]==98 | PatFrame[,Var]==99
  Count  <- sum(G&Center);
  out <- findOutliers(PatFrame[G&Center& !DK,Var],OLim);
  
  Total <- sum(PatFrame[G&Center& !DK,Var][!out])
  Mean <- mean(PatFrame[G&Center& !DK,Var][!out])
  Median <- median(PatFrame[G&Center& !DK,Var][!out])
  Max <- max(PatFrame[G&Center& !DK,Var][!out])
  DK <- sum(DK[G&Center]);
  Out <- sum(out);
  if (OLim>0) {
    list(Zeros=Zeros,DK=DK,Outliers=Out,Count=Count,Total=Total,Mean=Mean,Median=Median,Max=Max)
  }else {
    list(Zeros=Zeros,DK=DK,Count=Count,Total=Total,Mean=Mean,Median=Median,Max=Max)
  }
}



OLim=-1
OutPatTotCostAll <- sapply(OutPatGroups,function(G){SumGroup(OutPatCostTable,G,"Overall_average")})
OutPatTotCostAll
write.csv(OutPatTotCostAll,file="./output/Q3_3_Overall_cost_All_services.csv")

InPatTotCostAll <- sapply(InPatGroups,function(G){SumGroup(InPatCostTable,G,"Overall_average")})
InPatTotCostAll
write.csv(InPatTotCostAll,file="./output/Q3_5_Overall_cost_All_services.csv")

InPatTotCostAll2 <- sapply(InPatGroups2,function(G){SumGroup(InPatCostTable2,G,"Overall_average")})
InPatTotCostAll2
write.csv(InPatTotCostAll2,file="./output/Q3_7_Overall_cost_All_services.csv")


OLim=3
OutPatTotCostAll <- sapply(OutPatGroups,function(G){SumGroup(OutPatCostTable,G,"Overall_average")})
OutPatTotCostAll
write.csv(OutPatTotCostAll,file="./output/Q3_2_Overall_cost_All_services_limit3SD.csv")

InPatTotCostAll <- sapply(InPatGroups,function(G){SumGroup(InPatCostTable,G,"Overall_average")})
InPatTotCostAll
write.csv(InPatTotCostAll,file="./output/Q3_5_Overall_cost_All_services_limit3SD.csv")

InPatTotCostAll2 <- sapply(InPatGroups2,function(G){SumGroup(InPatCostTable2,G,"Overall_average")})
InPatTotCostAll2
write.csv(InPatTotCostAll2,file="./output/Q3_7_Overall_cost_All_services_limit3SD.csv")

CCs <- levels(factor(InPatCostTable$CareCenter))


GF <- function(Var) {

OutTabs <- sapply(CCs,function(CC){
         tab <- paste("OutPatTotCost",CC,Var,sep="_")
         assign (tab , sapply(OutPatGroups,function(G){SumGroup(OutPatCostTable,G,Var,CC)}))
         fname <- paste("./output/Q3_3_",Var,"_cost_",CC,".csv",sep="");
         print(fname)
         print(CC)
         print(get(tab))
         write.csv(get(tab),fname)
         return(get(tab))
       })

InTabs <- sapply(CCs,function(CC){
         tab <- paste("InPatTotCost",CC,Var,sep="_")
         assign (tab , sapply(InPatGroups,function(G){SumGroup(InPatCostTable,G,Var,CC)}))
         fname <- paste("./output/Q3_5_",Var,"_cost_",CC,".csv",sep="");
         print(fname)
         print(CC)
         print(get(tab))
         write.csv(get(tab),fname)
         return(get(tab))
       })

InTabs <- sapply(CCs,function(CC){
         tab <- paste("InPatTotCost",CC,Var,sep="_")
         assign (tab , sapply(InPatGroups2,function(G){SumGroup(InPatCostTable2,G,Var,CC)}))
         fname <- paste("./output/Q3_5_",Var,"_cost_",CC,".csv",sep="");
         print(fname)
         print(CC)
         print(get(tab))
         write.csv(get(tab),fname)
         return(get(tab))
       })
}

vars <- c("Medical_Fee","Medicine","Transport","Others","Overall_average")
s <- lapply(vars,GF)


## CC  <- CCs[1];
## CC1 <- lapply(vars,function(Var){
##                     fname <- paste("./output/Q3_Outpatient_",CC,".csv",sep="");
##                     return(summ)
##                   })


GetCostMeans <- function(PatGroup,PatCostTable,patType) {
  STable <- data.frame(stringsAsFactors=F)
  rnames <- c();
  CCNames <- c();
  CTNames <- c();
  for (c in 1:length(CCs)) {
    CC <- CCs[c];
    for(v in 1:length(vars)) {
      Var <- vars[v];
      summ <- sapply(PatGroup,function(G){SumGroup(PatCostTable,G,Var,CC)})
      r <- as.vector(summ["DK",])
      r <- as.numeric(sapply(summ["DK",],function(V){return(V)}))
      r2 <- as.numeric(sapply(summ["Count",],function(V){return(V)}))
      row <- c();
      for (i in 1:4) {
        row <- c(row,r2[i]-r[i],summ["Mean",i])
      }
      STable <- rbind(STable,row)
      CCNames <- c(CCNames,CC);
      CTNames <- c(CTNames,Var);
    }
  }
  STable <- cbind(CCNames,CTNames,STable)
  colnames(STable) <- c("Center Type","Cost Type","N PreID","Mean PreID","N GeoID","Mean GeoID","N No Assist","Mean No Assist","N All","Mean All")
  print(STable)
  write.csv(STable,paste("./output/Q3_",patType,"_Means.csv",sep=""));
}

OLim=-1
GetCostMeans(OutPatGroups,OutPatCostTable,"3")
GetCostMeans(InPatGroups, InPatCostTable, "5")
GetCostMeans(InPatGroups2,InPatCostTable2,"7")
GetCostMeans(InPatGroupsAll,InPatCostTableAll,"5+7")

#########################################
##Get the means for all oupatient Services
write.csv(interleave2d(sapply(OutPatGroups,function(G){
                                sapply(vars,function(V){a <- SumGroup(OutPatCostTable,G,V);
                                                        a$Count-a$DK})
                              }),
                       sapply(OutPatGroups,function(G){
                                sapply(vars,function(V){SumGroup(OutPatCostTable,G,V)$Mean})
                              })),"./output/3_3_Total_Means.csv")

#########################################
##Get the means for all inpatient Services
write.csv(interleave2d(sapply(InPatGroupsAll,function(G){
                                sapply(vars,function(V){a <- SumGroup(InPatCostTableAll,G,V);
                                                        a$Count-a$DK})
                              }),
                       sapply(InPatGroupsAll,function(G){
                                sapply(vars,function(V){SumGroup(InPatCostTableAll,G,V)$Mean})
                              })),"./output/3_5+7_Total_Means.csv")


OverallInpMean <- sapply(InPatGroupsAll,function(G){
                           sapply(CCs,function(C){
                                    SumGroup(InPatCostTableAll,G,"Overall_average",C)$Mean;})
                         })
OverallInpMean <- OverallInpMean[order(OverallInpMean[,4],decreasing=T),]
OverallInpMean
write.csv(OverallInpMean,"./output/Q3_5+7_Inpatient_Overall_Mean.csv")

OverallOutpMean <- sapply(OutPatGroups,function(G){
                           sapply(CCs,function(C){
                                    SumGroup(OutPatCostTable,G,"Overall_average",C)$Mean;})
                         })
OverallOutpMean <- OverallOutpMean[order(OverallOutpMean[,4],decreasing=T),]
OverallOutpMean
write.csv(OverallOutpMean,"./output/Q3_3_Outpatient_Overall_Mean.csv")

sapply(OutPatGroups,function(G){SumGroup(OutPatCostTable,G,"Overall_average",T)$Mean;})



OLim=3
sapply (OutPatGroups,function(G){SumGroup(OutPatCostTable,
                                         G,
                                          vars[4],
                                          CCs[2])})

OLim=3
GetCostMeans(InPatGroups, InPatCostTable, "5_limit3sd")
GetCostMeans(InPatGroups2,InPatCostTable2,"7_limit3sd")
GetCostMeans(OutPatGroups,OutPatCostTable,"3_limit3sd")



AdmitPerHH <- sapply(seq(8,16,2),function(C){
                       q2_xx <- paste("q2_",C,"_",1:30,sep="")
                       ##admissions in the past year by HH
                       admissions <- apply(lps[,q2_xx],1,function(R){
                                             sum(as.numeric(R[])==1,na.rm=T)
                                           })
                       sapply(lgroups,function(G){
                                sum(admissions[G]>0)
                              })
                     })
AdmitPerHH <- t(AdmitPerHH);
rownames(AdmitPerHH) <- c("HC","DH","PH","NH","PC")
AdmitPerHH <- rbind(AdmitPerHH,Total=colSums(AdmitPerHH))
AdmitPerHH
SaveTables(AdmitPerHH,"Q2_8-16_1-30_HouseHolds_With_Admisions_in_past_year","House holds with atleast 1 admission")
AdmitsPerHH <- sapply(seq(8,16,2),function(C){
                       q2_xx <- paste("q2_",C,"_",1:30,sep="")
                       ##admissions in the past year by HH
                       admissions <- apply(lps[,q2_xx],1,function(R){
                                             sum(as.numeric(R[])==1,na.rm=T)
                                           })
                       sapply(lgroups,function(G){
                                sum(admissions[G])
                              })
                     })
AdmitsPerHH <- t(AdmitsPerHH);
rownames(AdmitsPerHH) <- c("HC","DH","PH","NH","PC")
AdmitsPerHH <- rbind(AdmitsPerHH,Total=colSums(AdmitsPerHH))
AdmitsPerHH
SaveTables(AdmitsPerHH,"Q2_8-16_1-30_Total_Admisions_last_year","House holds with atleast 1 admission")


sapply(lgroups,function(G) {sum(lps[G,"HH_Illness_3_report_individual_number"]>0,na.rm=T)})

       
##% of HHs who borrowed money for health events by provider
BorrowService <- c("HH_Services_Consultations","HH_Services_Medicine","HH_Services_Delivery","HH_Services_Admission_without_surgery","HH_Services_Admission_with_surgery")
BorrowByGroup <- sapply(lgroups,function(G){
                          sapply(BorrowService,function(S){
                                   sum(!is.na(lps[G,S]))
                                 })
                        })
BorrowByGroup <- rbind(BorrowByGroup,Total=colSums(BorrowByGroup))
BorrowByGroup
BorrowByGroupF <- rbind(interleaveBl(NumGroups),
                        intPer(BorrowByGroup,NumGroups));
rownames(BorrowByGroupF) <- c("Num Groups",rownames(BorrowByGroup))
colnames(BorrowByGroupF) <- lgNames
write.csv(BorrowByGroupF,"./output/Q3_10_Number_of_HH_borrowing_by_service_and_group.csv")

BorrowMean <- sapply(lgroups,function(G){mean(lps[G,"HH_Payment_Borrowed_TOTAL_LAK"],na.rm=T)})
BorrowMax  <- sapply(lgroups,function(G){max(lps[G,"HH_Payment_Borrowed_TOTAL_LAK"],na.rm=T)})

SaveTables(rbind(BorrowMean,BorrowMax),"Q3_9_total_Money_borrowed_by_group_Mean_and_Max.csv","")


##Household Assets
QAssets <- c("HH_Posses_Video","HH_Posses_Mobile_phone","HH_Posses_Camera","HH_Posses_Television","HH_Posses_Bicycle","HH_Posses_Motorbike","HH_Posses_Car_or_truck","HH_Posses_Tractor","HH_Posses_Boat","HH_Posses_Motor_boat")
Assets <- sapply(lgroups,function(G){sapply(QAssets,function(A){sum(lps[G,A]>0)})})
Assets                 
AssetsP <- format(t(t(Assets)/NumHouseHolds)*100,digits=1)
AssetsP
write.csv(Assets,"./output/Q9_1_1-10_Num_of_HH_with_at_least_one_of_Asset_by_Group.csv")
write.csv(AssetsP,"./output/Q9_1_1-10_Percent_of_HH_with_at_least_one_of_Asset_by_Group.csv")

qname <- sapply(1:30,function(N){paste("q2_3_",N,sep="")})
FamSize <- apply(lps[,qname],1,function(X){sum(!is.na(X))})
if (length(which(names(lps)=="FamSize"))==0) {
  lps <- cbind(lps,FamSize=FamSize);
  print("Adding FamSize")
}
           

## HavePhone <- lps[,"HH_Posses_Mobile_phone"]>0
## Have2Phone <- lps[,"HH_Posses_Mobile_phone"]>2
## ages <- lps[,qage]
## lpsp <- lps[]
## lhpgroups <- lapply(lgroups,function(G){G[HavePhone]})
## sapply(lhpgroups,sum)




meanCellAge <- vector('numeric',930)
minCellAge <- vector('numeric',930)
for (i in 1:930){
  try(meanCellAge[i] <- mean(as.numeric(sort(ages[i,],decreasing=T)[1:lps[i,"HH_Posses_Mobile_phone"]])))
  try(minCellAge[i] <- min(as.numeric(sort(ages[i,],decreasing=T)[1:lps[i,"HH_Posses_Mobile_phone"]])))
}

CellUseAge <- rbind(
sapply(lgroups,function(G){mean(meanCellAge[HavePhone])}),
sapply(lgroups,function(G){mean(meanCellAge[Have2Phone])}),
sapply(lgroups,function(G){mean(minCellAge[HavePhone&G])}),
sapply(lgroups,function(G){mean(minCellAge[Have2Phone&G])}))
rownames(CellUseAge) <- c("Mean mobile phone user age","Mean mobile phone user age in multiphone house hold","Mean youngest phone user age","Mean youngest phone user age in multiphone house hold")
CellUseAge  


#Calculate the OOP/year by pers & HH = utilization OPD x median/average/mean OPD cost (3.3) + utilization IPD x
#median/average/mean IPD costs (avg 3.5+3.7)

numtimesSick <- sapply(lgroups,function(G){
         nutimesSickQ <- paste("q2_6",1:30,sep="_")
         sum(lps[G,nutimesSickQ],na.rm=T)})
cat("Num Times Sick Per Individual\n")
print(numtimesSick/NumIndivids)


CareLocs <- c(
 "No care",
 "Home-made medicine",
 "Village modern medical practitioner",
 "Village health volunteer",
 "Traditional healer",
 "Health centre",
 "District hospital",
 "Provincial hospital/Regional",
 " ",
 "National hospital",
 "Private pharmacy",
 "Private clinic",
 "Abroad",
 "Illegal medical practitioner",
 "Other",
 "Do not remember",
 "Do not know")

IndiCareQ <- c(10,8,7,6,4,5,12,11)
IndiCareQ <- c(10,8,7,6,4,5,12,11)
CareCounts <- sapply(lgroups,function(G){
                       sapply(IndiCareQ,function(L) {
                                CareLocQs  <- paste(paste("q2_7_",1:30,sep=""),L,sep="_")
                                sum (!is.na(lps[G,CareLocQs]))})
                     })
rownames(CareCounts) <- CareLocs[IndiCareQ]
CareCounts
colSums(CareCounts)/NumIndivids

CareCountsF <- rbind(interleave(NumIndivids,rep("",4)),
                     interleave2d(CareCounts,sprintf(fmt="%.3f",t(t(CareCounts)/NumIndivids))))
rownames(CareCountsF) <- c("Num",CareLocs[IndiCareQ])
CareCountsF
SaveTables(CareCountsF,"Q2_7_X_X_Counts_of_outp_care_in_past_year","Outpatient events in past year")



length(CareLocs)

SumGroup(OutPatCostTable,rep(length(OutPatCostTable[,1]),T),"Overall_average","Private_Pharmacist")                  

#Q2 8,10,12,16 we get that the following number of HH had admissions in the past year: YES
OutPatMeans <- sapply(OutPatGroups,function(G){
                        sapply(CCs,function(C){
                                 SumGroup(OutPatCostTable,G,"Overall_average",C)$Mean
                               })
                      })
SaveTables(OutPatMeans,"Q3_3_Overall_Means","Mean Overall Spending by department")
OutPatMeans
SaveTables(t(t(CareCounts)/NumIndivids)*OutPatMeans,"Q2xQ3 Ouptatient spending by individual","Outpatien spending by individual")
NumPerHH <- NumIndivids/sapply(lgroups,sum)

OutpMeanOOP  <- t(t(CareCounts)/NumGroups)*OutPatMeans
OutpMeanOOPF <- rbind(OutpMeanOOP,Total=colSums(OutpMeanOOP,na.rm=T))
OutpMeanOOPF
SaveTables(OutpMeanOOPF,"Outpatient_Mean_OOP_Extrapolation","")



OutPatMedians <- sapply(OutPatGroups,function(G){
                        sapply(CCs,function(C){
                                 SumGroup(OutPatCostTable,G,"Overall_average",C)$Median
                               })
                      })
SaveTables(OutPatMedians,"Q3_3_Overall_Medians","Median Overall Spending by department")
OutPatMedians
SaveTables(t(t(CareCounts)/NumIndivids)*OutPatMedians,"Q2xQ3 Ouptatient median spending by individual","Outpatient media spending by individual")
NumPerHH <- NumIndivids/sapply(lgroups,sum)
SaveTables(NumPerHH*t(t(CareCounts)/NumIndivids)*OutPatMedians,"Q2xQ3 Ouptatient median spending by HH","Outpatient median spending by HH")




##Inpatient Extrapolation
InpCareCountQNs <- c(8,10,12,14,16)
InpCareNames <- c("health centre","district hospital","provincial hospital","national hospital","private clinic")
InpCareCount <- sapply(lgroups,function(G){
                         sapply(InpCareCountQNs,function(C){
                                  sum(lps[G,paste("q2",C,1:30,sep="_")]==1,na.rm=T)})
                       })
rownames(InpCareCount) <- InpCareNames
InpCareCountF <- rbind(interleave(NumIndivids,rep("",4)),
                       interleave2d(InpCareCount,sprintf(fmt="%.3f",t(t(InpCareCount)/NumIndivids))))
rownames(InpCareCountF) <- c("Num Individuals",InpCareNames)
InpCareCount
SaveTables(InpCareCountF,"Q2_8-16_X_Inpatient_Care_Utilization_Rates","Care Utilization rates by individuals in the past year");

InpAllMeanCost <- sapply(InPatGroupsAll,function(G){
                           sapply(CCs[c(4,3,2,1,7)],function(C){
                                    SumGroup(InPatCostTableAll,G,"Overall_average",C)$Mean
                                  })
                         })
write.csv(InpAllMeanCost,"./output/InpAllMeanCost.csv")
InpAllMeanCost



InpAllMedianCost <- sapply(InPatGroupsAll,function(G){
                           sapply(CCs[c(4,3,2,1,7)],function(C){
                                    SumGroup(OutPatCostTable,G,"Overall_average",C)$Median
                                  })
                         })
InpAllMedianCost

sapply(InPatGroupsAll,function(G){
                           sapply(CCs[c(4,3,2,1,7)],function(C){
                                    SumGroup(OutPatCostTable,G,"Overall_average",C)$Count
                                  })
                         })

InpCareCount
InpAllMeanCost
InpMeanOOP  <- t(t(InpCareCount)/NumGroups)*InpAllMeanCost
InpMeanOOPF <- rbind(InpMeanOOP,Total=colSums(InpMeanOOP,na.rm=T))
InpMeanOOPF
SaveTables(InpMeanOOPF,"Inpatient_Mean_OOP_Extrapolation","")

colSums(NumPerHH*t(t(CareCounts)/NumIndivids)*OutPatMeans,na.rm=T)/12

SaveTables(InpAllMedianCost,"Q3_5+7_Median_Overall_Cost","Median overall cost by department")
SaveTables(t(t(InpCareCount)/NumIndivids)*InpAllMedianCost,"Q2XQ3 Inpatient Median Care Cost By Individual","Inpatient Median Care Cost by Individual")
SaveTables(t(t(InpCareCount)/NumGroups)*InpAllMedianCost,"Q2XQ3 Inpatient Median Care Cost By HH","Inpatient Median Care Cost by HH")
NumGroups <- sapply(lgroups,sum)






sapply(OutPatGroups,function(G){
         sapply(CCs,function(C){
                  SumGroup(OutPatCostTable,G,"Overall_average",C)$Means
                })
              })





       
##% Catastrophic health expenditure (> 500,000, >1million kips, > 20% cash income)   
InPatG500k  <- sapply(InPatGroupsAll,function(G){sum(InPatCostTableAll$Overall_average[G]>500000)})
sapply(InPatGroupsAll,sum)
InPatG500k   <- lps$HH_Illness_2_Total_cost_Overall_average>500000 | lps$HH_Illness_3_Total_cost_Overall_average>500000
InPatG500k[is.na(InPatG500k)] <- F;
InPatG1M   <- lps$HH_Illness_2_Total_cost_Overall_average>1000000 | lps$HH_Illness_3_Total_cost_Overall_average>1000000
InPatG1M[is.na(InPatG1M)] <- F
InPatG20P <- apply(lps,1,function(X){
                     crit <- as.numeric(X["HH_Cash_income"])*.2;
                     X["HH_Illness_2_Total_cost_Overall_average"]>crit | X["HH_Illness_3_Total_cost_Overall_average"]>crit;
                   })
InPatG20P[is.na(InPatG20P)] <- F

InpCatExp  <- rbind (sapply(lgroups,function(G){sum(InPatG500k[G])}),
                     sapply(lgroups,function(G){sum(InPatG1M[G])}),
                     sapply(lgroups,function(G){sum(InPatG20P[G])}))
InpCatExp

totInp <- apply(lps,1,function(X) {sum(as.numeric(c(X["HH_Illness_2_Total_cost_Overall_average"],X["HH_Illness_3_Total_cost_Overall_average"])),na.rm=T)})
totInp
InPatG500k   <- totInp>500000
InPatG1M   <- totInp>1000000
InPatG20P <-totInp>lps$HH_Cash_income*.2


InpCatExp  <- rbind (sapply(lgroups,function(G){sum(InPatG500k[G])}),
                     sapply(lgroups,function(G){sum(InPatG1M[G])}),
                     sapply(lgroups,function(G){sum(InPatG20P[G])}))
InpCatExpF <- rbind(interleaveBl(NumGroups),
                    intPer(InpCatExp,NumGroups));
rownames(InpCatExpF) <- c("Num Households",
                          "Last 2 Inpatient Expenditures >   500,000 LAK",
                          "Last 2 Inpatient Expenditures > 1,000,000 LAK",
                          "Last 2 Inpatient Expenditures > 20% Cash Income")
colnames(InpCatExpF) <- lgNames
SaveTables(InpCatExpF,"Q3_5.7_Catastrophic_Expenditure","")

totOutp <- lps$HH_Illness_1_Total_cost_Overall_average
totOutp[is.na(totOutp)] <- 0;
OutPatG500k   <- totOutp>500000
OutPatG1M   <- totOutp>1000000
OutPatG20P <-totOutp>lps$HH_Cash_income*.2


OutpCatExp  <- rbind (sapply(lgroups,function(G){sum(OutPatG500k[G])}),
                     sapply(lgroups,function(G){sum(OutPatG1M[G])}),
                     sapply(lgroups,function(G){sum(OutPatG20P[G])}))
OutpCatExpF <- rbind(interleaveBl(NumGroups),
                    intPer(OutpCatExp,NumGroups));
rownames(OutpCatExpF) <- c("Num Households",
                          "Last Outpatient Expenditure >   500,000 LAK",
                          "Last Outpatient Expenditure > 1,000,000 LAK",
                          "Last Outpatient Expenditure > 20% Cash Outcome")
colnames(OutpCatExpF) <- lgNames
SaveTables(OutpCatExpF,"Q3_3_Catastrophic_Expenditure","")


##OOP check

!is.na(lps$Adult_Admitted_Cost_OOP)&lps$Adult_Admitted_Cost_OOP==1

!is.na(lps$HH_Payment_OOP)
