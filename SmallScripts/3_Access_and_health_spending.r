#Access and health spending analysis
source("./SmallScripts/Utility.r")
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
sapply(CareLenQs,function(Q) {IndiHealth[is.na(IndiHealth[,Q]),Q]=0})

##get care flags and count by facility
HadCare <- sapply (HadCareQs,function(X){IndiHealth[,X]==1})
colSums(HadCare,na.rm=T)

##inspect care distribution
OvernightCare  <- sapply(ligroups,function(G){sapply(HadCareQs,function(Q){sum(IndiHealth[G,Q]==1,na.rm=T)})})
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
sapply(opc,function(C){which(names(IndiHealth)==C)})
IndiHealth[,32]


OutPatientEvents <- t(sapply(opc,function(C){apply(ligroups,2,function(G){sum(!is.na(IndiHealth[G,C]))})}))
OutPatientEventsT <- rbind(OutPatientEvents,Total_Care_Events=colSums(OutPatientEvents))
SaveTables(OutPatientEventsT,"Q2_Outpatient_event_count","Individual care event counts (max one per individual)")
OutPatientEvents
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

sum(limgroups,na.rm=T)
OutPatientEventsM <- t(sapply(opc,function(C){sapply(ligroups,function(G){sum(!is.na(IndiHealth[G&limgroups,C]))})}))
fresp <- sapply(lgroups,function(G){sum(G&limgroups)})
OutPatientEventsM <- rbind(OutPatientEventsM,TotalEvents=colSums(OutPatientEventsM),NumIndivs=mresp)
write.csv(OutPatientEventsM,"./output/Q2_OutpatientEvents_For_Male_Respondents.csv")
OutPatientEventsM


OutPatientEventsF <- t(sapply(opc,function(C){apply(ligroups,2,function(G){sum(!is.na(IndiHealth[G&!limgroups,C]))})}))
fresp <- sapply(lgroups,function(G){sum(G&!limgroups)})
OutPatientEventsF <- rbind(OutPatientEventsF,TotalEvents=colSums(OutPatientEventsF),NumIndivs=fresp)
write.csv(OutPatientEventsF,"./output/Q2_OutpatientEvents_For_Female_Respondents.csv")

g <- 3;
TBT <- c(OutPatientEventsF["TotalEvents",g],OutPatientEventsF["NumIndivs",g],OutPatientEventsM["TotalEvents",g],OutPatientEventsM["NumIndivs",g])
dim(TBT) <- c(2,2)
chisq.test(TBT)




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


source("./SmallScripts/Count_Consultation_Users.r")
names(ConsultTable)
ConsultTable[1,]

cgroups  <- sapply(c("PreID","GeoID","NoAssist"),function(G){ConsultTable$Group==G})
cgroups  <- cbind(cgroups,ALL=T)
cIndv    <- as.data.frame(t(sapply(as.data.frame(t(ConsultTable)),function(C){IndiHealth[which(IndiHealth$SRow==C["Serial"])[C["ID"] ],]})))
ConsultTable <- cbind(ConsultTable,cIndv)


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
write.csv(OutPatTotCostAll,file="./output/Q3_2_Overall_cost_All_services.csv")

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


CC  <- CCs[1];
CC1 <- lapply(vars,function(Var){
  
                    fname <- paste("./output/Q3_Outpatient_",CC,".csv",sep="");

                    return(summ)
                  })


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
GetCostMeans(InPatGroups, InPatCostTable, "5")
GetCostMeans(InPatGroups2,InPatCostTable2,"7")
GetCostMeans(OutPatGroups,OutPatCostTable,"3")

OLim=3
sapply (OutPatGroups,function(G){SumGroup(OutPatCostTable,
                                         G,
                                          vars[4],
                                          CCs[2])})
OutPa
vars[4]

OLim=3
GetCostMeans(InPatGroups, InPatCostTable, "5_limit3sd")
GetCostMeans(InPatGroups2,InPatCostTable2,"7_limit3sd")
GetCostMeans(OutPatGroups,OutPatCostTable,"3_limit3sd")




##% of HHs who borrowed money for health events by provider
BorrowService <- c("HH_Services_Consultations","HH_Services_Medicine","HH_Services_Delivery","HH_Services_Admission_without_surgery","HH_Services_Admission_with_surgery")
BorrowByGroup <- sapply(lgroups,function(G){sapply(BorrowService,function(S){sum(!is.na(lps[G,S]))})})
BorrowByGroup <- rbind(BorrowByGroup,Total=colSums(BorrowByGroup))
BorrowByGroup
write.csv(BorrowByGroup,"./output/Q3_10_Number_of_HH_borrowing_by_service_and_group.csv")

BorrowMean <- sapply(lgroups,function(G){mean(lps[G,"HH_Payment_Borrowed_TOTAL_LAK"],na.rm=T)})
BorrowMax  <- sapply(lgroups,function(G){max(lps[G,"HH_Payment_Borrowed_TOTAL_LAK"],na.rm=T)})

write.csv(rbind(BorrowMean,BorrowMax),"./Q3_9_total_Money_borrowed_by_group_Mean_and_Max.csv")


##Household Assets
QAssets <- c("HH_Posses_Video","HH_Posses_Mobile_phone","HH_Posses_Camera","HH_Posses_Television","HH_Posses_Bicycle","HH_Posses_Motorbike","HH_Posses_Car_or_truck","HH_Posses_Tractor","HH_Posses_Boat","HH_Posses_Motor_boat")
Assets <- sapply(lgroups,function(G){sapply(QAssets,function(A){sum(lps[G,A]>0)})})
Assets                 
AssetsP <- format(t(t(Assets)/NumHouseHolds)*100,digits=1)
AssetsP
write.csv(Assets,"./output/Q9_1_1-10_Num_of_HH_with_at_least_one_of_Asset_by_Group.csv")
write.csv(AssetsP,"./output/Q9_1_1-10_Percent_of_HH_with_at_least_one_of_Asset_by_Group.csv")

qage <- sapply(1:30,function(N){paste("q2_3_",N,sep="")})
FamSize <- apply(lps[,qname],1,function(X){sum(!is.na(X))})
if (length(which(names(lps)=="FamSize"))==0) {
  lps <- cbind(lps,FamSize=FamSize);
  print("Adding FamSize")
}
           

HavePhone <- lps[,"HH_Posses_Mobile_phone"]>0
Have2Phone <- lps[,"HH_Posses_Mobile_phone"]>2
ages <- lps[,qage]
lpsp <- lps[]
lhpgroups <- lapply(lgroups,function(G){G[HavePhone]})
sapply(lhpgroups,sum)




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

sapply(groups
