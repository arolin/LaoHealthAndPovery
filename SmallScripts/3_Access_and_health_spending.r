#Access and health spending analysis
source("./SmallScripts/Utility.r")

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
HadCare <- sapply (HadCareQs,function(X){print(X);IndiHealth[,X]==1})
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
  print(chisq.test(  t(NightsUsed))$p.value)
}
                


##Check care length sanity
CareLengths <- sapply(OvernightFTypes,function(Q){
                        cq <- paste("Overnight_",Q,sep="");
                        lq <- paste("Nights_",Q,sep="");
                        hc <- !is.na(IndiHealth[,cq])
                        levels(factor(IndiHealth[hc,lq]));
                      })


##Outpatient Care
opc<-c("Care_At_No_care","Care_At_Homemade_medicine","Care_At_Village_modern_medical_practitioner","Care_At_Village_health_volunteer","Care_At_Traditional_healer","Care_At_Health_centre","Care_At_District_hospital","Care_At_Provincial_hospital/Regional","Care_At_National_hospital","Care_At_Private_pharmacy","Care_At_Private_clinic","Care_At_Abroad","Care_At_Illegal_medical_practitioner","Care_At_Other","Care_At_Military_Hospital","Care_At_Religious_Healer")



OutPatientEvents <- t(sapply(opc,function(C){sapply(ligroups,function(G){sum(!is.na(IndiHealth[G,C]))})}))
OutPatientEventsT <- rbind(OutPatientEvents,Total_Care_Events=colSums(OutPatientEvents))
SaveTables(OutPatientEventsT,"Q2_Outpatient_event_count","Individual care event counts (max one per individual)")
OutPatientUsageDist <- ToPercents(t(t(OutPatientEvents)/NumIndivids))
OutPatientUsageDist
SaveTables(OutPatientUsageDist,"Q2_Outpatient_service_utilitzation_rates","Percent of group utilizing services")
OutPatientEventsP  <- PercentifyTable(OutPatientEvents)
SaveTables(OutPatientEventsP,"Q2_Outpatient_usage_distribution","Percent of outpatient services by service type within each group");

TotUsagePerIndv <- rowSums(!is.na(IndiHealth[,opc]))
levels <- levels(factor(TotUsagePerIndv))
UsagePerIndv <- sapply(ligroups,function(G){sapply(levels,function(L){sum(TotUsagePerIndv[G]==L)})})
SaveTables(PercentifyTable(UsagePerIndv),"Q2_Outpatient_NumServices_Used","Percent of individuals in a group reporting N number of different service types used in a year")

HH <- sapply(1:930,function(S){which(IndiHealth$SRow==S)})
HHOutPatientEvents <- sapply(opc,function(C){sapply(HH,function(H){sum(!is.na(IndiHealth[H,C]))})})
HHOutPatientEventCount <- t(sapply(opc,function(C){sapply(lgroups,function(G){sum(HHOutPatientEvents[G,C])})}))
HHOutPatientEventCount

IN <- 1:6550
HHn <- sapplysapply(HH,function(H){sum(IndiHealth$SRow[H])})


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
