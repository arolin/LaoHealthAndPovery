source("LoadData.R")

load(file="IndiHealth.bin")

IH <- IndiHealth[,1:39]
names(IH)[names(IH)=="SRow"    ] <- "Serial"
names(IH)[names(IH)=="Care_At_Village_health_volunteer"    ] <- "Care_At_HV"
names(IH)[names(IH)=="Care_At_Traditional_healer"          ] <- "Care_At_TH"
names(IH)[names(IH)=="Care_At_Health_centre"               ] <- "Care_At_HC"
names(IH)[names(IH)=="Care_At_District_hospital"           ] <- "Care_At_DH"
names(IH)[names(IH)=="Care_At_Provincial_hospital/Regional"] <- "Care_At_PH"
names(IH)[names(IH)=="Care_At_National_hospital"           ] <- "Care_At_NH"
names(IH)[names(IH)=="Care_At_Private_pharmacy"            ] <- "Care_At_PP"
names(IH)[names(IH)=="Care_At_Private_clinic"              ] <- "Care_At_PC"
names(IH)[names(IH)=="Care_At_Religious_Healer"            ] <- "Care_At_RH"
names(IH)[names(IH)=="Overnight_Health_Centre"             ] <- "Overnight_At_HC"
names(IH)[names(IH)=="Overnight_District_Hospital"         ] <- "Overnight_At_DH"
names(IH)[names(IH)=="Overnight_Provincial_Hospital"       ] <- "Overnight_At_PH"
names(IH)[names(IH)=="Overnight_National_Hospital"         ] <- "Overnight_At_NH"
names(IH)[names(IH)=="Overnight_Private_Clinic"            ] <- "Overnight_At_PC"
names(IH)[names(IH)=="Nights_Health_Centre"             ] <- "Nights_At_HC"
names(IH)[names(IH)=="Nights_District_Hospital"         ] <- "Nights_At_DH"
names(IH)[names(IH)=="Nights_Provincial_Hospital"       ] <- "Nights_At_PH"
names(IH)[names(IH)=="Nights_National_Hospital"         ] <- "Nights_At_NH"
names(IH)[names(IH)=="Nights_Private_Clinic"            ] <- "Nights_At_PC"

IH$Group <- factor(IH$Group,levels=c(1,2,3),labels=c("PreID","GeoID","NoAssist"))
IH$Group 


IH[,PatOPC] <- !is.na(IH[,PatOPC])
IH[,grep("Overnight",names(IH))][is.na(IH[,grep("Overnight",names(IH))])] <- 0
IH[is.na(IH$NumIllnesses),"NumIllnesses"] <- 0;
for (I in grep("Overnight",names(IH))) {
  IH[,I] <- factor(IH[,I],levels=c(0,1,2),labels=c("NoCare","Overnight","Day"))
}
IH[,grep("Nights",names(IH))][is.na(IH[,grep("Nights",names(IH))])] <- 0

colnames(IH)

IH <- IH[,colnames(IH)!="Marital_Status_Other"]


head(IH)
