library("xtable")
lpsraw<-read.csv("./Data/LPS.csv");
lps=lpsraw[1:930,];

ColNames<-read.csv("Constants/ColNames.csv");
names(lps)<-ColNames$VarName;

#Fix Gender
lps$Gender<-factor(lps$Gender,levels = c("1","2"),labels=c("F","M"));

PossesionNames = c("Video","Mobile_phone","Camera","Television","Bicycle","Motorbike","Car_or_truck","Tractor","Boat","Motor_boat","N_Cattle","N_Buffalo","N_Horses","N_Pigs","N_Goats_sheep","N_Chickens");
PossVarName = paste("HH_Posses_",PossesionNames[1:10],sep="")
LiveStockVarName = paste("HH_Livestock_",PossesionNames[11:length(PossesionNames)],sep="")
DistNames = c("Lamam","Kaleum","Dukjeung","Thateang")
lps$"dist" <- factor(lps$"dist",labels = DistNames);


## Villages <- read.csv("Vilages.csv")

## VillageNum <-as.integer(Villages$Code.district)*1000 + Villages$Code.village;
## Villages<-cbind(Villages,VillageNum)

## PreId<-lps$id.format==1;
## r=1;
## for (i in 1:length(PreId)) {
##   vrow <-which (Villages$VillageNum == lps$Village[r]);
##   PreId[r]<-Villages$Pre.ID[vrow]==1
##   r<-r+1;
## }
## lps<-cbind (lps,PreId)


## lps$VilPoor<-factor(lps$VilPoor,c("1","3"),c("Yes","No"));

## HEFCard <-!is.na(lps$HH_Insurance_HEFCard);
## GVTPoor <-!is.na(lps$HH_Insurance_GVTPoor);
## NoInsu <- !is.na(lps$HH_Insurance_Informal_no_insurance)
## VilPoor <-lps$VilPoor=="Yes"
q2_8x <- paste("q2_8_",1:30,sep="")
for (i in 1:30) {lps[,q2_8x[i]] <- as.numeric(lps[,q2_8x[i]]);}


lps$HH_Illness_1_Total_cost_Overall_average <- as.numeric(as.character(lps$HH_Illness_1_Total_cost_Overall_average))
lps$HH_Illness_2_Total_cost_Overall_average <- as.numeric(as.character(lps$HH_Illness_2_Total_cost_Overall_average))
lps$HH_Illness_3_Total_cost_Overall_average <- as.numeric(as.character(lps$HH_Illness_3_Total_cost_Overall_average))

#Split Dependents
#source("SplitDeps.R")
load("IndiHealth.bin");

source("./SmallScripts/DefineGroups.r")
source("./SmallScripts/Count_Consultation_Users.r")





