
BuildImage <- F
#BuildImage <- T

if (BuildImage) {

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

##The folowing individual has only DK in his report---removing it
lps$HH_Illness_2_report_individual_number[708] <- 0;
lps[708,c("HH_Illness_2_Health_Center_cost_Medicine","HH_Illness_2_Health_Center_cost_Medical_Fee","HH_Illness_2_Health_Center_cost_Transport","HH_Illness_2_Health_Center_cost_Others","HH_Illness_2_Health_Center_cost_Overall_average")] <- NA
lps[708,c("HH_Illness_2_Total_cost_Medicine","HH_Illness_2_Total_cost_Medical_Fee","HH_Illness_2_Total_cost_Transport","HH_Illness_2_Total_cost_Others","HH_Illness_2_Total_cost_Overall_average")] <- NA
lps[541,"HH_Illness_2_District_Hospital_cost_Transport"] <- 48000
lps[658,"HH_Illness_1_Provincial_Hospital_cost_Medical_Fee"] <- 10000


#Split Dependents
#source("SplitDeps.R")
load("IndiHealth.bin");


print("D3")

source("./SmallScripts/DefineGroups.r")

print("D4")
source("./SmallScripts/Count_Consultation_Users.r")

  print("Data Rebuilt")
  source("./SmallScripts/PCA2.r")

  
  save.image(file="InitialAnalysis.RData")
  print("Data Saved")
  
}else{
  load(file="InitialAnalysis.RData")
  print("Data Fully Loaded")
}

