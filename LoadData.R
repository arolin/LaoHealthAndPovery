lpsraw<-read.csv("lps.csv");
lps=lpsraw;

ColNames<-read.csv("Constants/ColNames.csv");
names(lps)<-ColNames$VarName;

#Fix Gender
lps$Gender<-factor(lps$Gender,levels = c("1","2"),labels=c("F","M"));

PossesionNames = c("Video","Mobile","Camera","Television","Bicycle","Motorbike","Car or truck","Tractor","Boat","Motor Boat","N cattle","N buffalo","N horses","N pigs","N goats","N chickens");
DistNames = c("Lamam","Kaleum","Dukjeung","Thateang")
lps$"dist" <- factor(lps$"dist",labels = DistNames);


Villages <- read.csv("Vilages.csv")

VillageNum <-as.integer(Villages$Code.district)*1000 + Villages$Code.village;
Villages<-cbind(Villages,VillageNum)

PreId<-lps$id.format==1;
r=1;
for (i in 1:length(PreId)) {
  vrow <-which (Villages$VillageNum == lps$Village[r]);
  PreId[r]<-Villages$Pre.ID[vrow]==1
  r<-r+1;
}
lps<-cbind (lps,PreId)


lps$VilPoor<-factor(lps$VilPoor,c("1","3"),c("Yes","No"));

HEFCard <-!is.na(lps$HH_Insurance_HEFCard);
GVTPoor <-!is.na(lps$HH_Insurance_GVTPoor);
NoInsu <- !is.na(lps$HH_Insurance_Informal_no_insuranc)
VilPoor <-lps$VilPoor=="Yes";

#Split Dependents
if (!exists("IndiHealth")) {
  #source("SplitDeps.R")
  load("IndiHealth.bin");
}

