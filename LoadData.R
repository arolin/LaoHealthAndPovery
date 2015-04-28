lpsraw<-read.csv("lps.csv");
lps<-lpsraw;

#Fix Gender
lps$"q1_4"<-factor(lps$"q1_4",levels = c("1","2"),labels=c("F","M"));

CNames = c("Video","Mobile","Camera","Television","Bicycle","Motorbike","Car or truck","Tractor","Boat","Motor Boat","N cattle","N buffalo","N horses","N pigs","N goats","N chickens");
DistNames = c("Lamam","Kaleum","Dukjeung","Thateang")
lps$"dist" <- factor(lps$"dist",labels = DistNames);

#Split Dependents
source("SplitDeps.R")

