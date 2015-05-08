#1.1. Characteristics of the sample
#
#For all households (N=930) and for the 3 sub-groups (HEF Pre-id, HEF Geographic, Non-HEF)
#
#Section 1: 	Household identification
#
#List of frequency tables for all questions – Demogrphics
#
#Comparison and adequacy with the official national and provincial population figures

demcols <-c("Gender","Age")
ligroups <- lapply(c("PreID","GeoID","NoAssist"),function(X){IndiHealth$Group==X})
ligroups[[4]] =rep(T,length(IndiHealth[,1]));
names(ligroups)<-c("HEF PreID","HEF GeoID","NoAssis","All");

NumHouseHolds   <-sapply(lgroups,sum)
NumIndivids     <-sapply(ligroups,sum)
MeanHHSize      <-NumIndivids/NumHouseHolds
PercentMale     <-sapply(ligroups,function(X){100*sum(IndiHealth$Gender[X]=="M")/sum(X)})
PercentFemale   <-sapply(ligroups,function(X){100*sum(IndiHealth$Gender[X]=="F")/sum(X)})
PercentSingle   <-sapply(ligroups,function(X){100*sum(IndiHealth$Marital_Status[X]=="Single")/sum(X)})
PercentMarried  <-sapply(ligroups,function(X){100*sum(IndiHealth$Marital_Status[X]=="Married")/sum(X)})
PercentDivorced <-sapply(ligroups,function(X){100*sum(IndiHealth$Marital_Status[X]=="Divorced/separated")/sum(X)})
PercentWidowed  <-sapply(ligroups,function(X){100*sum(IndiHealth$Marital_Status[X]=="Widowed")/sum(X)})
MeanAge         <-sapply(ligroups,function(X){mean(IndiHealth$Age[X])})
MedianAge       <-sapply(ligroups,function(X){median(IndiHealth$Age[X])})
Percent_HoH_CantRW      <-sapply(lgroups,function(X){100*sum(lps$HoH_Literacy_Level[X]==1)/sum(X)})
Percent_HoH_CanR        <-sapply(lgroups,function(X){100*sum(lps$HoH_Literacy_Level[X]==2)/sum(X)})
Percent_HoH_CanRW       <-sapply(lgroups,function(X){100*sum(lps$HoH_Literacy_Level[X]==3)/sum(X)})
Percent_HoH_Spouse_CantRW      <-sapply(lgroups,function(X){100*sum(lps$HoH_Spouse_Literacy_Level[X]==1)/sum(X)})
Percent_HoH_Spouse_CanR        <-sapply(lgroups,function(X){100*sum(lps$HoH_Spouse_Literacy_Level[X]==2)/sum(X)})
Percent_HoH_Spouse_CanRW       <-sapply(lgroups,function(X){100*sum(lps$HoH_Spouse_Literacy_Level[X]==3)/sum(X)})

DemogTable <- data.frame(NumHouseHolds,NumIndivids,MeanHHSize,MeanAge,MedianAge,PercentMale,PercentFemale,PercentSingle,PercentMarried,PercentDivorced,PercentWidowed,Percent_HoH_CantRW,Percent_HoH_CanR,Percent_HoH_CanRW,Percent_HoH_Spouse_CantRW,Percent_HoH_Spouse_CanR,Percent_HoH_Spouse_CanRW)

precentRows<-c("PercentMale","PercentFemale","PercentSingle","PercentMarried","PercentDivorced","PercentWidowed","Percent_HoH_CantRW","Percent_HoH_CanR","Percent_HoH_CanRW","Percent_HoH_Spouse_CantRW","Percent_HoH_Spouse_CanR","Percent_HoH_Spouse_CanRW")
write.csv(DemoGraphicsTable,"./output/Demographics.csv")

ftable<-DemogTable;
ftable$NumHouseHolds<-format(ftable$NumHouseHolds,digits=0,format="d",bigmark=",")
ftable$NumIndivids  <-format(ftable$NumIndivids  ,digits=0,format="d",bigmark=",")
ftable$MeanHHSize   <-sprintf(ftable$MeanHHSize   ,fmt="%.1f")
ftable$MeanAge      <-sprintf(ftable$MeanAge    ,fmt="%.1f")
ftable$MedianAge    <-format(ftable$MedianAge    ,digits=0)
ftable[,percentRows]<-lapply(percentRows,function(X){sapply(ftable[,X],sprintf,fmt="%.1f%%")})

write.csv(ftable,"./output/DemogTable.csv")

DemogTableX<-xtable(t(ftable),caption="Population Demographics")
cat(print(DemogTableX),file="./tex/DemogTable.tex")
