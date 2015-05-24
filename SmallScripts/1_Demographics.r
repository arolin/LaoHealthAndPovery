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
ligroups <- sapply(c(1,2,3),function(X){IndiHealth$Group==X})
ligroups <- cbind.data.frame(ligroups,rep(T,length(IndiHealth[,1])));
names(ligroups)<-c("HEF PreID","HEF GeoID","NoAssis","All");
ligroups

NumHouseHolds   <-sapply(lgroups,sum)
print("Num Households")
print(NumHouseHolds)
NumIndivids     <-colSums(ligroups)
print("Num Individuals")
print(NumIndivids)

MeanHHSize      <-NumIndivids/NumHouseHolds
PercentMale     <-sapply(ligroups,function(X){sum(IndiHealth$Gender[X]=="M")/sum(X)})
PercentFemale   <-sapply(ligroups,function(X){sum(IndiHealth$Gender[X]=="F")/sum(X)})
PercentSingle   <-sapply(ligroups,function(X){sum(IndiHealth$Marital_Status[X]=="Single")/sum(X)})
PercentMarried  <-sapply(ligroups,function(X){sum(IndiHealth$Marital_Status[X]=="Married")/sum(X)})
PercentDivorced <-sapply(ligroups,function(X){sum(IndiHealth$Marital_Status[X]=="Divorced/separated")/sum(X)})
PercentWidowed  <-sapply(ligroups,function(X){sum(IndiHealth$Marital_Status[X]=="Widowed")/sum(X)})
MeanAge         <-sapply(ligroups,function(X){mean(IndiHealth$Age[X])})
MedianAge       <-sapply(ligroups,function(X){median(IndiHealth$Age[X])})
Percent_HoH_CantRW      <-sapply(lgroups,function(X){sum(lps$HoH_Literacy_Level[X]==1)/sum(X)})
Percent_HoH_CanR        <-sapply(lgroups,function(X){sum(lps$HoH_Literacy_Level[X]==2)/sum(X)})
Percent_HoH_CanRW       <-sapply(lgroups,function(X){sum(lps$HoH_Literacy_Level[X]==3)/sum(X)})
Percent_HoH_Spouse_CantRW      <-sapply(lgroups,function(X){sum(lps$HoH_Spouse_Literacy_Level[X]==1)/sum(X)})
Percent_HoH_Spouse_CanR        <-sapply(lgroups,function(X){sum(lps$HoH_Spouse_Literacy_Level[X]==2)/sum(X)})
Percent_HoH_Spouse_CanRW       <-sapply(lgroups,function(X){sum(lps$HoH_Spouse_Literacy_Level[X]==3)/sum(X)})
PercentMale                <- FmtPer( PercentMale              )
PercentFemale              <- FmtPer( PercentFemale            )
PercentSingle              <- FmtPer( PercentSingle            )
PercentMarried             <- FmtPer( PercentMarried           )
PercentDivorced            <- FmtPer( PercentDivorced          )
PercentWidowed             <- FmtPer( PercentWidowed           )
Percent_HoH_CantRW         <- FmtPer( Percent_HoH_CantRW       )
Percent_HoH_CanR           <- FmtPer( Percent_HoH_CanR         )
Percent_HoH_CanRW          <- FmtPer( Percent_HoH_CanRW        )
Percent_HoH_Spouse_CantRW  <- FmtPer( Percent_HoH_Spouse_CantRW)
Percent_HoH_Spouse_CanR    <- FmtPer( Percent_HoH_Spouse_CanR  )
Percent_HoH_Spouse_CanRW   <- FmtPer( Percent_HoH_Spouse_CanRW )
DemogTable <- rbind(NumHouseHolds,NumIndivids,MeanHHSize,MeanAge,MedianAge,PercentMale,PercentFemale,PercentSingle,PercentMarried,PercentDivorced,PercentWidowed,Percent_HoH_CantRW,Percent_HoH_CanR,Percent_HoH_CanRW,Percent_HoH_Spouse_CantRW,Percent_HoH_Spouse_CanR,Percent_HoH_Spouse_CanRW)
rownames(DemogTable) <- c("Num Households","Num Individuals","Mean HH Size","Mean Age","Median Age","% Males","% Females","% Single","% Married","% Divorced","% Widowed","% HoH Can't R/W","% HoH Can Read","% HoH Can R/W","% HoH Spouse Can't R/W","% HoH Spouse Can Read","% HoH Spouse Can R/W");
DemogTable["Num Households",]<-format  (DemogTable["Num Households",],digits=0,format="d",bigmark=",")
DemogTable["Num Individuals",]  <-format  (DemogTable["Num Individuals",]  ,digits=0,format="d",bigmark=",")
DemogTable["Mean HH Size",]   <-sprintf (as.numeric(DemogTable["Mean HH Size",])   ,fmt="%.1f")
DemogTable["Mean Age",]      <-sprintf (as.numeric(DemogTable["Mean Age",])    ,fmt="%.1f")
DemogTable["Median Age",]    <-format  (as.numeric(DemogTable["Median Age",])    ,digits=0)
SaveTables(DemogTable,"DemogTable","Population Demographics")
print(DemogTable)


Ethnicity <- t(sapply(levels(factor(lps$Ethnic_group)),function(X){sapply(lgroups,function(G){sum(lps[G,"Ethnic_group"]==X,na.rm=T)})}))
Ethnicity <- PercentifyTable(Ethnicity)
## write.csv(Ethnicity,file="./output/Ethnicity.csv");
## cat(print(xtable(Ethnicity,caption="Ethnic Makeup")),file="./tex/EthnicMakeup.tex")
SaveTables(Ethnicity,"EthnicMakeup","Ethnic Distribution of Sampled Households")
print(Ethnicity)
