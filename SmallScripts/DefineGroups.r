################################################################################
###Define Principle Groups
## HEF_PreID <-lps$PreId & HEFCard;
## Geo_Poor  <-!lps$PreId & GVTPoor;
## NoAssist  <-lps$PreId & !HEFCard;


########################3
##Real Group Definietions
source("./SmallScripts/Utility.r")
################################################################################
###Populate the groups back into the table for MWW tests
lps$Group<-factor(lps$Group,levels=c(1,2,3),labels=c("PreID","GeoID","NoAssist"))
HEF_PreID<-lps$Group=="PreID";
HEF_GeoID<-lps$Group=="GeoID";
No_Assist<-lps$Group=="NoAssist";
All<-rep(T,length(HEF_PreID));
lgroups <- list(HEF_PreID,HEF_GeoID,No_Assist,All)
names(lgroups) <- c("PreID","GeoID","No Assist","All")

lgNames <-  interleave(names(lgroups),rep("",length(lgroups)))

NumGroups <- sapply(lgroups,sum)

ligroups <- sapply(c(1,2,3),function(X){IndiHealth$Group==X})
ligroups <- cbind.data.frame(ligroups,rep(T,length(IndiHealth[,1])));
names(ligroups)<-c("HEF PreID","HEF GeoID","No Assist","All");
ligroups

lps$Ethnic_group<-lps$HoH_Ethnic_group
lps$Ethnic_group[lps$HoH_Ethnic_group==6]<-lps$HoH_Ethnic_group_other[lps$HoH_Ethnic_group==6]
ethnicCode<-c(1,2,3,4,5,9,21,23,24,25,29,32,34)
ethnicName <- c("Lao","Alak","Katu","Trieng","Hmong","keumu","Yru","Taoey","Yae","Brao","Krieng","Xuay","Lav")
lps$Ethnic_group<-factor(lps$Ethnic_group,levels=ethnicCode,labels=ethnicName)


################################################################################
####Create extensions of the group for use with the individuals tables
## HEF_PreID_Indv = c();
## Geo_Poor_Indv = c();
## NoAssist_Indv = c();
## OnAList_Indv  = c();

## for (r in as.integer(IndiHealth$SRow)) {
##     HEF_PreID_Indv <-c(HEF_PreID_Indv,HEF_PreID[r]);
##     Geo_Poor_Indv  <-c(Geo_Poor_Indv,Geo_Poor[r]);
##     NoAssist_Indv  <-c(NoAssist_Indv,NoAssist[r]);
##     OnAList_Indv   <-c(OnAList_Indv,OnAList[r]);
## }

## ##copy the indvidual table groups into the table for MWW tests
## IndiHealth$HEF_PreID <- HEF_PreID_Indv;
## IndiHealth$Geo_Poor  <- Geo_Poor_Indv;
## IndiHealth$NoAssist  <- NoAssist_Indv;
## ##
## ################################################################################

## ################################################################################
## ##Create the  basic list of groups for inspection
## ##create a second versino for inspection into the individuals table
## lgroups <- list(HEF_PreID,Geo_Poor,NoAssist,OnAList)
## names(lgroups)=c("PreID","GeoID","NoAssist","OnAList")
## gNames <- c("PreId_HEF,GEO_HEF,NoAssist,All")
## gSigNames <-c("PreId vs Geo", "PreId vs NoAssist", "Geo vs NoAssist")
## liGroups <- list(HEF_PreID_Indv,Geo_Poor_Indv,NoAssist_Indv,OnAList_Indv)


