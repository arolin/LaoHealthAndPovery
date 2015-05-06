################################################################################
###Define Principle Groups
HEF_PreID <-lps$PreId & HEFCard;
Geo_Poor  <-!lps$PreId & GVTPoor;
NoAssist  <-lps$PreId & NoInsu;

################################################################################
###Populate the groups back into the table for MWW tests
lps$HEF_PreID<-HEF_PreID;
lps$Geo_Poor<-Geo_Poor;
lps$NoAssist<-NoAssist;
lps$Group<-"None";
lps$Group[HEF_PreID]="PreID";
lps$Group[Geo_Poor]="GeoID";
lps$Group[NoAssist]="NoAssist";

OnAList = HEF_PreID | Geo_Poor | NoAssist;

################################################################################
####Create extensions of the group for use with the individuals tables
HEF_PreID_Indv = c();
Geo_Poor_Indv = c();
NoAssist_Indv = c();
OnAList_Indv  = c();

for (r in as.integer(IndiHealth$SRow)) {
    HEF_PreID_Indv <-c(HEF_PreID_Indv,HEF_PreID[r]);
    Geo_Poor_Indv  <-c(Geo_Poor_Indv,Geo_Poor[r]);
    NoAssist_Indv  <-c(NoAssist_Indv,NoAssist[r]);
    OnAList_Indv   <-c(OnAList_Indv,OnAList[r]);
}

##copy the indvidual table groups into the table for MWW tests
IndiHealth$HEF_PreID <- HEF_PreID_Indv;
IndiHealth$Geo_Poor  <- Geo_Poor_Indv;
IndiHealth$NoAssist  <- NoAssist_Indv;
##
################################################################################

################################################################################
##Create the  basic list of groups for inspection
##create a second versino for inspection into the individuals table
lgroups <- list(HEF_PreID,Geo_Poor,NoAssist,OnAList)
names(lgroups)=c("PreID","GeoID","NoAssist","OnAList")
gNames <- c("PreId_HEF,GEO_HEF,NoAssist,All")
gSigNames <-c("PreId vs Geo", "PreId vs NoAssist", "Geo vs NoAssist")
liGroups <- list(HEF_PreID_Indv,Geo_Poor_Indv,NoAssist_Indv,OnAList_Indv)
