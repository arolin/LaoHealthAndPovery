HEF_PreID <-lps$PreId & HEFCard;
Geo_Poor  <-!lps$PreId & GVTPoor;
NoAssist  <-lps$PreId & NoInsu;

HEF_PreID_Indv = c();
Geo_Poor_Indv = c();
NoAssist_Indv = c();
for (r in as.integer(IndiHealth$SRow)) {
  HEF_PreID_Indv <-c(HEF_PreID_Indv,HEF_PreID[r]);
  Geo_Poor_Indv  <-c(Geo_Poor_Indv,Geo_Poor[r]);
  NoAssist_Indv  <-c(NoAssist_Indv,NoAssist[r]);
}

lgroups <- list(HEF_PreID,Geo_Poor,NoAssist)
liGroups <- list(HEF_PreID_Indv,Geo_Poor_Indv,NoAssist_Indv)

#create a blank frame to hold the table
ftable <-data.frame();
ttable <-data.frame();
#copute the rows for each group in the table
rtab=c()
for (l in lgroups) {
  rtab<-c(rtab,sum(lps[l,"Gender"]=="F",na.rm=TRUE))  
}
ftable<-rbind(ftable,rtab)

countIndivis <-function (g) {
  v <-sum(g,na.rm = TRUE)
  return (v);
}
percentMarried <- function(g) {
  m <-sum(IndiHealth$Marital_Status[g]=="Married") / sum (g)
}
percentSingle <- function(g) {
  m <-sum(IndiHealth$Marital_Status[g]=="Single") / sum (g)
}
percentDivorced <- function(g) {
  m <-sum(IndiHealth$Marital_Status[g]=="Divorced/separated") / sum (g)
}
percentWidowed <- function(g) {
  m <-sum(IndiHealth$Marital_Status[g]=="Widowed") / sum (g)
}
percentOther <- function(g) {
  m <-sum(IndiHealth$Marital_Status[g]=="Other") / sum (g)
}



familySize <-function (g) {
   v <-mean(lps[g,"HH_Num_People"],na.rm = TRUE)
  return (v);
}


countFemale <-function (g) {
  v <-sum(lps[g,"Gender"]=="F",na.rm = TRUE)
  return (v);
}
countMale <-function (g) {
  v <-sum(lps[g,"Gender"]=="M",na.rm = TRUE)
  return (v);
}
percentMale <-function (g) {
  m <-sum(lps[g,"Gender"]=="M",na.rm = TRUE)
  f <-sum(lps[g,"Gender"]=="F",na.rm = TRUE)
  v<-m/(m+f)
  return (v);
}

countFemaleI <-function (g) {
  v <-sum(IndiHealth[g,"Gender"]=="F",na.rm = TRUE)
  return (v);
}
countMaleI <-function (g) {
  v <-sum(IndiHealth[g,"Gender"]=="M",na.rm = TRUE)
  return (v);
}
percentMaleI <-function (g) {
  m <-sum(IndiHealth[g,"Gender"]=="M",na.rm = TRUE)
  f <-sum(IndiHealth[g,"Gender"]=="F",na.rm = TRUE)
  v<-m/(m+f)
  return (v);
}
avgAge <-function (g) {
 v= mean (IndiHealth[g, "Age"])
 return (v);
}
bindVar <-function(ftable,group,varFunc,name) {
  rtab<-c();
  for (l in group) {
    rtab<-c(rtab,varFunc(l))
  }
  ftable<-rbind(ftable,rtab)
  rownames(ftable)[length(ftable[,1])]=name;
  return(ftable)
}

ftable <-data.frame();

ftable<-bindVar(ftable,liGroups,countIndivis,"Total Individuals");
ftable<-bindVar(ftable,lgroups,countFemale,"Females(HoH)");
ftable<-bindVar(ftable,lgroups,countMale,"Males(HoH)");
ftable<-bindVar(ftable,lgroups,percentMale,"%Males(HoH)");

ftable<-bindVar(ftable,liGroups,countFemaleI,"Females");
ftable<-bindVar(ftable,liGroups,countMaleI,"Males");
ftable<-bindVar(ftable,liGroups,percentMaleI,"%Males");
ftable<-bindVar(ftable,liGroups,percentMarried,"%Married");
ftable<-bindVar(ftable,liGroups,percentSingle,"%Single");
ftable<-bindVar(ftable,liGroups,percentDivorced,"%Divorced");
ftable<-bindVar(ftable,liGroups,percentOther,"%Marital_Other");
ftable<-bindVar(ftable,liGroups,avgAge,"Average_Age");
ftable<-bindVar(ftable,lgroups,familySize,"Average_FamilySize");

t.test(IndiHealth$Marital_Status[HEF_PreID_Indv]=="Married",IndiHealth$Marital_Status[NoAssist_Indv]=="Married")
t.test(IndiHealth$Gender=="F",IndiHealth$Gender[NoAssist_Indv]=="F")

colnames(ftable) <- c("HEF","GEO_Poor","NoAssist")
print(ftable)
print(t(ftable))

