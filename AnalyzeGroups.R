HEF_PreID <-lps$PreId & HEFCard;
Geo_Poor  <-!lps$PreId & GVTPoor;
NoAssist  <-lps$PreId & NoInsu;

lps$HEF_PreID<-HEF_PreID;
lps$Geo_Poor<-Geo_Poor;
lps$NoAssist<-NoAssist;
OnAList = HEF_PreID | Geo_Poor | NoAssist;


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

IndiHealth$HEF_PreID <- HEF_PreID_Indv;
IndiHealth$Geo_Poor  <- Geo_Poor_Indv;
IndiHealth$NoAssist  <- NoAssist_Indv;




lgroups <- list(HEF_PreID,Geo_Poor,NoAssist,OnAList)
liGroups <- list(HEF_PreID_Indv,Geo_Poor_Indv,NoAssist_Indv,OnAList_Indv)

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
   v <-mean(lps[g,"HH_NumPeople"],na.rm = TRUE)
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

hohLit <-function (g) {
  avg<-mean(lps$HoH_Literacy_Level[g]);
  return (avg);
}

spouseLit <- function (g) {
  fours <-lps$HoH_Spouse_Literacy_Level==4;
  who<-(g & !(fours));
  avg <- mean(lps$HoH_Spouse_Literacy_Level[who]);
  return (avg);
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
ftable<-bindVar(ftable,lgroups,hohLit,"HoH Litteracy");
ftable<-bindVar(ftable,lgroups,spouseLit,"Spouse Literacy");



t.test(IndiHealth$Marital_Status[HEF_PreID_Indv]=="Married",IndiHealth$Marital_Status[NoAssist_Indv]=="Married")
t.test(IndiHealth$Gender=="F",IndiHealth$Gender[NoAssist_Indv]=="F")

colnames(ftable) <- c("HEF","GEO_Poor","NoAssist","All Respondants")
print(ftable)
print(t(ftable))


sigTable = data.frame();

testHoHLit <- function (g,b) {
  wt<-wilcox.test(reformulate(termlabels=g,response='HoH_Literacy_Level'),data=lps[b,])
  return(wt$p.value)
}

testNumPeople <-function (g,b) {
  wt<-wilcox.test(reformulate(termlabels=g,response='HH_NumPeople'),data=lps[b,])
  return(wt$p.value)
}


testAge <-function (g,b) {
  wt<-wilcox.test(reformulate(termlabels=g,response='Age'),data=IndiHealth[b,])
  return(wt$p.value)
}


cases=c('HEF_PreID','HEF_PreID','Geo_Poor')
pops_survey = list (lps$HEF_PreID|lps$Geo_Poor,lps$HEF_PreID|lps$NoAssist,lps$Geo_Poor|lps$NoAssist)
pops_idv    = list (IndiHealth$HEF_PreID|IndiHealth$Geo_Poor,IndiHealth$HEF_PreID|IndiHealth$NoAssist,IndiHealth$Geo_Poor|IndiHealth$NoAssist)
bindSig <-function(sig,varFunc,name,p) {
  rtab<-c();
  for (i in 1:3) {
    rtab<-c(rtab,varFunc(cases[i],p[[i]]))
  }
  sig<-rbind(sig,rtab)
  rownames(sig)[length(sig[,1])]=name;
  return(sig)
}

sigTable<-bindSig(sigTable,testAge, "Avg HH Age",pops_idv)
sigTable<-bindSig(sigTable,testNumPeople,"Household Size",pops_survey)
sigTable<-bindSig(sigTable,testHoHLit,"HoH Literacy",pops_survey)
colnames(sigTable) = c("HEF vs Geo", "HEF vs NoAssist", "Geo vs NoAssist")
print(sigTable)

## Healthcare Seeking Behavior
hcSeek <- data.frame();
hcSeekSig <-data.frame();

sickLastYear <-function (g) {
  m <-100*sum(IndiHealth$Illness[g]=="1") / sum (g)
  return (m)
}

sickTimesYear <-function (g) {
  m <- mean(IndiHealth$NumIllnesses[g],na.rm = TRUE)
  return(m)
}


testSickLastYear <- function(g,b) {
  grp<-IndiHealth[,g];
  grp2 <-b & !grp;
  
  print (c(sum(grp), sum(IndiHealth$Illness[grp]==1),sum(grp2),sum(IndiHealth$Illness[grp2]==1)))
  tr<-t.test(IndiHealth$Illness[grp]==1,IndiHealth$Illness[grp2]==1)
  return(tr$p.value)
}

testSickTimesYear <- function(g,b) {
  wt<-wilcox.test(reformulate(termlabels=g,response='NumIllnesses'),data=IndiHealth[b,])
  return (wt$p.value)
}

#compute a percnet for a given variable from the invididuales table
#specific to the number of sick people
hcsPComp <- function (g,var) {
    cv=IndiHealth[g,var];
    cv = !is.na(cv);
    cv= sum(cv);
    dv=sum(IndiHealth$Illness[g]==1);
    return (100*cv/dv);  
}

bindCVars <-function(ftable,group,varFunc,vars,names) {
  
  for (v in 1:length(vars)) {
    rtab<-c();
    for (l in group) {
      rtab<-c(rtab,varFunc(l,vars[v]))
    }
    ftable<-rbind(ftable,rtab)
    rn<-names[v];
    rownames(ftable)[length(ftable[,1])]=rn;
  }
  return(ftable)
}

testHCSPComp <- function (g,b,var){
  sickG  <-IndiHealth$Illness[g]==1;
  sickB  <-IndiHealth$Illness[b]==1;
  group <-IndiHealth[sickG,var];
  comp <-IndiHealth[sickB,var];
  tr<-t.test(!is.na(group),!is.na(comp));
  return(tr$p.value)
}
bindSigVarsT <-function(sig,varFunc,varNames,name,groups) {
  for(v in 1:length(varNames)) {
    rtab <- c();
    for (i in 1:length(groups)) {
      if (i<length(groups)) {
        for (n in (i+1):3) {
          rtab<-c(rtab,varFunc(groups[[i]],groups[[n]],varNames[v]))
        }
      }
    }
    sig<-rbind(sig,rtab)
    rownames(sig)[length(sig[,1])]=name[v];
  }
  return(sig)
}

careTypes<-c("Care At- No care","Care At- Home-made medicine","Care At- Village modern medical practitioner","Care At- Village health volunteer","Care At- Traditional healer","Care At- Health centre","Care At- District hospital","Care At- Provincial hospital/Regional","Care At","Care At - National hospital","Care At - Private pharmacy","Care At - Private clinic","Care At - Abroad","Care At - Illegal medical practitioner","Care At - Other","Care At - Do not remember","Care At - Do not know","Care At - Other specify")


hcSeek<-bindVar(hcSeek,liGroups,sickLastYear,"Sick last year");
hcSeek<-bindVar(hcSeek,liGroups,sickTimesYear,"Sick times last year");
hcSeek<-bindCVars(hcSeek,liGroups,hcsPComp,careTypes,careTypes)
hcSeekSig<-bindSig(hcSeekSig,testSickLastYear, "Sick last year",pops_idv)
hcSeekSig<-bindSig(hcSeekSig,testSickTimesYear, "Sick times last year",pops_idv)
hcSeekSig<-bindSigVarsT(hcSeekSig,testHCSPComp,careTypes,careTypes,liGroups[1:3])

colnames(hcSeek) <- c("HEF","GEO_Poor","NoAssist","All Respondants")

colnames(hcSeekSig) = c("HEF vs Geo", "HEF vs NoAssist", "Geo vs NoAssist")
print(hcSeek)
print(hcSeekSig)

