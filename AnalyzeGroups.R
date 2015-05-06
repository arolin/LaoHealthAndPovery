
################################################################################
###Main analysis by groups
###

################################################################################

library("foreign")
source("./SmallScripts/DefineGroups.r")

################################################################################
################################################################################
##***Demographic Analysis



##create a blank frame to hold the table
demsTable <-data.frame();
ttable <-data.frame();

##copute the rows for each group in the table
rtab=c()

##Gender computed "by hand" and added to the table
for (l in lgroups) {
    rtab<-c(rtab,sum(lps[l,"Gender"]=="F",na.rm=TRUE))  
}
demsTable<-rbind(demsTable,rtab)

##define a function to call to bind a row to a tavle by group
##Pass in the table, groups list, function to compute variable and name of row
bindVar <-function(fTable,group,varFunc,name) {
    rtab<-c();
    for (l in group) {
        rtab<-c(rtab,varFunc(l))
    }
    fTable<-rbind(fTable,rtab)
    rownames(fTable)[length(fTable[,1])]=name;
    return(fTable)
}


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

################################################################################
##count in the Individuals table
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

canRW <-function (g) {
  pct<-sum(lps$HoH_Literacy_Level[g]==3)/sum(g);
  return (pct);
}

canR<-function (g) {
  pct<-sum(lps$HoH_Literacy_Level[g]==2)/sum(g);
  return (pct);
}
cantRW<-function (g) {
  pct<-sum(lps$HoH_Literacy_Level[g]==1)/sum(g);
  return (pct);
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

##build the demographics table
demsTable <-data.frame();

demsTable<-bindVar(demsTable,liGroups,countIndivis    ,"Total Individuals"  );
demsTable<-bindVar(demsTable,lgroups,countFemale      ,"Females(HoH)"       );
demsTable<-bindVar(demsTable,lgroups,countMale        ,"Males(HoH)"         );
demsTable<-bindVar(demsTable,lgroups,percentMale      ,"%Males(HoH)"        );

demsTable<-bindVar(demsTable,liGroups,countFemaleI    ,"Females"            );
demsTable<-bindVar(demsTable,liGroups,countMaleI      ,"Males"              );
demsTable<-bindVar(demsTable,liGroups,percentMaleI    ,"%Males"             );
demsTable<-bindVar(demsTable,liGroups,percentMarried  ,"%Married"           );
demsTable<-bindVar(demsTable,liGroups,percentSingle   ,"%Single"            );
demsTable<-bindVar(demsTable,liGroups,percentDivorced ,"%Divorced"          );
demsTable<-bindVar(demsTable,liGroups,percentOther    ,"%Marital_Other"     );
demsTable<-bindVar(demsTable,liGroups,avgAge          ,"Average_Age"        );
demsTable<-bindVar(demsTable,lgroups,familySize       ,"Average_FamilySize" );
demsTable<-bindVar(demsTable,lgroups,hohLit           ,"HoH Litteracy"      );
demsTable<-bindVar(demsTable,lgroups,spouseLit        ,"Spouse Literacy"    );

##copy the column names to the table
colnames(demsTable) <- gNames

litTable<-data.frame();

litTable<-bindVar(litTable,lgroups,canRW           ,"HoH Can Read Write %"      );
litTable<-bindVar(litTable,lgroups,canR           ,"HoH Can Read %"      );
litTable<-bindVar(litTable,lgroups,cantRW           ,"HoH Can Not Read Write %"      );
colnames(litTable) <- gNames

write.csv(litTable,"./spss/litTable.csv");
write.csv(demsTable,"./spss/demsTable.csv");

##Demographics table Complete
################################################################################

################################################################################
##Comepute demographic significance table
demsSigTable = data.frame();

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

demsSigTable<-bindSig(demsSigTable,testAge, "Avg HH Age",pops_idv)
demsSigTable<-bindSig(demsSigTable,testNumPeople,"Household Size",pops_survey)
demsSigTable<-bindSig(demsSigTable,testHoHLit,"HoH Literacy",pops_survey)
colnames(demsSigTable) <- gSigNames


write.csv(demsSigTable,"./spss/demsSigTable.csv")
##
################################################################################

################################################################################
## Healthcare Seeking Behavior

##create to empty frames
hcSeek    <- data.frame();
hcSeekSig <- data.frame();

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

##compute a percnet for a given variable from the invididuales table
##specific to the number of sick people
hcsPComp <- function (g,var) {
    cv=IndiHealth[g,var];
    if (!is.null(cv)) {
        cv = !is.na(cv);
        cv= sum(cv);
    }
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

testHCSickComp <- function (g,b,var){
    tr<-t.test(IndiHealth$Illness[g]==1,IndiHealth$Illness[b]==1);
    return(tr$p.value)
}

##compare two groups for illness significance
testHCSPComp <- function (g,b,var){
    ##get people in group who reported illness
    sickG  <-(IndiHealth$Illness==1 & g);
    #count these people
    nSick<-sum(sickG)
    ##get people in base line who were sic
    sickB  <-(IndiHealth$Illness==1 & b);
    nSick <- sum(sickB)
    
    ##get the specific variable of interest
    group <-IndiHealth[sickG,var];
    comp <-IndiHealth[sickB,var];
    if (length(group)>0 && length(comp) >0) {
        ##print("Zero Lenght group!") 
        if (length(!is.na(group))>0 && length(!is.na(comp)>0)) {
            ##do a ttest on both groups
            tr<-t.test(!is.na(group),!is.na(comp));
            return(tr$p.value)
        }
        else {
            breakMeHere<-0;
            return(999)
        }
    }
    
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


write.csv(hcSeek,"./spss/hsSeekTable.csv");
write.csv(hcSeekSig,"./spss/hsSeekSigTable.csv");


################################
##Overnight HC



    


## NHsi = which (names(lps)=="HH_Illness_1_National_Hospital_cost_Medicine")
## NHei = which (names(lps)=="HH_Illness_1_National_Hospital_cost_Overall_average")
## payTable<-bindCostVars2(payTable,lgroups,careCostComp,NHsi:NHei,lps$HH_Illness_1_Consult_National_Hospital,names(lps)[NHsi:NHei])
## payTableSpss<-bindCostVars2(payTable,lgroups,careCostComp,NHsi:NHei,lps$HH_Illness_1_Consult_National_Hospital,names(lps)[NHsi:NHei])




