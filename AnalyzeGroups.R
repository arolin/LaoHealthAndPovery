
################################################################################
###Main analysis by groups
###

################################################################################

library("foreign")

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
liGroups <- list(HEF_PreID_Indv,Geo_Poor_Indv,NoAssist_Indv,OnAList_Indv)

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
colnames(demsTable) <- c("HEF","GEO_Poor","NoAssist","All Respondants")


write.foreign(demsTable, "./spss/demsTable.txt", "./spss/demsTable.sps",   package="SPSS") 
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
colnames(demsSigTable) = c("HEF vs Geo", "HEF vs NoAssist", "Geo vs NoAssist")

write.foreign(demsSigTable, "./spss/demsSigTable.txt", "./spss/demsSigTable.sps",   package="SPSS") 
##
################################################################################

################################################################################
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

testHCSPComp <- function (g,b,var){
    sickG  <-(IndiHealth$Illness==1 & g);
    nSick<-sum(sickG)
    sickB  <-(IndiHealth$Illness==1 & b);
    nSick <- sum(sickB)
    group <-IndiHealth[sickG,var];
    used<-sum(!is.na(group))
    
    comp <-IndiHealth[sickB,var];
    used<-sum(!is.na(comp))
    
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

write.foreign(hcSeek, "./spss/hcSeekTable.txt", "./spss/hcSeekTable.sps",   package="SPSS")
write.foreign(hcSeekSig, "./spss/hcSeekSigTable.txt", "./spss/hcSeekSigTable.sps",   package="SPSS")


################################
##Overnight HC


################################################################################
### Paymets
payTable <- data.frame(stringsAsFactors = FALSE);
payTableSpss <- data.frame(stringsAsFactors = FALSE);
paySigTabel <-data.frame();

careCostComp <- function (g,var,flag) {
    g2 <- g & !is.na(flag);
    cv=lps[g2,var];
    
    cv <- cv[cv!=98 & cv!=99]
    if (!is.null(cv) ){
        r$num=0;
    }
    else {
        r$num=sum(!is.na(cv) );
    }
    
    r$max <- max (cv,na.rm = TRUE);
    r$min <- min (cv,na.rm = TRUE);
    r$mean <- mean(cv,na.rm = TRUE);
    r$median <- median(cv,na.rm = TRUE);  
    return (r);  
}

bindCostVars <-function(ftable,group,varFunc,vars,flag,names) {
    for (v in 1:length(vars)) {
        meantab<-c();
        mediantab<-c();
        numtab<-c();
        for (l in group) {
            vf <- varFunc(l,vars[v],flag);
            meantab<-c(,meantab,vf$mean)
            mediantab<-c(mediantab,vf$median)
        }
        ftable<-rbind(ftable,numtab,meantab,mediantab)
        rn<-paste(names[v]," num");
        rownames(ftable)[length(ftable[,1])-2]=rn;
        ftable<-rbind(ftable,meantab,mediantab)
        rn<-paste(names[v]," mean");
        rownames(ftable)[length(ftable[,1])-1]=rn;
        rn<-paste(names[v]," median");
        rownames(ftable)[length(ftable[,1])]=rn;
    }
    return(ftable)
}




##this gets called for each care center (cc) and will add a line for
##each payment class in the list (pcS) - the varFunc function should
##combine all itterations (prefix)
bindCostVars2 <-function(ftable,groups,varFunc,cc,pcS) {
    for (pc in pcS) {
        rtab<-c();
        for (l in groups) {
            vf <- varFunc(l,cc,pc);
            s<-"";
            if (vf$num!=0) {
                s<-paste (format(vf$mean/1000,digits=2),"/",format(vf$median/1000,digits=2),"[",vf$num,"]")
            }else {          
                 s<-paste ("")
             }
            rtab<-c(rtab,s)
        }
        options(stringsAsFactors=FALSE);
        ftable<-rbind(ftable,rtab)
        options(stringsAsFactors=TRUE);
        rownames(ftable)[length(ftable[,1])]=paste(cc,pc,sep="");
    }
    return(ftable)
}

bindCostVarsSPSS <-function(ftable,groups,varFunc,cc,pcS) {
  for (pc in pcS) {
    rtab<-c();
    for (l in groups) {
      vf <- varFunc(l,cc,pc);
      
      if (vf$num!=0) {
        rtab<-c(rtab,vf$mean,vf$median,vf$num)
      }else {          
        rtab<-c(rtab,NA,NA,vf$num)
      }
      
    }
    ftable<-rbind(ftable,rtab)
    rn<-paste(cc,pc,sep="");
    rownames(ftable)[length(ftable[,1])]=rn;
  }
  return(ftable)
}



##takes in a group a care centeter (cc) and payment class (pc) and
##tests all combines results for all prefixes valid for g+cc+pc
careCostComp <- function (g,cc,pc) {

    cv<-c();
    ##build a list for all prefixes
    for (p in prefixes) {
        flag<-paste(p,"Consult_",cc,sep="")
        fn <-which (names(lps)==flag)
        if (length(fn)==0 | is.null(fn)){
          fn<-1;
        }
        atPrefix <- !is.na(lps[,flag]);
        cap <- sum(atPrefix)
        g2= g & atPrefix;
        cap2 <-sum(g2)
        vname<-paste(p,cc,pc,sep="");
        cv<-c(cv,lps[g2,vname])
    }
    cap3=length(cv)
    cv <- cv[cv!=98 & cv!=99]
    if (is.null(cv) ){
        r$num=0;        
    }
    else {
        r$num=sum(!is.na(cv) );
    }
    
    r$max <- max (cv,na.rm = TRUE);
    r$min <- min (cv,na.rm = TRUE);
    r$mean <- mean(cv,na.rm = TRUE);
    r$median <- median(cv,na.rm = TRUE);  
    return (r);  
}





##"Private_Clinic",
careCenters  = c("National_Hospital","Provincial_Hospital","District_Hospital","Health_Center","Health_Volunteer","Traditional_Healer","Private_Clinic","Private_Pharmacist","Religious_Healer");
paymentClass = c("_cost_Medicine","_cost_Medical_Fee","_cost_Transport","_cost_Others","_cost_Overall_average");
prefixes     = c("HH_Illness_1_","HH_Illness_2_","HH_Illness_3_");

payTable <-data.frame();

payTableSPSS <-data.frame();
for (cc in careCenters) {
    payTable<-bindCostVars2(payTable,lgroups,careCostComp,cc,paymentClass)
    
    payTableSPSS<-bindCostVarsSPSS(payTableSPSS,lgroups,careCostComp,cc,paymentClass)
}

colnames(payTable)<-c("Preid_HEF","GEO_Poor","NoAssist","All Respondants")

spssColn<-c()
for (cn in c("Preid_HEF","GEO_Poor","NoAssist","All Respondants")) {
  spssColn<-c(spssColn,paste(cn,"mean"),paste(cn,"median"),paste(cn,"num"))
}
colnames(payTableSPSS)<-spssColn

##############################################################
##Test that all the expected vars exist
fail<-0;
suc <-0;
for (p in prefixes) {
    for (cc in careCenters) {
        for (pc in paymentClass) {
            name<-paste(p,cc,pc,sep="");
            w<-which (names(lps)==name)
            if (length(w)!=1) {
                print(name);
                fail<-fail+1;
            }else {
                 suc<-suc+1;
             }
        }
    }
}
print(suc);
print(fail);

    


## NHsi = which (names(lps)=="HH_Illness_1_National_Hospital_cost_Medicine")
## NHei = which (names(lps)=="HH_Illness_1_National_Hospital_cost_Overall_average")
## payTable<-bindCostVars2(payTable,lgroups,careCostComp,NHsi:NHei,lps$HH_Illness_1_Consult_National_Hospital,names(lps)[NHsi:NHei])
## payTableSpss<-bindCostVars2(payTable,lgroups,careCostComp,NHsi:NHei,lps$HH_Illness_1_Consult_National_Hospital,names(lps)[NHsi:NHei])




