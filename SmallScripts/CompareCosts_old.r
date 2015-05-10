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
    r<-list();
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
    cv <- cv[cv!=98 & cv!=99 & cv!=0]

    ##Check that anything is left
    if (sum(!is.na(cv))==0) {
        r$num<-0
        return (r)
    }
    
    if (is.null(cv) ){
        r$num=cap3;        
    }
    else {
        hlim<-max(cv)
        ##hlim=10000
        if(sum(cv[]<hlim)>0) {
            r$hist <- hist(cv[cv[]<hlim],100,plot=FALSE);
        }
        r$num=sum(!is.na(cv) );
    }

    r$max <- max (cv,na.rm = TRUE);
    r$min <- min (cv,na.rm = TRUE);
    r$mean <- mean(cv,na.rm = TRUE);
    r$median <- median(cv,na.rm = TRUE); 
    r$total <-sum(cv,na.rm=TRUE);
    if (length(cv>100)>1) {
        ##r$hist <- hist(cv[cv>100],na.exclude=TRUE,plot=FALSE);
    }
    return (r);  
}


careCostTab <- function (g,cc,pc) {
  
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
  
  r$count<-length(cv)
  r$noResp <-sum(is.na(cv));
  
  cv <- cv[cv!=98 & cv!=99 & cv!=0]
  if (is.null(cv) ){
    r$num=cap3;        
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




bindCostVarsSPSS <-function(ftable,groups,varFunc,cc,pcS) {
  for (pc in pcS) {
    rtab<-c();
    i<-1
    for (l in groups) {
      vf <- varFunc(l,cc,pc);
      
      if (vf$num!=0) {
        rtab<-c(rtab,vf$total,vf$mean,vf$median,vf$num)
      }else {          
        rtab<-c(rtab,0,NA,NA,vf$num)
      }

      i<-i+1;
    }
    ftable<-rbind(ftable,rtab)
    rn<-paste(cc,pc,sep="");
    rownames(ftable)[length(ftable[,1])]=rn;

  }
  return(ftable)
}

buildHists <-function(hlist,groups,varFunc,cc,pcS) {
  for (pc in pcS) {
    rtab<-c();
    i<-1
    for (l in groups) {
      vf <- varFunc(l,cc,pc);
      
      if (vf$num!=0) {
        rtab<-c(rtab,vf$total,vf$mean,vf$median,vf$num)
      }else {          
        rtab<-c(rtab,0,NA,NA,vf$num)
      }
      hname<-paste(names(groups)[i],"_",cc,pc,sep="");
      if(!is.null(vf$hist)){
        vf$hist$xname<-hname;
        hlist[[hname]]<-vf$hist;
      }
      
      i<-i+1;
    }
      
  }
  return(hlist)
}





##"Private_Clinic",
careCenters  = c("National_Hospital","Provincial_Hospital","District_Hospital","Health_Center","Health_Volunteer","Traditional_Healer","Private_Clinic","Private_Pharmacist","Religious_Healer");
NationalCenters  = c("National_Hospital","Provincial_Hospital","District_Hospital","Health_Center","Health_Volunteer");
paymentClass = c("_cost_Medicine","_cost_Medical_Fee","_cost_Transport","_cost_Others","_cost_Overall_average");
paymentClass = c("_cost_Overall_average");

prefixes     = c("HH_Illness_1_","HH_Illness_2_","HH_Illness_3_");

for (g in lgroups){
  gs<-0;
  for (p in prefixes) {
    f<-1:length(g);
    f[]<-FALSE;
    for (cc in careCenters) {
      flag<-paste(p,"Consult_",cc,sep="")
      f[g] <- f[g] | !is.na(lps[g,flag]);
    }
    gs<-gs+sum(f);
  }
  print(gs);
}



payTable <-data.frame();

payTableSPSS <-data.frame();
payTableAvgOnly <-data.frame();

lhist<-list()
for (cc in careCenters) {
    payTable<-bindCostVars2(payTable,lgroups,careCostComp,cc,paymentClass)
    
    payTableSPSS<-bindCostVarsSPSS(payTableSPSS,lgroups,careCostComp,cc,paymentClass)
    lhist<-buildHists(lhist,lgroups,careCostComp,cc,paymentClass)
    payTableAvgOnly <-bindCostVarsSPSS(payTableAvgOnly ,lgroups,careCostComp,cc,c("_cost_Overall_average"))
}

colnames(payTable)<-c("Preid_HEF","GEO_Poor","NoAssist","All Respondants")

spssColn<-c()
for (cn in c("Preid","GEO","No","All")) {
  spssColn<-c(spssColn,paste(cn,"total"),paste(cn,"mean",sep="_"),paste(cn,"median",sep="_"),paste(cn,"num",sep="_"))
}
colnames(payTableSPSS)<-spssColn
colnames(payTableAvgOnly)<-spssColn

write.csv     (format(payTable        ,scientific=FALSE),"./spss/payTablecmp.csv")
write.csv     (format(payTableSPSS    ,scientific=FALSE),"./spss/payTable.csv")
write.csv     (format(payTableAvgOnly ,scientific=FALSE),"./spss/payTableAvgs.csv")

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

