##Need to determine:
## How many people sought treatment
## How many treatment options per person
## Do any treatment options not include the estimate field
## How many treatments include 0 in the estimate field
## How many treatments include 98 99 in the estimate field

## How many people sought treatment
## How many questions had atleast 1 yes answer
source ("./SmallScripts/Utility.r")
##"Private_Clinic",
careCenters  = c("National_Hospital","Provincial_Hospital","District_Hospital","Health_Center","Health_Volunteer","Traditional_Healer","Private_Clinic","Private_Pharmacist","Religious_Healer");
NationalCenters  = c("National_Hospital","Provincial_Hospital","District_Hospital","Health_Center","Health_Volunteer");
paymentClass = c("_cost_Medicine","_cost_Medical_Fee","_cost_Transport","_cost_Others","_cost_Overall_average");

prefixes     = c("HH_Illness_1_","HH_Illness_2_","HH_Illness_3_");




ConsultTableExtract <- function (prefixes) {
  paymentClass = c("Medicine","Medical_Fee","Transport","Others","Overall_average");
  fnames =c ("Flag","Serial","Group","ID",paymentClass,"CareCenter");
  newTab <-data.frame();
  names <-NULL;
  for (prefix in prefixes) {
    for (cc in careCenters) {
      flag<-paste(prefix,"Consult_",cc,sep="");
      payFields<-sapply(paymentClass,function(X){paste(prefix,cc,"_cost_",X,sep="")});
      names<-c(names,payFields);
      ID<-paste(prefix,"report_individual_number",sep="");
      fields<-c(flag,"Serial","Group",ID,payFields); 
      #lapply(fields,function(X){if (length(which(names(lps)==X)[1]==0)) {print(X)}})
      careEvents = lps[,fields];
      #careEvents = careEvents[!is.na(careEvents[1]),];
      
      careEvents = cbind.data.frame (careEvents,cc,);
      #print (dim(careEvents))
      colnames(careEvents)<-fnames;
      newTab<-rbind.data.frame (newTab,careEvents)
      
    }
  }
  
  newTab<-newTab[!is.na(newTab[,1]),]
  for (pc in paymentClass) {
    newTab[,pc]<-as.numeric(gsub(",","",newTab[,pc]))
  }
  return (newTab);
}

OutPatCostTable <- ConsultTableExtract(c("HH_Illness_1_"))
for (i in c(96,238,883)){
  OutPatCostTable <- OutPatCostTable[OutPatCostTable$Serial!=i,]
}
OutPatGroups <- sapply(c("PreID","GeoID","NoAssist"),function(G){OutPatCostTable$Group==G})
OutPatGroups <- cbind.data.frame(OutPatGroups,All=T)
sapply(OutPatGroups,sum)
OutPatCostTable$TotalCost <- lps$HH_Illness_1_Total_cost_Overall_average[OutPatCostTable$Serial]


col <- paste(prefix,CCs,"_cost_","Overall_average",sep="")
OutPatCostTable$SumOfTotals <- rowSums(lps[,col],na.rm=T)[OutPatCostTable$Serial]


RepIndiv <- data.frame(matrix(nrow=length(OutPatCostTable[,1]),ncol=length(IndiHealth[1,])))
colnames(RepIndiv) <- colnames(IndiHealth)
for (i in 1:length(OutPatCostTable[,1]) ) {
  srow <- which(IndiHealth$SRow==OutPatCostTable$Serial[i])
  srow <- srow[  which(IndiHealth$SN[srow]==OutPatCostTable$ID[i] )]
  RepIndiv[i,] <- IndiHealth[srow,]
}
  
                               
OutPatCostTable <- cbind(OutPatCostTable,RepIndiv)
  

InPatCostTable1 <- ConsultTableExtract(c("HH_Illness_2_"))
for (i in c(96,238,883)){
  InPatCostTable <- InPatCostTable[InPatCostTable$Serial!=i,]
}
InPatGroups <- sapply(c("PreID","GeoID","NoAssist"),function(G){InPatCostTable$Group==G})
InPatGroups <- cbind.data.frame(InPatGroups,All=T)
out <- findOutliers(InPatCostTable$Overall_average,3)
InPatCostTable[out,]
sapply(InPatGroups,sum)

InPatCostTable2 <- ConsultTableExtract(c("HH_Illness_3_"))
for (i in c(96,238,883)){
  InPatCostTable2 <- InPatCostTable2[InPatCostTable2$Serial!=i,]
}
InPatGroups2 <- sapply(c("PreID","GeoID","NoAssist"),function(G){InPatCostTable2$Group==G})
#Analyse the maximum values and remove the extremes: row 97 and 239 and certainly 884 (check)
InPatGroups2 <- cbind.data.frame(InPatGroups2,All=T)
sapply(InPatGroups2,sum)


recinded_events <- c(96,238,327)
InPatCostTable <- ConsultTableExtract(c("HH_Illness_2_","HH_Illness_3_"))
for (i in recinded_events){
  InPatCostTable <- InPatCostTable[InPatCostTable$Serial!=i,]
}
InPatCostTableAll  <- InPatCostTable
InPatGroups <- sapply(c("PreID","GeoID","NoAssist"),function(G){InPatCostTable$Group==G})
                                        #Analyse the maximum values and remove the extremes: row 97 and 239 and certainly 884 (check)
InPatGroups <- cbind.data.frame(InPatGroups,All=T)
sapply(InPatGroups,sum)
InPatGroupsAll <- InPatGroups





buildFlags <- function (prefixes = prefixes, careCenter=careCenters) {
    flags <- c();
    for (prefix in prefixes) {
        for (cc in careCenter) {
            flag<-paste(prefix,"Consult_",cc,sep="");
            flags<-c(flags,flag);
        }
    }
    return(flags);
    
}
                   
nAnswers <- vector (mode="numeric",length(lps[,1]))
nTreatments <-c();
nTreatmentsInList <-c();
allReports<-c();
#p is a prexix 1-3 for each possible report in a survery
for (p in prefixes) {
    #Pick of the flags for weather a treament happend
    flags <- buildFlags(p,careCenters);
    reports = !is.na(lps[,flags]);
    
    #The number of treatents for a response is the sum of the flags
    nTreat<-rowSums(reports);
    
    #An answer existis if any flag is set
    nAnswers<-nAnswers + 1*(nTreat>0);
    
    nTreatments<-c(nTreatments,nTreat[nTreat>0]);
    nTreatmentsInList <- c(nTreatments,nTreat[nTreat>0])
    allReports <-rbind(allReports,reports);
}

nAllTreatments<-rowSums(allReports)


       
## How many treatment options per person
## Do any treatment options not include the estimate field
CountReport <- function (payType,group=TRUE, prefixs = prefixes, careCenter=careCenters,sdl=-1) {
    counts<-list();
    counts$numNA  =0;
    counts$zeros  =0;
    counts$is9899 =0;
    counts$total  =0;
    counts$inTotal=0;
    counts$numTreated = 0;
    counts$numOutliers = 0;
    grandTotal <-0;
    counts$rdata<-data.frame(stringsAsFactors=T);
    
    for (prefix in prefixs) {
        for (cc in careCenter) {
            flag<-paste(prefix,"Consult_",cc,sep="");
            persons<-lps[group,];
            
            hadCare=!is.na(persons[,flag]);
            grandTotal = grandTotal+sum(hadCare);

    
            payField<-paste(prefix,cc,payType,sep="");
            payments <- persons[hadCare,payField];
            #print("group");print(group);
            #print("hadcare");print(hadCare);
            counts$rdata<-rbind.data.frame(counts$rdata,cbind.data.frame(lps$Group[group][hadCare],payments));
        }#for each care center
    }

    payments <- as.numeric(as.vector(counts$rdata[,2]));
    counts$numTreated = grandTotal;
    
    if (sdl>0) {
      outLiers = findOutliers( payments,sdl)
      counts$rdata<-cbind.data.frame(counts$rdata,outLiers);
      payments<-payments[!outLiers]
      counts$numOutliers = counts$numOutliers + sum(outLiers)
    }else {
      #print(counts$rdata)
      counts$rdata<-cbind.data.frame(counts$rdata,F);
    }
    dontRemb = payments==98 | payments==99;
    
    counts$numNA   = counts$numNA   + sum(is.na(payments));
    payments       = payments[!is.na(payments)];
    counts$zeros   = sum(payments==0,na.rm=TRUE);
    counts$is9899  = sum(dontRemb, na.rm=TRUE);
    counts$total   = sum(payments[!dontRemb],na.rm=TRUE);
    counts$inTotal = length (is.na(payments))-counts$is9899;


    counts$numTreated  = counts$numTreated + sum(hadCare);

    names(counts$rdata)<-c("Group","Amount","outLiers");
    return(counts);
}

ConsultationSummary <- function (paytype,carecenters,sdl=-1) {
    
    SumTab=NULL;
    for (g in lgroups) {
      
        cr <- CountReport(group = g,payType = paytype,careCenter=carecenters,sdl=sdl)

        if (sdl>0) {
          col <-c(cr$numOutliers,cr$numNA,cr$zeros,cr$zeros/cr$numTreated,cr$is9899,cr$is9899/cr$numTreated,cr$inTotal,cr$numTreated,cr$total,cr$total/cr$inTotal)
        }else {
          col <-c(               cr$numNA,cr$zeros,cr$zeros/cr$numTreated,cr$is9899,cr$is9899/cr$numTreated,cr$inTotal,cr$numTreated,cr$total,cr$total/cr$inTotal)
        }
        SumTab<-cbind.data.frame(SumTab,col);

    }

    
    if (sdl>0) {
      rownames(SumTab)<-c("Num outliers removed",
                          "Num no payment info",
                          "Num responding 0",
                          "% responding 0",
                          "Num responding don't know",
                          "% responding don't know",
                          "Num in total",
                          "Total treated",
                          "Total expenditure",
                          "Average expenditure");
    }else {
      rownames(SumTab)<-c("Num no payment info",
                          "Num responding 0",
                          "% responding 0",
                          "Num responding don't know",
                          "% responding don't know",
                          "Num in total",
                          "Total treated",
                          "Total expenditure",
                          "Average expenditure");
    }
        
    colnames(SumTab) = names(lgroups);


    moneyRows<-c("Average expenditure","Total expenditure");
    percentRows <-c("% responding 0","% responding don't know");
    highlightRows <-c("% responding 0","Average expenditure");
    highlightRows <- as.vector(sapply(highlightRows,function(X){return(which(rownames(SumTab)==X))}));
    
    ftable <-as.data.frame(SumTab);
    ftable[percentRows,]<-100*ftable[percentRows,];
    if (sdl>0) {
      ftable <-as.data.frame(lapply(ftable,sprintf,fmt=c("%.0f","%.0f","%.0f","%.1f%%","%.0f","%.1f%%","%.0f","%.0f","%.0f","%.0f")),stringsAsFactors=FALSE);
    }else{
      ftable <-as.data.frame(lapply(ftable,sprintf,fmt=c("%.0f","%.0f","%.1f%%","%.0f","%.1f%%","%.0f","%.0f","%.0f","%.0f")),stringsAsFactors=FALSE);
    }
        

    rownames(ftable) <-rownames(SumTab)
    colnames(ftable) <-colnames(SumTab)
    ftable[moneyRows,]<-format(SumTab[moneyRows,],digits=0,format="d",big.mark=",");
    
    ptable<-xtable(ftable);

    addform<-list();
    addform$pos<-list(highlightRows-1);
    addform$command<-c("\\rowcolor[gray]{0.75}\n");
    ret=list();
    ret$table=SumTab;
    ret$printable=ptable
    ret$prow=addform;
    ret$data=cr;
    return(ret);
}




#paymentClass = c("_cost_Overall_average");
#print(CountReport (paymentClass));


## How many treatments include 0 in the estimate field
## How many treatments include 98 99 in the estimate field






X11()
ggplot(data=subset(OutPatCostTable,TotalCost<1e7),aes(x=SumOfTotals,y=TotalCost )) +geom_point()
ggplot(data=subset(OutPatCostTable,TotalCost<1e6 & SumOfTotals<1e6),aes(x=SumOfTotals,y=TotalCost )) +geom_point()

library("GGally")
cols <-c("Age","NumIllnesses","Overall_average","Group","Gender","CareCenter")
p <- ggpairs(data=subset(OutPatCostTable[,cols],Overall_average<5e5))
p

p <- qplot(Age,data=IndiHealth) +geom_histogram(aes(y=..density..))
p+facet_grid(Group~.)+ggtitle("Age Distribution in Population")

x11()
p <- qplot(Age,data=subset(OutPatCostTable)) +geom_histogram(aes(y=..density..))
p+facet_grid(Group~.)+ggtitle("Age Distribution in Outpatient Cost Reports")
which(OutPatCostTable$Age==0)



p <- ggplot(data=subset(OutPatCostTable),aes(x=Overall_average))
p <- p+ geom_histogram(aes(color=Group))
p+facet_grid(Group~CareCenter)

p <- ggplot(data=subset(OutPatCostTable,Overall_average<1e6),aes(x=Overall_average))
p <- p+ geom_histogram(aes(color=Group))
p+facet_grid(Group~CareCenter)

p <- ggplot(data=subset(OutPatCostTable,Overall_average<1e5),aes(x=Overall_average))
p <- p+ geom_histogram(aes(color=Group))
p <- p+ geom_vline( aes(xintercept = mean(Overall_average)), colour="black")
p <- p+ geom_vline( aes(xintercept = median(Overall_average)), colour="blue")
p+facet_grid(Group~CareCenter,)

CCs <- levels(OutPatCostTable$CareCenter)
sapply(CCs,function(cc){

         cc <- CCs[7]
         cc
         p <- ggplot(data=subset(OutPatCostTable,CareCenter==cc & Overall_average<1e6),aes(x=Overall_average))
         p <- p+ geom_histogram(aes(color=Group))
         p <- p+ geom_vline( aes(xintercept = mean(Overall_average)), colour="black")
         p <- p+ geom_vline(data=ddply(data,Group~.,mean,na.rm=T), 
                            mapping=aes(xintercept=val), color="red")
         p+facet_grid(Group~.)


         library("plyr")
         ggplot(df) + geom_histogram(mapping=aes(x=val)) 
         + geom_vline(data=ddply(df, cat1~cat2, numcolwise(mean)), 
      mapping=aes(xintercept=val), color="red") 
  + facet_grid(cat1~cat2)
         
       }




