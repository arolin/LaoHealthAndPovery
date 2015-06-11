library("xtable")
source("LoadData.R")


printt <- function(Tb,C,format=T) {
  if(format) {
    TF <- formatC(Tb,digits=0,big.mark=",",format="f")
  }else {
    TF <- Tb
  }
  print(xtable(TF,C,align=c("l",rep("r",ncol(Tb)))),type="html")
}

RebuildRaw=F
RebuildRaw=T
if (RebuildRaw) {

  ##Clear any modification from the questions
  lps[,which(names(lpsraw)=="q3_2"):which(names(lpsraw)=="q3.7.10_other")] <-
    lpsraw[,which(names(lpsraw)=="q3_2"):which(names(lpsraw)=="q3.7.10_other")]

  Q <- which(names(lpsraw)=="q3_2")
  Q2 <- which(names(lpsraw)=="q3_4")

                                        #build header for extracted costs table
  RepCols <- c("Indiv","Consult_NH","Consult_PH","Consult_DH","Consult_HC","Consult_HV","Consult_TH","Consult_PP","Consult_PC","Consult_RH","NH_Medicine","NH_Medical_Fee","NH_Transport","NH_Others","NH_Overall_average","NH_other_","PH_Medicine","PH_Medical_Fee","PH_Transport","PH_Others","PH_Overall_average","PH_other_","DH_Medicine","DH_Medical_Fee","DH_Transport","DH_Others","DH_Overall_average","DH_other_","HC_Medicine","HC_Medical_Fee","HC_Transport","HC_Others","HC_Overall_average","HC_other_","HV_Medicine","HV_Medical_Fee","HV_Transport","HV_Others","HV_Overall_average","HV_other_","TH_Medicine","TH_Medical_Fee","TH_Transport","TH_Others","TH_Overall_average","TH_other_","PP_Medicine","PP_Medical_Fee","PP_Transport","PP_Others","PP_Overall_average","PP_other_","PC_Medicine","PC_Medical_Fee","PC_Transport","PC_Others","PC_Overall_average","PC_other_","RH_Medicine","RH_Medical_Fee","RH_Transport","RH_Others","RH_Overall_average","RH_other_","Total_cost_Medicine","Total_cost_Medical_Fee","Total_cost_Transport","Total_cost_Others","Total_cost_Overall_average","Total_cost_other")
  RepCols <- sapply(1:3,paste,RepCols,sep="_")


  CostRepCols <- c("Serial",names(lps)[Q:(Q+209)])
  AllResp <-  lps[,CostRepCols]
  colnames(AllResp) <- c("Serial",RepCols)
                                        #write.csv(AllResp,file="AllCostReports.csv")


  SuspectLines <- lps[c(541,658,708),CostRepCols]
  colnames(SuspectLines) <- c("Serial",RepCols);


  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  AllResp <- sapply(AllResp,function(C){as.numeric(trim(as.character(C)))})
  str(AllResp)
  head(AllResp)


  bad <- is.na(AllResp[,paste(1,c("NH_Medicine","NH_Medical_Fee","NH_Transport","NH_Others","NH_Overall_average","NH_other_","PH_Medicine","PH_Medical_Fee","PH_Transport","PH_Others","PH_Overall_average","PH_other_","DH_Medicine","DH_Medical_Fee","DH_Transport","DH_Others","DH_Overall_average","DH_other_","HC_Medicine","HC_Medical_Fee","HC_Transport","HC_Others","HC_Overall_average","HC_other_","HV_Medicine","HV_Medical_Fee","HV_Transport","HV_Others","HV_Overall_average","HV_other_","TH_Medicine","TH_Medical_Fee","TH_Transport","TH_Others","TH_Overall_average","TH_other_","PP_Medicine","PP_Medical_Fee","PP_Transport","PP_Others","PP_Overall_average","PP_other_","PC_Medicine","PC_Medical_Fee","PC_Transport","PC_Others","PC_Overall_average","PC_other_","RH_Medicine","RH_Medical_Fee","RH_Transport","RH_Others","RH_Overall_average","RH_other_","Total_cost_Medicine","Total_cost_Medical_Fee","Total_cost_Transport","Total_cost_Others","Total_cost_Overall_average",
                                  "Total_cost_other"),sep='_')])

  CostRows <- data.frame()
  for (H in 1:930) {
    for (Q in 1:3) {
      for (C in c("NH","PH","DH","HC","HV","TH","PP","PC","RH")) {
        rowNames <- c("Serial")
        rowNames <- c(rowNames,paste(Q,"Indiv",sep="_"))
        rowNames <- c(rowNames,paste(Q,"Consult",C,sep="_"))
        rowNames <-  c(rowNames,paste(Q,as.character(C),c("Medicine","Medical_Fee","Transport","Others","Overall_average"),sep="_"))
        row <- c(Q=Q,center=C,levels(lps$Group)[lps$Group[H]],as.numeric(AllResp[H,rowNames]))
        if (dim(CostRows)[1]==0) {
          CostRows <- rbind(row)
        }else{
          CostRows  <- rbind(CostRows,row)
        }
      }
    }
  }

  CostRows$Group <- relevel(CostRows$Group,("NoAssist"))
  CostRows$Group <- relevel(CostRows$Group,("GeoID"))
  CostRows$Group <- relevel(CostRows$Group,("PreID"))
  
  names(CostRows) <- c("Q","Center","Group","Serial","Indiv","Consult","Medicine","Medical_Fee","Transport","Others","Overall_average")
  save(CostRows,file="CostRowsRaw.RData")

}


################################################################################
##Data is clean and saved
load(file="CostRowsRaw.RData")
CostRows <- subset(as.data.frame(CostRows),!is.na(Consult))
names(CostRows) <- c("Q","Center","Group","Serial","Indiv","Consult","Medicine","Medical_Fee","Transport","Others","Overall_average")

printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1)[,"Group"]==G)}),
             TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2)[,"Group"]==G)}),
             TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3)[,"Group"]==G)})
             ),"Report Counts")

CostRowsN<- CostRows$Serial
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
CostRowsN$Medicine        <- as.numeric(trim(levels(CostRows$Medicine       )[CostRows$Medicine       ]))
CostRowsN$Medical_Fee     <- as.numeric(trim(levels(CostRows$Medical_Fee    )[CostRows$Medical_Fee    ]))
CostRowsN$Transport       <- as.numeric(trim(levels(CostRows$Transport      )[CostRows$Transport      ]))
CostRowsN$Others           <- as.numeric(trim(levels(CostRows$Others          )[CostRows$Others         ]))
CostRowsN$Overall_average <- as.numeric(trim(levels(CostRows$Overall_average)[CostRows$Overall_average]))

CostRows$Medicine        <-  CostRowsN$Medicine       
CostRows$Medical_Fee     <-  CostRowsN$Medical_Fee    
CostRows$Transport       <-  CostRowsN$Transport      
CostRows$Others           <-  CostRowsN$Others          
CostRows$Overall_average <-  CostRowsN$Overall_average

print("means before cleanup")
mbc <- aggregate(.~Group + Center,subset(as.data.frame(CostRows[,c("Group","Center","Overall_average")])),function(X){mean(X,na.rm=T)})
mbc
Tb <- t(matrix(mbc$Overall_average,3))
colnames(Tb) <- mbc$Group[1:ncol(Tb)]
rownames(Tb) <- levels(mbc$Center)
printt(Tb,"Means before cleanup")



print("Remove dirty data")
printt(as.data.frame(subset(as.data.frame(CostRows),is.na(Overall_average))),"NA in Overall average",format=F)
CostRows <- subset(as.data.frame(CostRows),!is.na(Overall_average))


print(xtable(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1)[,"Group"]==G)}),
                   TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2)[,"Group"]==G)}),
                   TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3)[,"Group"]==G)})
                   )),type="html")

print("Remove 99s")
CostRows[CostRows==99] <- NA
CostRows[CostRows==98] <- NA


print("Remove totals with 99")
printt(subset(as.data.frame(CostRows),is.na(Overall_average)),"DK in Overall_average",F)
CostRows <- subset(as.data.frame(CostRows),!is.na(Overall_average))


CostRows$SumParts <- rowSums(CostRows[,c("Medicine","Medical_Fee","Transport","Others")],na.rm=T)

print("Average below sum:")
printt(subset(as.data.frame(CostRows),Overall_average<SumParts),"Overall_average is lees than Sum of Parts",F)

print("Average below sum:")
printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1& Overall_average<SumParts)[,"Group"]==G)}),
             TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2& Overall_average<SumParts)[,"Group"]==G)}),
             TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3& Overall_average<SumParts)[,"Group"]==G)})
             ),"Average below sum",F)
print("Average at sum:")
printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1& Overall_average==SumParts)[,"Group"]==G)}),
             TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2& Overall_average==SumParts)[,"Group"]==G)}),
             TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3& Overall_average==SumParts)[,"Group"]==G)})
             ),"Average at sum",F)









## aggregate(.~Group + Center,subset(as.data.frame(CostRows[,c("Group","Center","Overall_average","SumParts")]),Overall_average<SumParts),function(X){mean(X,na.rm=T)})

## aggregate(.~Group + Center,subset(as.data.frame(CostRows[,c("Group","Center","Overall_average","SumParts")])),function(X){mean(X,na.rm=T)})

## p <- ggplot (data=CostRows,aes(x=Overall_average,y=SumParts,color=Group))
## p + geom_point()

## p <- ggplot (data=subset(CostRows,Overall_average<1e7 & SumParts<1e7),aes(y=Overall_average,x=SumParts,color=Group))
## p + geom_point()

## p <- ggplot (data=subset(CostRows,Overall_average<1e6 & SumParts<1e6),aes(y=Overall_average,x=SumParts,color=Group))
## p + geom_point()


## p <- ggplot (data=subset(CostRows,Overall_average & SumParts),aes(y=(SumParts/Overall_average),x=SumParts,color=Group))
## p + geom_point()

## p <- ggplot (data=subset(CostRows,Overall_average<1e6 & SumParts<1e6),aes(y=(SumParts/Overall_average),x=SumParts,color=Group))
## p + geom_point()


##Recover these
print("Identifying correction candidates")
subset(CostRows,(SumParts/Overall_average)>1 & (SumParts/Overall_average)<2)

subset(CostRows,(SumParts/Overall_average)>2)

print("remove serial 541 abd 658")
CostRows <-subset(CostRows,!(Serial==541))
CostRows <-subset(CostRows,!(Serial==658 & Center=="PH"))
CostRows <-subset(CostRows,Serial==96)
CostRows <-subset(CostRows,!(Center=="PH"& Serial==238))
CostRows <-subset(CostRows,!(Serial==327& Q==2))
CostRows <-subset(CostRows,!(Serial==883))





CostRows$Total <- CostRows$Overall_average
CostRows[(CostRows$SumParts> CostRows$Overall_average),"Total"] <-   subset(CostRows,(SumParts/Overall_average)>1)["SumParts"]



Tb <- t(matrix(mbc$Overall_average,3))
colnames(Tb) <- mbc$Group[1:ncol(Tb)]
rownames(Tb) <- levels(mbc$Center)
printt(Tb,"Means before cleanup")

print("means After cleanup")
mac <- aggregate(.~Group + Center,subset(as.data.frame(CostRows[,c("Group","Center","Total")])),function(X){mean(X,na.rm=T)})
mac
Tb <- t(matrix(mac$Total,3))
colnames(Tb) <- mac$Group[1:ncol(Tb)]
rownames(Tb) <- levels(mac$Center)
printt(Tb,"Means after cleanup")

nrow(subset(CostRows,Total>1000000))

save(CostRows,file="CostRows.RData")





## subset(CostRows,(SumParts/Overall_average)<.5)

## p <- ggplot(data=CostRows)
## p + geom_boxplot(aes(x=Center,y=Overall_average))+facet_grid(.~Group)

## p <- ggplot(data=subset(CostRows,Overall_average<2e7))
## p + geom_boxplot(aes(x=Center,y=Overall_average))+facet_grid(.~Group)

## p <- ggplot(data=subset(CostRows,Overall_average<5e6))
## p + geom_boxplot(aes(x=Center,y=Overall_average))+facet_grid(.~Group)


## p <- ggplot(data=subset(CostRows,Overall_average<2e7))
## p + geom_boxplot(aes(x=Group,y=Overall_average))+facet_grid(.~Center)

## sum(CostRows[,"Overall_average"])




## #Remove All 98/99
## AllResp[AllResp==98] <- NA
## AllResp[AllResp==99] <- NA

## which(names(lpsraw)=="q3_4")
## which(names(lpsraw)=="q3.6")


## #708 HC has no useful info in
## AllResp[708,colnames(AllResp)[grep("HC",colnames(AllResp)[grep("1_",colnames(AllResp))])]] <- NA
## #96 PH has no useful info
## AllResp[96,colnames(AllResp)[grep("PH",colnames(AllResp)[grep("1_",colnames(AllResp))])]] <- NA
## AllResp[96,colnames(AllResp)[grep("DH",colnames(AllResp)[grep("1_",colnames(AllResp))])]] <- NA
## #658 PH requires clarfication
## ## AllResp[658,colnames(AllResp)[grep("PH",colnames(AllResp)[grep("1_",colnames(AllResp))])]] <- NA

## #AllResp[541,"2_DH_Transport"] <- 48000
## Q <- which(names(lpsraw)=="q3_2")

## ConsultFlags <-lps[, (Q+1):(Q+9)]
## str(ConsultFlags)
## sum(rowSums(!is.na(ConsultFlags)))

## print (paste ("Total Outpatient Cost info:",length(OutPatCostTable[,1])))
## sapply(c("PreID","GeoID","NoAssist"),function(G){sum(!is.na(subset(OutPatCostTable,Group==G& Overall_average==0)))})



## Centers <- c("NH","PH","DH","HC","HV","TH","PP","PC","RH")

## for (Q in 1:3) {
##   for (C in Centers) {

##     w <- paste(Q,C,c("Medical_Fee","Medicine","Transport","Others"),sep="_")
    
##     IsValidData <- !is.na( AllResp[,paste(Q,"Consult",C,sep="_")])




## for (Q in 1:3) {
##   for (C in Centers) {

##     w <- paste(Q,C,c("Medical_Fee","Medicine","Transport","Others"),sep="_")
    
##     IsValidData <- !is.na( AllResp[,paste(Q,"Consult",C,sep="_")])

##     Sums <- rowSums(AllResp[,w],na.rm=T)
##     incomplete <- rep(F,length(Sums))
##     incomplete[IsValidData] <- Sums[IsValidData]>AllResp[IsValidData,paste(Q,C,"Overall_average",sep="_")]
##     incomplete[is.na(incomplete)] <- T
##     disp <- validData & incomplete


##     print(cbind(Flag=AllResp[,paste(Q,"Consult",C,sep="_")][disp],AllResp[disp,c("Serial",paste(Q,C,c("Medical_Fee","Medicine","Transport","Others","Overall_average"),sep="_"))]))
    
##     write.csv(cbind(Flag=AllResp[,paste(Q,"Consult",C,sep="_")][disp],AllResp[disp,c("Serial",paste(Q,C,c("Medical_Fee","Medicine","Transport","Others","Overall_average"),sep="_"))]),
##               file=paste("Suspect_Lines_Q",Q,"_",C,".csv",sep=""))

##     Sums[validData]

##     ##view the values to replace
##     AllResp[incomplete,paste(Q,C,"Overall_average",sep="_")]
##     Sums[incomplete]

##     ##replace the values
## #    AllResp[incomplete,paste(Q,C,"Overall_average",sep="_")] <-
## #    Sums[incomplete]

##     write.csv(cbind(Flag=AllResp[,paste(Q,"Consult",C,sep="_")][disp],AllResp[disp,c("Serial",paste(Q,C,c("Medical_Fee","Medicine","Transport","Others","Overall_average"),sep="_"))]),
##           file=paste("Suspect_Lines_Q",Q,"_",C,".csv",sep=""))
##   }
## }





## ## AllResp[disp,paste(Q,C,"Overall_average",sep="_")] <- Sums[disp]
## Sums[disp]






## ##Extract the consulatation tables
## source("./SmallScripts/Count_Consultation_Users.r")
## head(OutPatCostTable)








## ## sum(rowSums(lps[,1166:1169+i],na.rm=T) > lps[,1170+i],na.rm=T)
## ## which(rowSums(lps[,1166:1169+i],na.rm=T) > lps[,1170+i])

## ## lps[which(rowSums(lps[,1166:1169+i],na.rm=T) > lps[,1170+i]),(1166:1171)+i]

## ## lps[which(rowSums(lps[,1166:1169+i],na.rm=T) > lps[,1170+i]),1170+i]  <- 
## ##               rowSums(lps[,1166:1169+i],na.rm=T)[which(rowSums(lps[,1166:1169+i],na.rm=T) > lps[,1170+i])]




## Centers <- c("NH","PH","DH","HC","HV","TH","PP","PC","RH")

## sums <- rowSums(AllResp[!is.na(AllResp[,"1_Indiv"]),paste("1_",Centers,"_Overall_average",sep="")],na.rm=T)
## totals <-      AllResp[!is.na(AllResp[,"1_Indiv"]),"1_Total_cost_Overall_average"]
## repl <- totals<sums
## repl[is.na(repl)] <- F
## AllResp[!is.na(AllResp[,"1_Indiv"]),"1_Total_cost_Overall_average"][repl] <- sums[repl]
## plot(sums, AllResp[!is.na(AllResp[,"1_Indiv"]),"1_Total_cost_Overall_average"])



## Centers <- c("NH","PH","DH","HC","HV","TH","PP","PC","RH")

## sums <- rowSums(AllResp[!is.na(AllResp[,"1_Indiv"]),paste("1_",Centers,"_Overall_average",sep="")],na.rm=T)
## totals <-      AllResp[!is.na(AllResp[,"1_Indiv"]),"1_Total_cost_Overall_average"]
## repl <- totals<sums
## repl[is.na(repl)] <- F
## AllResp[!is.na(AllResp[,"1_Indiv"]),"1_Total_cost_Overall_average"][repl] <- sums[repl]
## plot(sums, AllResp[!is.na(AllResp[,"1_Indiv"]),"1_Total_cost_Overall_average"])

## sums <- rowSums(AllResp[!is.na(AllResp[,"2_Indiv"]),paste("2_",Centers,"_Overall_average",sep="")],na.rm=T)
## totals <-      AllResp[!is.na(AllResp[,"2_Indiv"]),"2_Total_cost_Overall_average"]
## repl <- totals<sums
## repl[is.na(repl)] <- F
## AllResp[!is.na(AllResp[,"2_Indiv"]),"2_Total_cost_Overall_average"][repl] <- sums[repl]
## plot(sums, AllResp[!is.na(AllResp[,"2_Indiv"]),"2_Total_cost_Overall_average"])



OutPat <- subset(CostRows,Q=1)
OutPatI <-t( sapply(1:nrow(OutPat),function(I){subset(IndiHealth,SRow==OutPat$Serial[I] & SN==OutPat$Indiv[I])}))
OutPat <- cbind(OutPat,OutPatI)
