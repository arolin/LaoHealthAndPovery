library("xtable")
source("LoadData.R")

##redoAll=T
if (!exists("redoAll")) redoAll <- F

printt <- function(Tb,C,format=T) {
  if(format) {
    TF <- formatC(Tb,digits=0,big.mark=",",format="f")
  }else {
    TF <- Tb
  }
  print(xtable(TF,C,align=c("l",rep("r",ncol(Tb)))),type="html")
}

RebuildRaw=F
if (redoAll){RebuildRaw = T}

if (redoAll){RebuildRaw=T}

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

  rownames(CostRows) <- NULL
  CostRows <- as.data.frame(CostRows)
  names(CostRows) <- c("Q","Center","Group","Serial","Indiv","Consult","Medicine","Medical_Fee","Transport","Others","Overall_average")
  CostRows$Group <- relevel(CostRows$Group,("NoAssist"))
  CostRows$Group <- relevel(CostRows$Group,("GeoID"))
  CostRows$Group <- relevel(CostRows$Group,("PreID"))
  

  save(CostRows,file="CostRowsRaw.RData")

}


################################################################################
##Data is clean and saved
rebuildCostRows = F
if (redoAll){rebuildCostRows = T}

if(rebuildCostRows) {
  
  load(file="CostRowsRaw.RData")
  CostRows <- subset(as.data.frame(CostRows),!is.na(Consult))
  names(CostRows) <- c("Q","Center","Group","Serial","Indiv","Consult","Medicine","Medical_Fee","Transport","Others","Overall_average")


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

  print("means before cleanup\n<br>")
  mbc <- aggregate(.~Group + Center,subset(as.data.frame(CostRows[,c("Group","Center","Overall_average")])),function(X){mean(X,na.rm=T)})
  Tb <- t(matrix(mbc$Overall_average,3))
  colnames(Tb) <- mbc$Group[1:ncol(Tb)]
  rownames(Tb) <- levels(mbc$Center)
  printt(Tb,"Means before cleanup\n<br>")
  
  printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1)[,"Group"]==G)}),
               TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2)[,"Group"]==G)}),
               TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3)[,"Group"]==G)})
               ),"Initial Report Counts")


  print("Remove dirty data\n<br>")
  printt(as.data.frame(subset(as.data.frame(CostRows),is.na(Overall_average))),"NA in Overall average",format=F)
  CostRows <- subset(as.data.frame(CostRows),!is.na(Overall_average))

  print("Remove 99s\n<br>")
  CostRows[CostRows==99] <- NA
  CostRows[CostRows==98] <- NA


  print("Remove totals with 99\n<br>")
  printt(subset(as.data.frame(CostRows),is.na(Overall_average)),"DK in Overall_average",F)
  CostRows <- subset(as.data.frame(CostRows),!is.na(Overall_average))

  printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1)[,"Group"]==G)}),
               TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2)[,"Group"]==G)}),
               TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3)[,"Group"]==G)})
               ),"Report Counts after initial cleanup")

  print("Clean the Suspicious Totals");

  CostRows$SumParts <- rowSums(CostRows[,c("Medicine","Medical_Fee","Transport","Others")],na.rm=T)

  print("Average below sum:\n<br>")
  printt(subset(as.data.frame(CostRows),Overall_average<SumParts),"Overall_average is lees than Sum of Parts",F)

  print("Average below sum:\n<br>")
  printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1& Overall_average<SumParts)[,"Group"]==G)}),
               TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2& Overall_average<SumParts)[,"Group"]==G)}),
               TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3& Overall_average<SumParts)[,"Group"]==G)})
               ),"Average below sum",F)
  print("Average at sum:\n<br>")
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
  print("\n<br>removeing Outliers and dirty data\n<br>")
  print(xtable(subset(CostRows, (Serial==541))),type=html)
  CostRows <-  subset(CostRows,!(Serial==541))
  
  print(xtable(subset(CostRows, (Serial==658 & Center=="PH"))),type=html)
  CostRows <-  subset(CostRows,!(Serial==658 & Center=="PH"))
  
  print(xtable(subset(CostRows, Serial==96)),type=html)
  CostRows <-  subset(CostRows,!Serial==96)
  
  print(xtable(subset(CostRows, (Center=="PH"& Serial==238))),type=html)
  CostRows <-  subset(CostRows,!(Center=="PH"& Serial==238))

  print(xtable(subset(CostRows, (Serial==327& Q==2))),type=html)
  CostRows <-  subset(CostRows,!(Serial==327& Q==2))
  
  print(xtable(subset(CostRows, (Serial==883))),type=html)
  CostRows <-  subset(CostRows,!(Serial==883))


  print("Identifying correction candidates\n<br>")
  print(xtable(
            subset(CostRows,(SumParts/Overall_average)>1)
     ,"Correction Candidates"),type=html)


  CostRows$Total <- CostRows$Overall_average
  CostRows[(CostRows$SumParts> CostRows$Overall_average),"Total"] <-   subset(CostRows,(SumParts/Overall_average)>1)["SumParts"]

  print("Corrected Rows\n<br>")
  print(xtable(
            subset(CostRows,(SumParts/Overall_average)>1)
     ,"Corrected Rows"),type=html)
  

  Tb <- t(matrix(mbc$Overall_average,3))
  colnames(Tb) <- mbc$Group[1:ncol(Tb)]
  rownames(Tb) <- levels(mbc$Center)
  printt(Tb,"Means before cleanup")


  print("means After cleanup")
  mac <- aggregate(.~Group + Center,subset(as.data.frame(CostRows[,c("Group","Center","Total")])),function(X){mean(X,na.rm=T)})
  Tb <- t(matrix(mac$Total,3))
  colnames(Tb) <- mac$Group[1:ncol(Tb)]
  rownames(Tb) <- levels(mac$Center)
  printt(Tb,"Means after cleanup")

  printt(rbind(TotalOutP=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==1)[,"Group"]==G)}),
               TotalInP1=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==2)[,"Group"]==G)}),
               TotalInP2=sapply(c("PreID","GeoID","NoAssist"),function(G){sum(subset(CostRows,Q==3)[,"Group"]==G)})
               ),"Report Counts after cleanup")


  save(CostRows,file="CostRows.RData")
} else {
  load(file="CostRows.RData")
}



WPCA <- t(sapply(Score2,function(S) {S}))

rebuildPats=F
if (redoAll){rebuildPats = T}

if (rebuildPats) {

  OutPat <- subset(CostRows,Q==1)
  OutPatI <-t( sapply(1:nrow(OutPat),function(I){subset(IndiHealth,SRow==OutPat$Serial[I] & SN==OutPat$Indiv[I])}))
  OutPat <- cbind(OutPat,OutPatI)
  rm(OutPatI)
  
  InPat <- subset(CostRows,Q==2 | Q==3)
  InPatI <-t( sapply(1:nrow(InPat),function(I){subset(IndiHealth,SRow==InPat$Serial[I] & SN==InPat$Indiv[I])}))
  InPat <- cbind(InPat,InPatI)
  rm(InPatI)

  save(OutPat,file="OutPat.RData")  
  save(InPat,file="InPat.RData")

  
} else {

  load(file="OutPat.RData")
  load(file="InPat.RData")

}


#####################################################
##clean formatting of Outpat Table
OutPat$Gender<-sapply(1:length(OutPat$Gender),function(I){OutPat$Gender[[I]]})
OutPat$Age   <-sapply(1:length(OutPat$Age),   function(I){OutPat$Age   [[I]]})
OutPat$Marital_Status   <-sapply(1:length(OutPat$Marital_Status),   function(I){OutPat$Marital_Status   [[I]]})
OutPat$NumIllnesses   <-sapply(1:length(OutPat$NumIllnesses),   function(I){OutPat$NumIllnesses   [[I]]})
OutPat$Group <- factor(OutPat$Group,levels=c("PreID","GeoID","NoAssist"))
names(OutPat)[names(OutPat)=="Center"] <- "Facility"
OutPatGroups <- list(PreID=OutPat$Group=="PreID",
                     GeoID=OutPat$Group=="GeoID",
                     NoAssist=OutPat$Group=="NoAssist",
                     All=rep(T,length(OutPat$Group)))

OutPat <- OutPat[,which(!(names(OutPat) %in% c("Group.1","Care_At","Marital_Status_Other","Illness")))]

names(OutPat)[names(OutPat)=="Care_At_Village_health_volunteer"    ] <- "Care_At_HV"
names(OutPat)[names(OutPat)=="Care_At_Traditional_healer"          ] <- "Care_At_TH"
names(OutPat)[names(OutPat)=="Care_At_Health_centre"               ] <- "Care_At_HC"
names(OutPat)[names(OutPat)=="Care_At_District_hospital"           ] <- "Care_At_DH"
names(OutPat)[names(OutPat)=="Care_At_Provincial_hospital/Regional"] <- "Care_At_PH"
names(OutPat)[names(OutPat)=="Care_At_National_hospital"           ] <- "Care_At_NH"
names(OutPat)[names(OutPat)=="Care_At_Private_pharmacy"            ] <- "Care_At_PP"
names(OutPat)[names(OutPat)=="Care_At_Private_clinic"              ] <- "Care_At_PC"
names(OutPat)[names(OutPat)=="Care_At_Religious_Healer"            ] <- "Care_At_RH"
names(OutPat)[names(OutPat)=="Overnight_Health_Centre"             ] <- "Overnight_At_HC"
names(OutPat)[names(OutPat)=="Overnight_District_Hospital"         ] <- "Overnight_At_DH"
names(OutPat)[names(OutPat)=="Overnight_Provincial_Hospital"       ] <- "Overnight_At_PH"
names(OutPat)[names(OutPat)=="Overnight_National_Hospital"         ] <- "Overnight_At_NH"
names(OutPat)[names(OutPat)=="Overnight_Private_Clinic"            ] <- "Overnight_At_PC"
names(OutPat)[names(OutPat)=="Nights_Health_Centre"             ] <- "Nights_At_HC"
names(OutPat)[names(OutPat)=="Nights_District_Hospital"         ] <- "Nights_At_DH"
names(OutPat)[names(OutPat)=="Nights_Provincial_Hospital"       ] <- "Nights_At_PH"
names(OutPat)[names(OutPat)=="Nights_National_Hospital"         ] <- "Nights_At_NH"
names(OutPat)[names(OutPat)=="Nights_Private_Clinic"            ] <- "Nights_At_PC"

    
PatOPC <- c("Care_At_Homemade_medicine","Care_At_Village_modern_medical_practitioner","Care_At_HV","Care_At_TH","Care_At_HC","Care_At_DH","Care_At_PH","Care_At_NH","Care_At_PP","Care_At_PC","Care_At_Abroad","Care_At_Illegal_medical_practitioner","Care_At_Other","Care_At_Do_not_remember","Care_At_Do_not_know","Care_At_Military_Hospital","Care_At_RH")



OutPat[,PatOPC] <- !is.na(OutPat[,PatOPC])
OutPat[,grep("Overnight",names(OutPat))][is.na(OutPat[,grep("Overnight",names(OutPat))])] <- 0
OutPat[is.na(OutPat$NumIllnesses),"NumIllnesses"] <- 0;
for (I in grep("Overnight",names(OutPat))) {
  OutPat[,I] <- factor(OutPat[,I],levels=c(0,1,2),labels=c("NoCare","Overnight","Day"))
}
OutPat[,grep("Nights",names(OutPat))][is.na(OutPat[,grep("Nights",names(OutPat))])] <- 0



OutPat <- cbind(OutPat,WPCA=WPCA[OutPat$Serial,])
for (C in grep("WPCA",names(OutPat))) {OutPat[,C]<-sapply(1:length(OutPat[,C]),   function(I){OutPat[,C][[I]]})}
head(OutPat[,PatOPC])






################################################################################
##InPat Clean
InPat$Gender<-sapply(1:length(InPat$Gender),function(I){InPat$Gender[[I]]})
InPat$Age   <-sapply(1:length(InPat$Age),   function(I){InPat$Age   [[I]]})
InPat$Marital_Status   <-sapply(1:length(InPat$Marital_Status),   function(I){InPat$Marital_Status   [[I]]})
InPat$NumIllnesses   <-sapply(1:length(InPat$NumIllnesses),   function(I){InPat$NumIllnesses   [[I]]})

InPat$Group <- factor(InPat$Group,levels=c("PreID","GeoID","NoAssist"))
names(InPat)[names(InPat)=="Center"] <- "Facility"

InPatGroups <- list(PreID=InPat$Group=="PreID",
                     GeoID=InPat$Group=="GeoID",
                     NoAssist=InPat$Group=="NoAssist",
                     All=rep(T,length(InPat$Group)))

InPat <- InPat[,which(!(names(InPat) %in% c("Group.1","Care_At","Marital_Status_Other","Illness")))]

##Simplify Facility Names
names(InPat)[names(InPat)=="Care_At_Village_health_volunteer"    ] <- "Care_At_HV"
names(InPat)[names(InPat)=="Care_At_Traditional_healer"          ] <- "Care_At_TH"
names(InPat)[names(InPat)=="Care_At_Health_centre"               ] <- "Care_At_HC"
names(InPat)[names(InPat)=="Care_At_District_hospital"           ] <- "Care_At_DH"
names(InPat)[names(InPat)=="Care_At_Provincial_hospital/Regional"] <- "Care_At_PH"
names(InPat)[names(InPat)=="Care_At_National_hospital"           ] <- "Care_At_NH"
names(InPat)[names(InPat)=="Care_At_Private_pharmacy"            ] <- "Care_At_PP"
names(InPat)[names(InPat)=="Care_At_Private_clinic"              ] <- "Care_At_PC"
names(InPat)[names(InPat)=="Care_At_Religious_Healer"            ] <- "Care_At_RH"
names(InPat)[names(InPat)=="Overnight_Health_Centre"             ] <- "Overnight_At_HC"
names(InPat)[names(InPat)=="Overnight_District_Hospital"         ] <- "Overnight_At_DH"
names(InPat)[names(InPat)=="Overnight_Provincial_Hospital"       ] <- "Overnight_At_PH"
names(InPat)[names(InPat)=="Overnight_National_Hospital"         ] <- "Overnight_At_NH"
names(InPat)[names(InPat)=="Overnight_Private_Clinic"            ] <- "Overnight_At_PC"
names(InPat)[names(InPat)=="Nights_Health_Centre"             ] <- "Nights_At_HC"
names(InPat)[names(InPat)=="Nights_District_Hospital"         ] <- "Nights_At_DH"
names(InPat)[names(InPat)=="Nights_Provincial_Hospital"       ] <- "Nights_At_PH"
names(InPat)[names(InPat)=="Nights_National_Hospital"         ] <- "Nights_At_NH"
names(InPat)[names(InPat)=="Nights_Private_Clinic"            ] <- "Nights_At_PC"

    
PatOPC <- c("Care_At_Homemade_medicine","Care_At_Village_modern_medical_practitioner","Care_At_HV","Care_At_TH","Care_At_HC","Care_At_DH","Care_At_PH","Care_At_NH","Care_At_PP","Care_At_PC","Care_At_Abroad","Care_At_Illegal_medical_practitioner","Care_At_Other","Care_At_Do_not_remember","Care_At_Do_not_know","Care_At_Military_Hospital","Care_At_RH")



InPat[,PatOPC] <- !is.na(InPat[,PatOPC])
InPat[,grep("Overnight",names(InPat))][is.na(InPat[,grep("Overnight",names(InPat))])] <- 0
InPat[is.na(InPat$NumIllnesses),"NumIllnesses"] <- 0;
for (I in grep("Overnight",names(InPat))) {
  InPat[,I] <- factor(InPat[,I],levels=c(0,1,2),labels=c("NoCare","Overnight","Day"))
}
InPat[,grep("Nights",names(InPat))][is.na(InPat[,grep("Nights",names(InPat))])] <- 0


head(InPat[,PatOPC])
InPat <- cbind(InPat,WPCA=WPCA[InPat$Serial,])
for (C in grep("WPCA",names(InPat))) {InPat[,C]<-sapply(1:length(InPat[,C]),   function(I){InPat[,C][[I]]})}

##**************************
##Synched











##  [3] "Group"                                      
## [16] "Gender"                                     
## [17] "Age"                                        
## [18] "Marital_Status"                             
## [19] "Marital_Status_Other"                       
## [20] "Illness"                                    
## [21] "NumIllnesses"                               
## [22] "Overnight_Health_Centre"                    
## [23] "Nights_Health_Centre"                       
## [24] "Overnight_District_Hospital"                
## [25] "Nights_District_Hospital"                   
## [26] "Overnight_Provincial_Hospital"              
## [27] "Nights_Provincial_Hospital"                 
## [28] "Overnight_National_Hospital"                
## [29] "Nights_National_Hospital"                   
## [30] "Overnight_Private_Clinic"                   
## [31] "Nights_Private_Clinic"                      
## [32] "Care_At_No_care"                            
## [33] "Care_At_Homemade_medicine"                  
## [34] "Care_At_Village_modern_medical_practitioner"
## [35] "Care_At_Village_health_volunteer"           
## [36] "Care_At_Traditional_healer"                 
## [37] "Care_At_Health_centre"                      
## [38] "Care_At_District_hospital"                  
## [39] "Care_At_Provincial_hospital/Regional"       
## [40] "Care_At_National_hospital"                  
## [41] "Care_At_Private_pharmacy"                   
## [42] "Care_At_Private_clinic"                     
## [43] "Care_At_Abroad"                             
## [44] "Care_At_Illegal_medical_practitioner"       
## [45] "Care_At_Other"                              
## [46] "Care_At_Do_not_remember"                    
## [47] "Care_At_Do_not_know"                        
## [48] "Care_At_Military_Hospital"                  
## [49] "Care_At_Religious_Healer"                   
## [55] "ModelID"

                      
