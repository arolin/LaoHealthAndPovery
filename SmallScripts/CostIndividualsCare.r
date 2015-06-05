## CostIndividualsCare.r


##  [1] "SRow"                                       
##  [2] "Name"                                       
##  [3] "Gender"                                     
##  [4] "Age"                                        
##  [5] "Marital_Status"                             
##  [6] "Marital_Status_Other"                       
##  [7] "Illness"                                    
##  [8] "NumIllnesses"                               
##  [9] "Overnight_Health_Centre"                    
## [10] "Nights_Health_Centre"                       
## [11] "Overnight_District_Hospital"                
## [12] "Nights_District_Hospital"                   
## [13] "Overnight_Provincial_Hospital"              
## [14] "Nights_Provincial_Hospital"                 
## [15] "Overnight_National_Hospital"                
## [16] "Nights_National_Hospital"                   
## [17] "Overnight_Private_Clinic"                   
## [18] "Nights_Private_Clinic"                      
## [19] "Care_At_No_care"                            
## [20] "Care_At_Homemade_medicine"                  
## [21] "Care_At_Village_modern_medical_practitioner"
## [22] "Care_At_Village_health_volunteer"           
## [23] "Care_At_Traditional_healer"                 
## [24] "Care_At_Health_centre"                      
## [25] "Care_At_District_hospital"                  
## [26] "Care_At_Provincial_hospital/Regional"       
## [27] "Care_At"                                    
## [28] "Care_At_National_hospital"                  
## [29] "Care_At_Private_pharmacy"                   
## [30] "Care_At_Private_clinic"                     
## [31] "Care_At_Abroad"                             
## [32] "Care_At_Illegal_medical_practitioner"       
## [33] "Care_At_Other"                              
## [34] "Care_At_Do_not_remember"                    
## [35] "Care_At_Do_not_know"                        
## [36] "Care_At_Military_Hospital"                  
## [37] "Care_At_Religious_Healer"                   
## [38] "Group"                     

##"NumIllnesses"
IndiHealth$Group <- as.numeric(IndiHealth$Group)

lps$OutTotOveralRep <- rowSums(lps[, c("HH_Illness_1_National_Hospital_cost_Overall_average"  ,"HH_Illness_1_Provincial_Hospital_cost_Overall_average","HH_Illness_1_District_Hospital_cost_Overall_average"  ,"HH_Illness_1_Health_Center_cost_Overall_average"      ,"HH_Illness_1_Health_Volunteer_cost_Overall_average"   ,"HH_Illness_1_Traditional_Healer_cost_Overall_average" ,"HH_Illness_1_Private_Pharmacist_cost_Overall_average" ,"HH_Illness_1_Private_Clinic_cost_Overall_average"     ,"HH_Illness_1_Religious_Healer_cost_Overall_average"   )],na.rm=T);
lps$OutTotOveralRep[which(rowSums (!is.na(lps[, c("HH_Illness_1_National_Hospital_cost_Overall_average"  ,"HH_Illness_1_Provincial_Hospital_cost_Overall_average","HH_Illness_1_District_Hospital_cost_Overall_average"  ,"HH_Illness_1_Health_Center_cost_Overall_average"      ,"HH_Illness_1_Health_Volunteer_cost_Overall_average"   ,"HH_Illness_1_Traditional_Healer_cost_Overall_average" ,"HH_Illness_1_Private_Pharmacist_cost_Overall_average" ,"HH_Illness_1_Private_Clinic_cost_Overall_average"     ,"HH_Illness_1_Religious_Healer_cost_Overall_average"   )]))==0)] <- NA
head(lps$OutTotOveralRep)
lps$OutTotRep <- lps[,"HH_Illness_1_Total_cost_Overall_average"              ]



inpcosts <- c("HH_Illness_2_National_Hospital_cost_Overall_average","HH_Illness_2_Provincial_Hospital_cost_Overall_average","HH_Illness_2_District_Hospital_cost_Overall_average","HH_Illness_2_Health_Center_cost_Overall_average","HH_Illness_2_Health_Volunteer_cost_Overall_average","HH_Illness_2_Traditional_Healer_cost_Overall_average","HH_Illness_2_Private_Pharmacist_cost_Overall_average","HH_Illness_2_Private_Clinic_cost_Overall_average","HH_Illness_2_Religious_Healer_cost_Overall_average","HH_Illness_3_National_Hospital_cost_Overall_average","HH_Illness_3_Provincial_Hospital_cost_Overall_average","HH_Illness_3_District_Hospital_cost_Overall_average","HH_Illness_3_Health_Center_cost_Overall_average","HH_Illness_3_Health_Volunteer_cost_Overall_average","HH_Illness_3_Traditional_Healer_cost_Overall_average","HH_Illness_3_Private_Pharmacist_cost_Overall_average","HH_Illness_3_Private_Clinic_cost_Overall_average","HH_Illness_3_Religious_Healer_cost_Overall_average")
lps$InpTotOveralRep <- rowSums(lps[,inpcosts],na.rm=T)
lps$InpTotOveralRep[which(rowSums(!is.na(lps[,inpcosts]))==0)] <- NA
lps$InpTotRep <- rowSums(lps[,c("HH_Illness_2_Total_cost_Overall_average","HH_Illness_3_Total_cost_Overall_average")],na.rm=T)

which(lps$InpTotOveralRep==0)








ComputeCosts <- function(I) {
  Cost <- list();
  Cost$SRow <- as.numeric(I[["SRow"]]);
  Cost$N <- as.numeric(I[["SN"]]);
  hasRep2O <- lps[Cost$SRow,"HH_Illness_2_report_individual_number"];
  if (is.na(hasRep2O)) {
    hasRep2 <- F;
  }else {
    hasRep2 <- hasRep2O==as.numeric(I[["SN"]])
  }
  hasRep3O <- lps[Cost$SRow,"HH_Illness_3_report_individual_number"];
  if (is.na(hasRep3O)) {
    hasRep3 <- F;
  }else {
    hasRep3 <- hasRep3O==as.numeric(I[["SN"]])
  }
  ##Add in each inpatient health event
  Cost$DH <- 0;
  Cost$HC <- 0;
  Cost$PH <- 0;
  Cost$PC <- 0;
  Cost$NH <- 0;
  ApplyModel  <-  !(hasRep3|hasRep2)
  if (hasRep3){
    if (!is.na(lps[Cost$SRow,]$HH_Illness_3_Private_Clinic_cost_Overall_average)) {
      Cost$DH <- lps[Cost$SRow,]$HH_Illness_3_Private_Clinic_cost_Overall_average;
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_3_District_Hospital_cost_Overall_average)) {
      Cost$DH <- lps[Cost$SRow,]$HH_Illness_3_District_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_3_National_Hospital_cost_Overall_average)) {
      Cost$NH <- lps[Cost$SRow,]$HH_Illness_3_National_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_3_National_Hospital_cost_Overall_average)) {
      Cost$NH <- lps[Cost$SRow,]$HH_Illness_3_National_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_3_Health_Center_cost_Overall_average)) {
      Cost$HC <- lps[Cost$SRow,]$HH_Illness_3_Health_Center_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_3_Provincial_Hospital_cost_Overall_average) ){
      Cost$PH <- lps[Cost$SRow,]$HH_Illness_3_Provincial_Hospital_cost_Overall_average
    }
  }else if (hasRep2){
    if (!is.na(lps[Cost$SRow,]$HH_Illness_2_Private_Clinic_cost_Overall_average)) {
      Cost$DH <- lps[Cost$SRow,]$HH_Illness_2_Private_Clinic_cost_Overall_average;
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_2_District_Hospital_cost_Overall_average)) {
      Cost$DH <- lps[Cost$SRow,]$HH_Illness_2_District_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_2_National_Hospital_cost_Overall_average)) {
      Cost$NH <- lps[Cost$SRow,]$HH_Illness_2_National_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_2_National_Hospital_cost_Overall_average)) {
      Cost$NH <- lps[Cost$SRow,]$HH_Illness_2_National_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_2_Health_Center_cost_Overall_average)) {
      Cost$HC <- lps[Cost$SRow,]$HH_Illness_2_Health_Center_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_2_Provincial_Hospital_cost_Overall_average) ){
      Cost$PH <- lps[Cost$SRow,]$HH_Illness_2_Provincial_Hospital_cost_Overall_average
    }
  }
  if(ApplyModel) {
    if(!is.na(I["Overnight_Health_Centre"])) {
      Cost$HC <-   ModelInpCosts[1, as.numeric( I[["Group"]] ) ];
    }
    if(!is.na(I["Overnight_District_Hospital"])) {
      Cost$DH <-   ModelInpCosts[2,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Overnight_Provincial_Hospital"])) {
      Cost$PH <-   ModelInpCosts[3,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Overnight_National_Hospital"])) {
      Cost$NH <-   ModelInpCosts[4,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Overnight_Private_Clinic"])) {
      Cost$PC <-   ModelInpCosts[5,as.numeric(I[["Group"]])];
    }
  }
  
  return(Cost)
}


ComputeCostsOutp <- function(I) {
  Cost <- list();
  Cost$SRow <- as.numeric(I[["SRow"]]);
  Cost$N <- as.numeric(I[["SN"]]);
  hasRep <- lps[Cost$SRow,"HH_Illness_1_report_individual_number"];
  if (is.na(hasRep)) {
    hasRep <- F;
  }else {
    hasRep <- hasRep==as.numeric(I[["SN"]])
  }
  ##Add in each inpatient health event
  Cost$NH <- 0;
  Cost$PH <- 0;
  Cost$DH <- 0;
  Cost$HC <- 0;
  Cost$HV <- 0;
  Cost$TH <- 0;
  Cost$PC <- 0;
  Cost$PP <- 0;
  #Set Model flag by default, indivitual response will clear the flag
  ApplyModel <- T;
  if (hasRep){
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_National_Hospital)) {
      Cost$NH <- lps[Cost$SRow,]$HH_Illness_1_National_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_Provincial_Hospital) ){
      Cost$PH <- lps[Cost$SRow,]$HH_Illness_1_Provincial_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_District_Hospital)) {
      Cost$DH <- lps[Cost$SRow,]$HH_Illness_1_District_Hospital_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_Health_Center)) {
      Cost$HC <- lps[Cost$SRow,]$HH_Illness_1_Health_Center_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_Health_Volunteer)) {
      Cost$HV <- lps[Cost$SRow,]$HH_Illness_1_Health_Volunteer_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_Traditional_Healer)) {
      Cost$TH <- lps[Cost$SRow,]$HH_Illness_1_Traditional_Healer_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_Private_Clinic)) {
      Cost$PC <- lps[Cost$SRow,]$HH_Illness_1_Private_Clinic_cost_Overall_average
    }
    if (!is.na(lps[Cost$SRow,]$HH_Illness_1_Consult_Private_Pharmacist)) {
      Cost$PP <- lps[Cost$SRow,]$HH_Illness_1_Private_Pharmacist_cost_Overall_average
    }
    ApplyModel <- F;
  }
  if(ApplyModel) {
    if(!is.na(I["Care_At_National_hospital"])) {
      Cost$NH <-   ModelOutpCosts[1, as.numeric( I[["Group"]] ) ];
    }
    if(!is.na(I["Care_At_Provincial_hospital/Regional"])) {
      Cost$PH <-   ModelOutpCosts[2,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Care_At_District_hospital"])) {
      Cost$DH <-   ModelOutpCosts[3,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Care_At_Health_centre"])) {
      Cost$HC <-   ModelOutpCosts[4,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Care_At_Village_health_volunteer"])) {
      Cost$HV <-   ModelOutpCosts[5,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Care_At_Traditional_healer"])) {
      Cost$TH <-   ModelOutpCosts[6,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Care_At_Village_Private_clinic"])) {
      Cost$PC <-   ModelOutpCosts[7,as.numeric(I[["Group"]])];
    }
    if(!is.na(I["Care_At_Village_Private_pharmacy"])) {
      Cost$PP <-   ModelOutpCosts[8,as.numeric(I[["Group"]])];
    }
  }
  return(Cost)
}





O2 <- order(lps$HH_Illness_2_Total_cost_Overall_average)
O2 <- O2[!is.na(lps$HH_Illness_2_Total_cost_Overall_average[O2])]
lps$HH_Illness_2_Total_cost_Overall_average[O2]
O2

O3 <- order(lps$HH_Illness_3_Total_cost_Overall_average)
O3 <- O3[!is.na(lps$HH_Illness_3_Total_cost_Overall_average[O3])]
lps$HH_Illness_3_Total_cost_Overall_average[O3]
O3

lps$HH_Illness_2_report_individual_number[96] <- 0;
lps$HH_Illness_2_report_individual_number[238] <- 0;
lps$HH_Illness_2_report_individual_number[327] <- 0;





trim <- function (x) gsub("^\\s+|\\s+$", "", x)
lps$HH_Illness_1_National_Hospital_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_National_Hospital_cost_Overall_average)))
lps$HH_Illness_1_Provincial_Hospital_cost_Overall_average <- as.numeric(trim(as.character(lps$HH_Illness_1_Provincial_Hospital_cost_Overall_average)))
lps$HH_Illness_1_District_Hospital_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_District_Hospital_cost_Overall_average)))
lps$HH_Illness_1_Health_Center_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_Health_Center_cost_Overall_average)))
lps$HH_Illness_1_Health_Volunteer_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_Health_Volunteer_cost_Overall_average)))
lps$HH_Illness_1_Traditional_Healer_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_Traditional_Healer_cost_Overall_average)))
lps$HH_Illness_1_Private_Pharmacist_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_Private_Pharmacist_cost_Overall_average)))
lps$HH_Illness_1_Private_Clinic_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_Private_Clinic_cost_Overall_average)))
lps$HH_Illness_1_Religious_Healer_cost_Overall_average   <- as.numeric(trim(as.character(lps$HH_Illness_1_Religious_Healer_cost_Overall_average)))

RunCostModel <- F;
#RunCostModel <- T;

if(RunCostModel){
  IndivInpCostTab  <-   t(simplify2array( apply (IndiHealth,1,ComputeCosts)))
  save(IndivInpCostTab,file="IndivInpCostTab.RData")

  IndivOutpCostTab  <-   t(simplify2array( apply (IndiHealth,1,ComputeCostsOutp)))
  save(IndivOutpCostTab,file="IndivOutpCostTab.RData")


  ##Bind total cost per Outp indiv to frame
  IndivOutpCost <- apply( IndivOutpCostTab,1,function(Cost){
                           sum(as.numeric(Cost[3:7]),na.rm=T)
                         })
  sapply(ligroups,function(G){mean(IndivOutpCost[G])})
  IndiHealth$OutpCost <- IndivOutpCost

  ##Bind total inp cost per HH to frame
  HHOutpCost <- sapply(1:length(lps[,1]),function(SRow){
                         individs <- which(IndivOutpCostTab[,"SRow"]==SRow)
                         sum(as.numeric(IndivOutpCost[individs]))})
  lps$OutpCost <- HHOutpCost




  ##Bind total cost per indiv Inp to frame
  IndivInpCost <- apply(IndivInpCostTab,1,function(Cost){
                          sum(as.numeric(Cost[3:7]),na.rm=T)
                        })
  sapply(ligroups,function(G){mean(IndivInpCost[G])})
  IndiHealth$InpCost <- IndivInpCost

  ##Bind total Outp cost per HH to frame
  HHInpCost <- sapply(1:length(lps[,1]),function(SRow){
                        individs <- which(IndivInpCostTab[,"SRow"]==SRow)
                        sum(as.numeric(IndivInpCost[individs]))})
  lps$InpCost <- HHInpCost

  #bind total cost to frame
  lps$CostTot <- lps$OutpCost + lps$InpCost
    IndiHealth$CostTot <- IndiHealth$OutpCost +IndiHealth$InpCost
  
}else{
  load(file="IndivInpCostTab.RData")
  load(file="IndivOutpCostTab.RData")
}








##HHInpCost






