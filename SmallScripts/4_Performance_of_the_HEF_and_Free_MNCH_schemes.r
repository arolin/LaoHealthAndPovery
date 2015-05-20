##Free_HS_for_CU5_in_Sekong_Known
q7 <- c("Free_HS_for_CU5_in_Sekong_Known","Free_HS_for_CU5_in_Sekong_Belive_can_access","Free_HS_for_CU5_in_Sekong_Includes_Preventive_services","Free_HS_for_CU5_in_Sekong_Includes_Outpatient_consultations","Free_HS_for_CU5_in_Sekong_Includes_Admissions","Free_HS_for_CU5_in_Sekong_Includes_Dontknow","Free_HS_for_CU5_in_Sekong_At_level_HC","Free_HS_for_CU5_in_Sekong_At_level_DH","Free_HS_for_CU5_in_Sekong_At_level_PH","Free_HS_for_CU5_in_Sekong_At_level_National_Hospital","Free_HS_for_CU5_in_Sekong_At_level_Dontknow","Free_HS_for_CU5_in_Sekong_Info_source_Head_of_village","Free_HS_for_CU5_in_Sekong_Info_source_Village_health_volunteer","Free_HS_for_CU5_in_Sekong_Info_source_Health_center_staff","Free_HS_for_CU5_in_Sekong_Info_source_Family_Friends","Free_HS_for_CU5_in_Sekong_Info_source_Other","Free_HS_for_CU5_in_Sekong_Info_source_Other_specify","Child_Admitted_Past_year","Child_Admitted_Name","Child_Admitted_Number","Child_Admitted_Record_found","Child_Admitted_Record_Facility_Name","Child_Admitted_Record_Procedure","Child_Admitted_Record_Cost_OOP","Child_Admitted_Record_Food_allowance","Child_Admitted_Record_Food_allowance_amount","Child_Admitted_Record_Outside_HEF_3km_radius","Child_Admitted_Record_Transport_allowance","Child_Admitted_Location_Provincial_Hospital","Child_Admitted_Location_District_Hospital","Child_Admitted_Location_Health_centre","Child_Admitted_Location_Private_clinic","Child_Admitted_Location_Other","Child_Admitted_Location_Name_Provincial_Hospital","Child_Admitted_Location_Name_District_Hospital","Child_Admitted_Location_Name_Health_centre","Child_Admitted_Location_Name_Other","Child_Admitted_Types_of_service","Child_Admitted_Cost_OOP","Child_Admitted_Cost_Drugs_supplies_in_hospital_LAK","Child_Admitted_Cost_Drugs_supplies_outtside_hospital_LAK","Child_Admitted_Cost_Service_charge_LAK","Child_Admitted_Cost_Other_LAK","Child_Admitted_Cost_Average_LAK","Child_Admitted_Cost_Other_specify","Child_Admitted_Code_3_4_5_6""Child_Admitted_Food_allowance_amount","Child_Admitted_Food_allowance_receipt_voucher","Child_Admitted_Food_allowance_daily","Child_Admitted_Food_allowance_other","Child_Admitted_Outside_HEF_3km_radius","Child_Admitted_Transport_allowance_amount","Child_Admitted_Transport_allowance_voucher","Child_Admitted_Service_Good","Child_Admitted_Service_Anamnesis","Child_Admitted_Service_Weight_height_measurement","Child_Admitted_Service_Body_Palpation","Child_Admitted_Service_Blood_Pressure","Child_Admitted_Service_Stethoscope","Child_Admitted_Service_Others","Child_Admitted_Service_Dont_know","Child_Admitted_Service_Dont_remember","Child_Admitted_Service_Other_specify","Child_Admitted_Wait_long","Child_Admitted_Wait_time","Child_Admitted_Wait_reason")
which(sapply(q7,function(q){length(which(names(lps)==q))==1}))


##Questions on knowledge: 7.1 to 7.4
q7_1_a <- "Free_HS_for_CU5_in_Sekong_Known"
q7_1_b <- "Free_HS_for_CU5_in_Sekong_Belive_can_access"

##Explore who answered Q7 - everyone with a CU5
q7_1ans <- !is.na(lps[,q7_1_a])
max(apply(lps[q7_1ans,qage],1,min,na.rm=T))
min(apply(lps[!q7_1ans,qage],1,min,na.rm=T))



Q7_Tab <- data.frame();
NumCU5 <- sapply(lgroups,function(G){sum(q7_1ans[G])})
NumCU5F <- interleave(NumCU5,rep(NA,length(NumCU5)))

q7_1yesBG <- sapply(lgroups,function(G){sum(lps[G,q7_1_a]==1,na.rm=T)})
q7_1PyesBG <- sprintf(100*q7_1yesBG/NumCU5,fmt="%.1f%%")
q7_1F <- interleave(q7_1yesBG,q7_1PyesBG)

q7_1bYes <- sapply(lgroups,function(G){sum(lps[G,q7_1_b]==1,na.rm=T)})
q7_1bPyes <- sprintf(100*q7_1bYes/q7_1yesBG,fmt="%.1f%%")
q7_1bF <- interleave(q7_1bYes,q7_1bPyes)

##services
q7_2_Services   <- c("Includes_Preventive_services","Includes_Outpatient_consultations","Includes_Admissions","Includes_Dontknow")

q7_2 <- sapply(q7_2_Services,function(X){paste("Free_HS_for_CU5_in_Sekong",X,sep="_")})
q7_2Yes <- sapply(lgroups,function(G){sapply(q7_2,function(Q){sum(!is.na(lps[G,Q]))})})
q7_2PYes <-  t(t(100*q7_2Yes)/q7_1yesBG)
q7_2PYesF <- apply(q7_2PYes,1,function(X){sprintf(X,fmt="%.1f%%")})
q7_2F <- interleave2d(q7_2Yes,t(q7_2PYesF))
q7_2F



length(q7_2Yes)
dim(q7_2Yes)

q7_3_level   <- c("At_level_HC","At_level_DH","At_level_PH","At_level_National_Hospital")
q7_3 <- sapply(q7_3_level,function(L){paste("Free_HS_for_CU5_in_Sekong_",L,sep="")})
q7_3Yes <- sapply(lgroups,function(G){sapply(q7_3,function(Q){sum(!is.na(lps[G,Q]))})})
q7_3PYes <-  t(t(100*q7_3Yes)/q7_1yesBG)
q7_3PYesF <- apply(q7_3PYes,1,function(X){sprintf(X,fmt="%.1f%%")})
q7_3F <- interleave2d(q7_3Yes,t(q7_3PYesF))
q7_3F


q7_4   <- c("Free_HS_for_CU5_in_Sekong_Info_source_Head_of_village","Free_HS_for_CU5_in_Sekong_Info_source_Village_health_volunteer","Free_HS_for_CU5_in_Sekong_Info_source_Health_center_staff","Free_HS_for_CU5_in_Sekong_Info_source_Family_Friends","Free_HS_for_CU5_in_Sekong_Info_source_Other")
sources <- rowSums(!is.na(lps[,q7_4]))
know <- lps[,q7_1_a]==1
know[is.na(know)]=F
NumSources <- sapply(lgroups,function(G){sapply(levels(factor(sources[know])),function(L){sum(sources[G & know]==L)})})
NumSourcesP <- sprintf(100*t(t(NumSources)/q7_1yesBG),fmt="%.1f%%")
NumSourcesP
NumSources <- interleave2d(NumSources,NumSourcesP)
rownames(NumSources) <- levels(factor(sources[know]))
colnames(NumSources) <- interleave(paste("N",names(lgroups)),paste("%",names(lgroups)))
NumSources
SaveTables(NumSources,"q7_4_Number_of_sources_used_by_women_in_the_know","Number of different sources of information about U5C care used by women who know it is available.")

q7_4_Use <- sapply(lgroups,function(G){sapply(q7_4,function(Q){sum(!is.na(lps[G,Q]))})})
q7_4_UseP <- sprintf(100*t(t(q7_4_Use)/q7_1yesBG),fmt="%.1f%%")
q7_4F <- interleave2d(q7_4_Use,q7_4_UseP)
q7_4F

q7_4_sources <- paste("Info from",c("Head_of_village","Village_health_volunteer","Health_center_staff","Family_Friends","Radio"))

Q7_Tab <- rbind(NumCU5F,q7_1F,q7_1bF,q7_2F,q7_3F,q7_4F)
rownames(Q7_Tab)<- c("Num CU5","Know of free services","Believe can acces (in know)",q7_2_Services,q7_3_level,q7_4_sources)
Q7_Tab
SaveTables(Q7_Tab,"q7_1..4_Knowledge_of_free_CU5_care","Dispersal of information about free care for children under 5 (CU5)")

###Questions on payments: 4.18, 7.8
q4_18 <- "Mother_Birth_Cost_delivery"
q4_18Count <- sapply(lgroups,function(G){sum(!is.na(lps[G,q4_18]))})
q4_18Mean  <- formatC(sapply(lgroups,function(G){mean(lps[G,q4_18],na.rm=T)}),format="f",big.mark=",",digits=0)
q4_18Median  <- sapply(lgroups,function(G){median(lps[G,q4_18],na.rm=T)})
q4_18Zeros   <- sapply(lgroups,function(G){sum(lps[G,q4_18]==0,na.rm=T)})
q4_18MeanPaid <-  sapply(lgroups,function(G){mean(lps[G&lps[,q4_18]!=0,q4_18],na.rm=T)})
q4_18MeanPaid <- formatC(q4_18MeanPaid,format="f",digits=0,big.mark=",")
Q4_18Tab <- rbind(interleave(q4_18Zeros,sprintf(100*q4_18Zeros/q4_18Count,fmt="%.1f%%")),
                  interleave(q4_18Count-q4_18Zeros,q4_18MeanPaid),
                  interleave(q4_18Count,q4_18Mean))
rownames(Q4_18Tab) <- c("Paid 0","Payers (mean paid)","All (mean paid)")
Q4_18Tab
SaveTables(Q4_18Tab,"Q4_18_Payment_for_delivery","Amount paid for delivery")

q7_5 <- "Child_Admitted_Past_year"
q7_5Admit <- sapply(lgroups,function(G){sum(lps[G,q7_5]==1,na.rm=T)})
q7_5Admit

q7_8 <- "Child_Admitted_Cost_OOP"
q7_8P <- !is.na(lps[,q7_8])
lps[q7_8P,q7_8]
q7_8OOPs <- !is.na(lps[,q7_8])
q7_8Count <- sapply(lgroups,function(G){sum(q7_8OOPs[G])})
q7_8Count
q7_8OOP <- sapply(lgroups,function(G){sum((lps[G,q7_8])==1,na.rm=T)})
q7_8OOP

Q7_8Tab <- rbind(interleave(q7_8Count,sprintf(100*q7_8Count/NumCU5,fmt="%.1f%%")),
                 interleave(q7_8OOP,sprintf(100*q7_8OOP/q7_8Count,fmt="%.1f%%")))
rownames(Q7_8Tab) <- c("Num Addmitted (%of CU5)","Num OOP (%of admitted)")
Q7_8Tab
SaveTables(Q7_8Tab,"q7_8_CU5_Addmitance_and_OOP","Out of pocket expenditures for CU5 addmitance")

## SumGroup <- function(G,Var) {
##   Zeros <- sum(PatFrame[G,Var]==0);
##   DK <-lps[,Var]==98 | lps[,Var]==99
##   Count  <- sum(!is.na(lps[G,Var]));
##   Total <- sum(PatFrame[G& !DK,Var],na.rm=T)
##   Mean <- mean(PatFrame[G& !DK,Var],na.rm=T)
##   DK <- sum(DK[G]);
##   list(Zeros=Zeros,DK=DK,Count=Count,Total=Total,Mean=Mean,Median=Median,Max=Max)
## }


## q7_9 <- c("Child_Admitted_Cost_Drugs_supplies_in_hospital_LAK","Child_Admitted_Cost_Drugs_supplies_outtside_hospital_LAK","Child_Admitted_Cost_Service_charge_LAK","Child_Admitted_Cost_Other_LAK","Child_Admitted_Cost_Average_LAK","Child_Admitted_Cost_Other_specify")
## rowSums(!is.na(lps[q7_8OOPs,q7_9]))
## c <- ("Drugs supplies in hospital LAK","Drugs supplies outtside hospital LAK","Service charge LAK","Other LAK","Average LAK","Other specify")


###Questions on allowances received: 4.20, 4.25, 7.10, 7.15
q4_20 <- "Mother_Birth_Hospital_Food_allowance"
q4_20s <- !is.na(lps[,q4_20])
NumHospitalBirths <- sapply(lgroups,function(G){sum(G&q4_20s)})
RecievedAllowance <- sapply(lgroups,function(G){sum(lps[G&q4_20s,q4_20]==1)})

q4_21 <- "Mother_Birth_Hospital_Food_allowance_Amount"
q4_21s <- !is.na(lps[,q4_21])
q4_21Mean <- formatC(sapply(lgroups,function(G){mean(lps[G&q4_21s,q4_21])}),format="f",big.mark=",",digits=0)
q4_21Median <- formatC(sapply(lgroups,function(G){median(lps[G&q4_21s,q4_21])}),format="f",big.mark=",",digits=0)

q4_25 <- "Mother_Birth_Delivery_was_at_hospital_more_3k"
q4_25s  <- !is.na(lps[,q4_25])
q4_25s <-  q4_25s & lps[,q4_25]==1
HospitalFar  <- sapply(lgroups,function(G){sum(lps[G,q4_25]==1,na.rm=T)})
q4_26 <- "Mother_Birth_Transportation_allowance"
q4_26Mean <- formatC(sapply(lgroups,function(G){mean(lps[G&q4_25s,q4_26])}),format="f",big.mark=",",digits=0)
q4_26Median <- formatC(sapply(lgroups,function(G){median(lps[G&q4_25s,q4_26])}),format="f",big.mark=",",digits=0)


Q4_20Tab <- rbind(interleave (NumHospitalBirths,sprintf(100*NumHospitalBirths/NumMothers,fmt="%1.f%%")),
                  interleave (RecievedAllowance,sprintf(100*RecievedAllowance/NumHospitalBirths,fmt="%1.f%%")),
                  interleave (q4_21Mean,q4_21Median),
                  interleave (HospitalFar,sprintf(100*HospitalFar/NumHospitalBirths,fmt="%1.f%%")),
                  interleave (q4_26Mean,q4_26Median))

rownames(Q4_20Tab) <- c("Hospital Births (%of total births)","Recieved Food Allowance (%of hospital births)","Food Allowance LAK (mean/median)","Hospital >3k (% hospital births)","Transport Allowance LAK (mean/median)")
Q4_20Tab
SaveTables(Q4_20Tab,"q4_20-25_Food_and_transportation_allowance_for_hospital_births","Food and transportation allowances for hospital births")



##7.10, 7.15 Questions on allowances received:
CodeLables[["q7_7"]][1,]
q7_7 <- "Child_Admitted_Types_of_service"
NumAdmitted <- sum(!is.na(lps[,q7_7]))
NumAdmitted
sapply(CodeLables[["q7_7"]][1,],function(C){sum(lps[,q7_7]==C,na.rm=T)})
q7_10 <- "Child_Admitted_Code_3_4_5_6"
NumAdmitted <- sum(!is.na(lps[,q7_10]))
NumCode3456 <- sum(lps[,q7_10]==1,na.rm=T)
NumCode3456






##Questions on satisfaction: 7.17, 7.21
q7_19 <- "Child_Admitted_Service_Good"
q7_19s <- !is.na(lps[,q7_19])
lps[q7_19s,q7_19]
ChildAdmitted <- sapply(lgroups,function(G){sum(q7_19s[G])})
GoodService <- sapply(lgroups,function(G){sum(lps[q7_19s&G,q7_19]==1)})
GoodService

q7_21 <- "Child_Admitted_Wait_long"
WaitLong  <- sapply(lgroups,function(G){sum(lps[G,q7_21]==1,na.rm=T)})
WaitLong

Q7_SatTab <- rbind.data.frame(interleave(ChildAdmitted,rep(NA,length(ChildAdmitted))),
                   interleave(GoodService,FmtPer(GoodService/ChildAdmitted)),
                   interleave(WaitLong,FmtPer(WaitLong/ChildAdmitted)))
rownames(Q7_SatTab) <- c("Num Child Admitted","Report Good Service (%of admitted)","Report Widted Long (%of admitted)")
N <- interleave(names(ChildAdmitted),rep("",length(ChildAdmitted)))
colnames(Q7_SatTab) <- N
N                
print(Q7_SatTab)
SaveTables(Q7_SatTab,"Q7_17.21_Chiled_admitted_satifcation_measure","Measure of satisfaction with child inpatient procedure")


## Questions on knowledge: 8.1 to 8.4
## HEF_Knowledge
## HEF_Recieved_card
## HEF_Kept_card
## HEF_Free_service_Know
## HEF_Free_service_Available_to
## HEF_Free_service_level_HC
## HEF_Free_service_level_DH
## HEF_Free_service_level_PH
## HEF_Free_service_level_National_hospital
## HEF_Free_service_level_Dont_know
## HEF_Member_kown_rights_Preventive_services
## HEF_Member_kown_rights_Outpatient_consultations
## HEF_Member_kown_rights_Admissions
## HEF_Member_kown_rights_Food_transport_all
## HEF_Member_kown_rights_Food_transport_hospitals_only
## HEF_Member_kown_rights_Dont_know
hgroups <- list(PreID=lgroups[[1]],GeoID=lgroups[[2]],AllHEF=lgroups[[1]]|lgroups[[2]])
hgroups


NumGroups <- sapply(hgroups,function(G){sum(!is.na(lps[G,"HEF_Knowledge"]))})
NumGroups
q8_1_1 <- "HEF_Knowledge";
NumKnow <- sapply(hgroups,function(G){sum(lps[G,q8_1_1]==1)})
NumKnow
q8_1_2 <- "HEF_Recieved_card";
NumRecv <- sapply(hgroups,function(G){sum(lps[G,q8_1_2]==1,na.rm=T)})
NumRecv
q8_1_3 <- "HEF_Kept_card";
NumKept <- sapply(hgroups,function(G){sum(lps[G,q8_1_3]==1,na.rm=T)})
NumKept
q8_2_a <- "HEF_Free_service_Know"
FreeKnow <- sapply(hgroups,function(G){sum(lps[G,q8_2_a]==1,na.rm=T)})
FreeKnow
q8_2_b <- "HEF_Free_service_Available_to"
FreeAvail <- sapply(hgroups,function(G){sum(lps[G,q8_2_b]==1,na.rm=T)})
FreeAvail
#sum (lps[,q8_2_b]==1 & lps[,q8_2_a]!=1,na.rm=T)

FreeLevels <- c("HEF_Free_service_level_HC","HEF_Free_service_level_DH","HEF_Free_service_level_PH","HEF_Free_service_level_National_hospital","HEF_Free_service_level_Dont_know")
FreeLoc <- sapply(hgroups,function(G){sapply(FreeLevels,function(F){sum(!is.na(lps[G,F]))})})

FreeRights <- c("HEF_Member_kown_rights_Preventive_services","HEF_Member_kown_rights_Outpatient_consultations","HEF_Member_kown_rights_Admissions","HEF_Member_kown_rights_Food_transport_all","HEF_Member_kown_rights_Food_transport_hospitals_only","HEF_Member_kown_rights_Dont_know")
FreeRightsK <- sapply(hgroups,function(G){sapply(FreeRights,function(F){sum(!is.na(lps[G,F]))})})
FreeRightsRowNames <- c("Kown rights Preventive services","Kown rights Outpatient consultations","Kown rights Admissions","Kown rights Food transport all","Kown rights Food transport hospitals only","Kown rights Dont know")


HEFTab <- rbind(interleave(NumGroups,rep(NA,length(names(NumGroups)))),
                interleave(NumKnow,FmtPer(NumKnow/NumGroups)),
                interleave(NumRecv,FmtPer(NumRecv/NumKnow)),
                interleave(NumKept,FmtPer(NumKept/NumKnow)),
                interleave(FreeKnow,FmtPer(FreeKnow/NumGroups)),
                interleave(FreeAvail,FmtPer(FreeAvail/NumGroups)),
                interleave2d(FreeLoc,FmtPer(t(t(FreeLoc)/FreeKnow))),
                interleave2d(FreeRightsK,FmtPer(t(t(FreeRightsK)/FreeKnow))))
HEFTab


paste(FreeRightsRowNames,"(%of know free)")

rownames(HEFTab) <- c("Count",
                      "Know HEF (%of group)",
                      "Num Reciev Card (%of know)",
                      "Num Kept Card (%of know)",
                      "Num know free services (%of group)",
                      "Free services avaialbe (%of know free)",
                      paste(FreeLevels,"(%of know free)"),
                      paste(FreeRightsRowNames,"(%of know free)"))
colnames(HEFTab) <- interleave(as.character(names(NumGroups)),rep(" ",length(names(NumGroups))))
HEFTab
SaveTables(HEFTab,"Q8_1..4_HEF_Awareness","Member awareness of HEF Services")

interleave(as.character(names(NumGroups)),rep("",length(names(NumGroups))))

length(interleave(names(NumGroups),rep("",length(names(NumGroups)))))

names(NumGroups)
## Questions on payments: 8.8
## Questions on allowances received: 8.10, 8.15
## Questions on satisfaction: 8.19, 8.21
