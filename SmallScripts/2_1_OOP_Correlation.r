#Analysis and correlation of general out-of-pocket health expenditure (Q3.3, 3.5, 3.7),
#FMNCH expenditure (Q7.8.7-7.16, Q4.10, 4.18, 4.20, 5.18, 5.23, 5.28) and HEF OOP expenditure (Q8.8-8.16),
#especially for the public health facilities

## Child_Admitted_Name
## Child_Admitted_Number
## Child_Admitted_Record_found
## Child_Admitted_Record_Facility_Name
## Child_Admitted_Record_Procedure
## Child_Admitted_Record_Cost_OOP
## Child_Admitted_Record_Food_allowance
## Child_Admitted_Record_Food_allowance_amount
## Child_Admitted_Record_Outside_HEF_3km_radius
## Child_Admitted_Record_Transport_allowance
## Child_Admitted_Location_Provincial_Hospital
## Child_Admitted_Location_District_Hospital
## Child_Admitted_Location_Health_centre
## Child_Admitted_Location_Private_clinic
## Child_Admitted_Location_Other
## Child_Admitted_Location_Name_Provincial_Hospital
## Child_Admitted_Location_Name_District_Hospital
## Child_Admitted_Location_Name_Health_centre
## Child_Admitted_Location_Name_Other
## Child_Admitted_Types_of_service
## Child_Admit_Cost_OOP
## Child_Admitted_Cost_Drugs_supplies_in_hospital_LAK
## Child_Admitted_Cost_Drugs_supplies_outtside_hospital_LAK
## Child_Admitted_Cost_Service_charge_LAK
## Child_Admitted_Cost_Other_LAK
## Child_Admitted_Cost_Average_LAK
## Child_Admitted_Cost_Other_specify
## Child_Admitted_Code_3_4_5_6
## Child_Admitted_Food_allowance_amount
## Child_Admitted_Food_allowance_receipt_voucher
## Child_Admitted_Food_allowance_daily
## Child_Admitted_Food_allowance_other
## Child_Admitted_Outside_HEF_3km_radius
## Child_Admitted_Transport_allowance_amount
## Child_Admitted_Transport_allowance_voucher
## Child_Admitted_Service_Good
## Child_Admitted_Service_Anamnesis
## Child_Admitted_Service_Weight_height_measurement
## Child_Admitted_Service_Body_Palpation
## Child_Admitted_Service_Blood_Pressure
## Child_Admitted_Service_Stethoscope
## Child_Admitted_Service_Others
## Child_Admitted_Service_Dont_know
## Child_Admitted_Service_Dont_remember
## Child_Admitted_Service_Other_specify
## Child_Admitted_Wait_long
## Child_Admitted_Wait_time
## Child_Admitted_Wait_reason

##82 children addmitted
TotalChildAdmit <- sum(lps$Child_Admitted_Past_year==1,na.rm=T)

##Extract just the Child addmitted subset as CA
CA <- subset(lps,Child_Admitted_Past_year==1)

##Extract vectors of groups identity
CAgroups <- cbind.data.frame(PreID=CA$Group=="PreID",GeoID=CA$Group=="GeoID",NoAssist=CA$Group=="NoAssist",All=rep(T,length(CA$Group)))
#rename the Location Variables
CALocs <- paste("CA",c("PH","DH","HC","PC","O"),sep="_")
names(CA)[which(names(lps)=="Child_Admitted_Location_Provincial_Hospital"):
            which(names(lps)=="Child_Admitted_Location_Other")] <- CALocs


########################################
## Even distribution of addmitance rates
## 
## PreID     GeoID No Assist       All 
##    27        26        29        82 
sapply(CAgroups,sum)

##Everyone has been addmitted to just one center, except 822
rowSums(!is.na(CA[,CALocs]))

########################################
##CA_PH CA_DH CA_HC CA_PC  CA_O 
##   23    46    13     0     1 
colSums(!is.na(CA[,CALocs]))
p <- ggplot(data=CA) + xlab("Addmit Location")
p <- p + geom_bar(aes(x=CALoc,color=Group,fill=Group))
p <- p + facet_grid(.~Child_Admitted_Cost_OOP,labeller=OOPLabler)
p

p <- p + facet_grid(Child_Admitted_Types_of_service~Child_Admitted_Cost_OOP,labeller=OOPLabler)
p
##Child_Admitted_Types_of_service
##      Question Value           Label
## 1663     q7_7     1            IPD,
## 1664     q7_7     2  small surgery,
## 1665     q7_7     3 medium surgery,
## 1666     q7_7     4   major surgery

aggregate(.~Group+CALoc,data=CA[,c("Group","CALoc","Child_Admitted_Cost_OOP",)],FUN=function(X){sum(X==1)/length(X)})
aggregate(.~Group+CALoc,data=CA[,c("Group","CALoc","Child_Admitted_Cost_OOP","Child_Admitted_Cost_Drugs_supplies_in_hospital_LAK")],FUN=mean)

CA[,c("Child_Admitted_Record_Procedure","Child_Admitted_Record_found","Child_Admitted_Record_Cost_OOP")]

## Child_Admitted_Cost_OOP in found records
## [1] 2 1 1 2 2 2 2 2 2 2
## the two who are reported OOP but are not recorded as such are GeoID
subset(CA,Child_Admitted_Record_found==1)[,"Child_Admitted_Cost_OOP"]
subset(CA,Child_Admitted_Record_found==1 & Group=="PreID")[,"Child_Admitted_Cost_OOP"]

## All found records record not OOP
subset(CA,Child_Admitted_Record_found==1)[,"Child_Admitted_Record_Cost_OOP"]
## All found records are divided evenly 5,5 in GEO/Pre
aggregate(.~Group,data=subset(CA,Child_Admitted_Record_found==1)[,c("Group","Child_Admitted_Cost_OOP")],FUN=length)

CARec <- subset(CA,Child_Admitted_Record_found==1)
CARec$Child_Admitted_Record_Food_allowance
CARec$Child_Admitted_Record_Food_allowance_amount
CARec$Child_Admitted_Food_allowance_amount
CARec$Child_Admitted_Food_allowance_receipt_voucher


p <- ggplot(data=subset(CA,Child_Admitted_Code_3_4_5_6==1)[,c("Serial","Group","CALoc","Child_Admitted_Food_allowance_amount")])
p <- p + geom_point(aes(x=CALoc,y=Child_Admitted_Food_allowance_amount,color=Group),position=position_jitter(w=.2,h=0))
p
                 



CACost <- subset(CA,Child_Admitted_Cost_OOP==1)[,c("Serial","Group","CALoc","Child_Admitted_Cost_Drugs_supplies_in_hospital_LAK","Child_Admitted_Cost_Drugs_supplies_outtside_hospital_LAK","Child_Admitted_Cost_Service_charge_LAK","Child_Admitted_Cost_Other_LAK","Child_Admitted_Cost_Average_LAK")]
colnames(CACost) <- c("Serial","Group","CALoc","Drugs Inside","Drugs Outtside","Service_charge","Other","Total")
CACost[CACost==98] <- NA
CACost[CACost==99] <- NA
CACost
library(reshape)
CAMelt <- melt(CACost,id=c("Group","CALoc","Serial"))
p <- ggplot(data=CAMelt)
p  <- p + geom_point(aes(x=variable,y=value,color=Group),alpha=.5)
print(p)
                     
