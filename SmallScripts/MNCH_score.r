##Family Planning met:
##Not Using - want child
##Using + pregnant intented
# > subset(Codes,Question=="q4_41")
# Question Value                                Label
# 1445    q4_41     1     No because I am already pregnant
# 1446    q4_41     2                  No and not pregnant    
# 1447    q4_41     3                                  IUD
# 1448    q4_41     4                   Injection progest.
# 1449    q4_41     5                        Pill combined
# 1450    q4_41     6                        Pill progest.
# 1451    q4_41     7                               Condom
# 1452    q4_41     8           Ligation (wife or husband)
# 1453    q4_41     9                         Chinese pill
# 1454    q4_41    10                       Natural method
# 1455    q4_41    11 Other (explain): ___________________


##Not using is 1 or 2 in q4_41  
##want child Mothers$Mother_Familily_planning_Want_child

Mothers <- subset(lps,!is.na(Mother_ID))

UnmetFPS <- subset(Mothers,(Mother_Familily_planning_Using==2 & Mother_Familily_planning_Want_child==2) | (Mother_Familily_planning_Using==1 & Mothers$Mother_Familily_planning_Surprise==1))

Mothers$MNCH_FPS<- 1
Mothers[Mothers$Serial %in% UnmetFPS$Serial,"MNCH_FPS"] <-0
Mothers$MNCH_FPS

##Skill Birthing Attendant
Mothers$MNCH_SBA <- 0
Mothers$MNCH_SBA[!is.na(Mothers$Mother_Birth_Assistant_Doctor)] <- 1
Mothers$MNCH_SBA[!is.na(Mothers$Mother_Birth_Assistant_Midwife)] <- 1
Mothers$MNCH_SBA[!is.na(Mothers$Mother_Birth_Assistant_Nurse)] <- 1
Mothers$MNCH_SBA[!is.na(Mothers$Mother_Birth_Assistant_TBA)] <- 1

Mothers$MNCH_ANC <- 0
Mothers$MNCH_ANC[Mothers$Mother_been_to_ANC == 1] <- 1
Mothers$MNCH_ANC

Mothers$MNCH_DPT3 <- 0
Mothers$MNCH_DPT3 [Mothers[,"Mother_Child_Immu_DPT-HepB-Hib3"]==1]<-1
Mothers$MNCH_DPT3 [Mothers[,"Mother_Child_Immu_Date_DPT-HepB-Hib3"]!="" & 
                     Mothers[,"Mother_Child_Immu_Date_DPT-HepB-Hib3"]!="00.00.0000" ]<-1

Mothers$MNCH_DPT3

Mothers$MNCH_BCG <- 0
Mothers$MNCH_BCG [Mothers[,"Mother_Child_Immu_BCG"]==1]<-1
Mothers$MNCH_BCG [Mothers[,"Mother_Child_Immu_BCG_date"]!="" & 
                    Mothers[,"Mother_Child_Immu_BCG_date"]!="00.00.0000" ]<-1
Mothers$MNCH_BCG


Mothers$MNCH_MR <- 0
Mothers$MNCH_MR [Mothers[,"Mother_Child_Immu_MR_12-23"]==1]<- 1
Mothers$MNCH_MR [Mothers[,"Mother_Child_Immu_Date_MR_12-23"]!="" & 
                    Mothers[,"Mother_Child_Immu_Date_MR_12-23"]!="00.00.0000" ]<-1
Mothers$MNCH_MR

Mothers$MNCH_ORS <- 0
Mothers$MNCH_ORS[is.na(Mothers$Mother_Child_Last_diarhea_Received_ORS)] <- 1
Mothers$MNCH_ORS[Mothers$Mother_Child_Last_diarhea_Received_ORS==1] <- 1
Mothers$MNCH_ORS


# 
# > subset(Codes,Question=="q5_21")
# Question Value                                Label
# 1562    q5_21     1                  Provincial hospital
# 1563    q5_21     2                    District hospital
# 1564    q5_21     3                        Health centre
# 1565    q5_21     4                    Village volunteer
# 1566    q5_21     5                   Private Pharmacy 3
# 1567    q5_21     6                 No treatment >>>5.24
# 1568    q5_21     7 Other (explain): ___________________
Mothers$MNCH_CPNM <- 0
Mothers$MNCH_CPNM [is.na(Mothers$Mother_Child_Last_respiratory_First_treatment)] <- 1
Mothers$MNCH_CPNM [Mothers$Mother_Child_Last_respiratory_First_treatment<6] <-1
Mothers$MNCH_CPNM

Mothers$MNCH_MN <-0
Mothers$MNCH_MN[Mothers[,"Mother_Mosquito_net_last_night"]!=3] <-1
Mothers$MNCH_MN

names(Mothers)[grep("MNCH_",names(Mothers))]

Mothers[,c("MNCH_FPS" , "MNCH_SBA"  ,"MNCH_ANC" , "MNCH_DPT3" ,"MNCH_BCG" , "MNCH_MR" ,  "MNCH_ORS" , "MNCH_CPNM" ,"MNCH_MN")]
Weights <- c(1,.5,.5,.5,.25,.25,.5,.5,1)
Weights <- Weights/sum(Weights)
Weights 
Mothers$Score_MNCH <- rowSums(t(t(Mothers[,c("MNCH_FPS" , "MNCH_SBA"  ,"MNCH_ANC" , "MNCH_DPT3" ,"MNCH_BCG" , "MNCH_MR" ,  "MNCH_ORS" , "MNCH_CPNM" ,"MNCH_MN")])*Weights))
Mothers$Score_MNCH

p <- ggplot(data=Mothers)
p <- p + geom_histogram(aes(x=Score_MNCH,y = ..density..),binwidth=.1)
p <- p + facet_grid(.~Group)
p


p <- ggplot(data=Mothers)
p + geom_boxplot(aes(x=Group,y=Score_MNCH))

p <- ggplot(data=Mothers,aes(x=Score,y=Score_MNCH))
p <- p + geom_point(aes(color=Group))
p <- p + stat_smooth(method = "lm",colour="Black",se=F)
p <- p + stat_smooth(colour="Violet",se = F)
p

UnmetFPS[,c("Mother_Familily_planning_Using","Mother_Familily_planning_Want_child","Mother_Familily_planning_Surprise")]

)]


q4_41  <- "Mother_Familily_planning_Using"
Q4_41Tab <- sapply(lgroups,function(G){
  sapply(c(1,3:11),function(P){
    sum(lps[G,q4_41]==P,na.rm=T)
  })
})
Q4_41Tab <- rbind(sapply(lgroups,function(G){sum(lps[G,"Mother_Familily_planning_Want_child"]==1,na.rm=T)}),
                  sapply(lgroups,function(G){sum(lps[G,"Mother_Familily_planning_Want_child"]==2,na.rm=T)}),
                  Q4_41Tab)
Q4_41Tab <- interleave2d(Q4_41Tab,FmtPer(t(t(Q4_41Tab)/colSums(Q4_41Tab ))))
rownames(Q4_41Tab) <- c("No- Not pregnant, want child","No- Not pregnant, don't want child",Codes$Label[which(Codes$Question=="q4_41")][c(1,3:11)])
colnames(Q4_41Tab) <- interleave(names(lgroups),rep("",length(lgroups)))
Q4_41Tab
SaveTables(Q4_41Tab,"Q4_41_Family_Planning_Use","Family planning use")
q4_43 <- "Mother_Familily_planning_Want_child"
