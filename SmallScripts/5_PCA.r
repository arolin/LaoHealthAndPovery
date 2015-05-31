Score <- apply(lps,1,function(lps){
        ## persons in Household	1	2 to 10	> 10			
        ## Score	0	1	0			
        Score <- 0;
        if(as.numeric(lps["HH_NumPeople"]) >= 2 & as.numeric(lps["HH_NumPeople"]) <= 10){
          Score <- Score + 1;
        }

        ## Sex HoH	Woman	Man				
        ## Sex of HOH	0	1
        if(as.numeric(lps["HoH_Sex"])==2) {
          Score <- Score+1;
        }

        ## 1	Cannot write/read
        ## 2	Can read
        ## 3	Can read / write
        ## Litteracy level	Cannot write/read	Can read	Can read/write			
        ## HoH	0	1	2
        Score <- Score + as.numeric(lps["HoH_Literacy_Level"]) -1;
        ## Spouse of HoH	0	1	2			
        Score <- Score + as.numeric(lps["HoH_Spouse_Literacy_Level"]) -1;

        paste("q2_3_",1:30,sep="")
        ## Children 0 to 14 years	4 or more	0 to 3				
        ## Score	0	1				
        NU14 <- sum(as.numeric(lps[paste("q2_3_",1:30,sep="")]) <= 14,na.rm=T)
        if(NU14<4) {
          Score <- Score+1;
        }
        
        ## Adults 20 to 60 years	0 or 1	2 or more				
        ## Score	0	1				
        N20to60 <- sum(as.numeric(lps[paste("q2_3_",1:30,sep="")]) >= 20 & as.numeric(lps[paste("q2_3_",1:30,sep="")])<=60,na.rm=T)
        if(N20to60>=2) {
          Score <- Score+1;
        }

        ## House walls (*)	Bamboo Leaves Grass	Wood	Brick-stone ciment	if mixed, record the highest		
        ## Score	0	2	3			
        if (!is.na(lps["HH_Housing_Material_Brick"]) |
            !is.na(lps["HH_Housing_Material_Concrete"])) {
          Score <- Score+3;
        } else if (!is.na(lps["HH_Housing_Material_Wood"])) {
          Score <- Score+2;
        }

        
        ## House roof (*)	Grass  Leaves	Bamboo Wood	Iron Fibro	if mixed, record the highest		
        ## Score	0	2	3
        if (!is.na(lps["HH_Roofing_Material_Corrugated_iron"]) |
            !is.na(lps["HH_Roofing_Material_Tiles"])) {
          Score <- Score+3;
        } else if (!is.na(lps["HH_Roofing_Material_Wood"])) {
          Score <- Score+2;
        }
        
        ## TV	No	Yes				
        ## Score	0	2				
        if (as.numeric(lps["HH_Posses_Television"]) > 0) {
          Score <- Score+2;
        }
        
        ## VDO/VCD	No	Yes				
        ## Score	0	2				
        if(as.numeric(lps["HH_Posses_Video"]) > 0) {
          Score <- Score + 2;
        } 
        ## Bicycle	No	Yes				
        ## Score	0	1				
        if(as.numeric(lps["HH_Posses_Bicycle"]) >0) {
          Score <- Score + 1;
        }
        
        ## Motorbike	No	Yes				
        ## Score	0	3				
        if(as.numeric(lps["HH_Posses_Motorbike"]) > 0) {
          Score <- Score + 3;
        }
        
        ## Tractor/motor boat	No	Yes				
        ## Score	0	3				
        if (as.numeric(lps["HH_Posses_Tractor"])  > 0 | as.numeric(lps["HH_Posses_Tractor"]) > 0) {
          Score <- Score +3;
        }
        
        ## HH_Latrine_type_Modern_toilet
        ## HH_Latrine_type_Normal_toilet
        ## HH_Latrine_type_Dry_toilet   
        ## HH_Latrine_type_Other        
        ## HH_Latrine_type_None         
        ## Toilets	No	Basic (dry)	Modern			
        ## Score	0	2	3			
        if (!is.na(lps["HH_Latrine_type_Modern_toilet"])) {
          Score <- Score + 3;
        } else if (!is.na(lps["HH_Latrine_type_Normal_toilet"]) |
                   !is.na(lps["HH_Latrine_type_Dry_toilet"])) {
          Score <- Score + 2;
        }
        
        ## Energy fo cooking	Wood/ sawdust	Charcoal	Gaz/electricity			
        ## Score	0	2	4			
        if(as.numeric(lps["HH_Energy_type"]) == 3) {
          Score <- Score + 4;
        } else if (as.numeric(lps["HH_Energy_type"]) == 2) {
          Score <- Score + 2;
        } 
        
        ## Plant Cereals (Bag 30kg)	"0-500 Kg
        ## (16 Bags)"	"501-1000Kg
        ## (17-33 bags)"	"1001-1500Kg
        ## (34-50 bags)"	">1,500Kg
        ## (>50Bags)"		
        ## Score	0	1	2	3		
        if (as.numeric(lps["9.6PlantCereals(Bag30kg)"]) == 5) {
          Score <- Score + 3;
        }else if (as.numeric(lps["9.6PlantCereals(Bag30kg)"]) == 4) {
          Score <- Score + 2;
        }else if (as.numeric(lps["9.6PlantCereals(Bag30kg)"]) == 3) {
          Score <- Score + 1;
        }
        
        ## Cash Income/year      Use guidelines below
        ## < 500,000
        ## 500,000 to 1,500,000
        ## 1,500,001 to 2,500,000
        ## 2,500,001 to 3,500,000
        ## 3,500,001 to 4,500,000
        ## more than 4,500,000
        ## Score	0	1	2	3	4	5
        if (as.numeric(lps["HH_Cash_income"]) > 4500000) {
          Score <- Score + 5;
        } else if (as.numeric(lps["HH_Cash_income"]) > 3500000) {
          Score <- Score + 4;
        } else if (as.numeric(lps["HH_Cash_income"]) > 2500000) {
          Score <- Score + 3;
        } else if (as.numeric(lps["HH_Cash_income"]) > 1500000) {
          Score <- Score + 2;
        } else if (as.numeric(lps["HH_Cash_income"]) >  500000) {
          Score <- Score + 1;
        }
        
        ## Own a shop	No	Yes				
        ## Score	0	1				
        if (!is.na(lps["HH_Income_Secondary_Shop_restaurant"])) {
          Score  <- Score + 1;
        }
        
        ## Chick/duck/….Nber
        ## 0 to 10
        ## 11 to 20
        ## 21 to 30
        ## 31 to 40
        ## 41 and more	
        ## Score	0	1	2	3	4	
        if (as.numeric(lps["HH_Livestock_N_Chickens"]) >40) {
          Score  <- Score + 4;
        } else if (as.numeric(lps["HH_Livestock_N_Chickens"]) >30) {
          Score  <- Score + 3;
        } else if (as.numeric(lps["HH_Livestock_N_Chickens"]) >20) {
          Score  <- Score + 2;
        } else if (as.numeric(lps["HH_Livestock_N_Chickens"]) >10) {
          Score  <- Score + 1;
        }

        ## Pig/Goat          Nber	0
        ## 1 or 2
        ## 3 or 4
        ## 5 or 6
        ## 7 and more	
        ## Score	0	1	2	3	4	
        if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 6 ) {
          Score <- Score + 4;
        } else if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 3 ) {
          Score <- Score + 3;
        } else if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 1 ) {
          Score <- Score + 2;
        } else if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 0 ) {
          Score <- Score + 1;
        }
        
        ## Buff/cow/horse   Nber	0
        ## 1
        ## 2 or 3
        ## 4 or more		
        ## Score	0	1	3	5		
        if ( (as.numeric(lps["HH_Livestock_N_Cattle"]) + as.numeric(lps["HH_Livestock_N_Buffalo"])) > 3) {
          Score <- Score + 5;
        } else if ( (as.numeric(lps["HH_Livestock_N_Cattle"]) + as.numeric(lps["HH_Livestock_N_Buffalo"])) > 1) {
          Score <- Score + 3;
        } else if ( (as.numeric(lps["HH_Livestock_N_Cattle"]) + as.numeric(lps["HH_Livestock_N_Buffalo"])) > 0) {
          Score <- Score + 1;
        }
        return (Score)
      }
      )

cgroups <- lapply(lgroups,function(G){G[c(96,238,883)]=F;return(G)})
sapply(cgroups,sum)

PCARes <- rbind(formatC(sapply(lgroups,function(G){mean(Score[G])}),1,format="f"),
                sapply(lgroups,function(G){median(Score[G])}),
                FmtPer(sapply(lgroups,function(G){sum(Score[G]>16)/sum(G)})),
                FmtPer(sapply(lgroups,function(G){sum(Score[G]>25)/sum(G)})))
rownames(PCARes) <- c("Mean Score","Median Score","% above 16","% above 25")
PCARes
SaveTables(PCARes,"PCA_Results","")

SaveTables(sapply(lgroups,function(G){summary(Score[G])}),"PCA_Summary","")

for (i in 1:3) {
  name <- names(lgroups)[i];
  png(filename=paste("./output/",name,"_PCA_hist.png",sep=""))
  plot(hist(Score[lgroups[[i]]],1:50,freq=T),ylim=c(0,40),xlab="PCA Score",ylab="Count",main=paste("PCA Wealth Scores for",name))
  dev.off()
}


h <- lapply(lgroups,function(G){hist(Score[G],40)})
plot(h[[3]],col = rgb(0,0,1),xlab="PCA Score",ylab="Count",main="PCA Score distribution by group",legend=c("PreID","GeoID","NoAssist"))
plot(h[[2]],col = rgb(0,1,0),add=T)
plot(h[[1]],col = rgb(1,0,0),add=T)


h1 <- hist(Score[is.na(lps$HH_Insurance_HEFCard) & lps$Group=="PreID"],40)
h2 <- hist(Score[!is.na(lps$HH_Insurance_HEFCard)],40)
plot(h1,col = rgb(0,0,1),xlab="PCA Score",ylab="Count",main="PCA Score distribution by group")
plot(h2,col=rgb(1,0,0),add=T)


##1.6.7


print(sapply(lgroups,function(G){mean(Score[G])}))

print(100*sapply(lgroups,function(G){sum(Score[G]<16)/sum(G)}),digits=1)

breaks <- matrix(c(0,5,5,10,10,15,15,20,20,25,25,30,30,35,35,40),2,8)
apply(breaks,2,function(B){Score>=B[1] & Score<B[2]})


boxplot(lapply(lgroups,function(G){Score[G]}))
lpoor <- list(VillagePoor=lps$VilPoor==1,NotPoor=lps$VilPoor!=1)

GroupNPoor <- interleave2d( sapply(lgroups,function(G){G&lpoor[[2]]}),sapply(lgroups,function(G){G&lpoor[[1]]}))
colnames(GroupNPoor) <- c("PreID","PreID Vil Poor","GeoID","GeoID Vil Poor","No Assist","No Assist Vil Poor","All","All Vil Poor")


png("./output/VillagePoor.png")
b <- boxplot(apply(GroupNPoor,2,function(G){Score[G]}),names=colnames(GroupNPoor),las=2,main="PCA Score Comparison With Village Poor List")
rownames(GroupNPoor)
GroupNPoor.Means <- apply(GroupNPoor,2,function(G){mean(Score[G])})
text(seq_along(GroupNPoor[1,]),b$stats[3,]+1,paste("N=",b$n))
text(seq_along(GroupNPoor[1,]),b$stats[3,]-1,formatC(GroupNPoor.Means,1,format="f"))
points(GroupNPoor.Means,pch=10,col=rgb(1,0,0))
dev.off()

insQ <- which(names(lps)=="HH_Insurance_Civil_servants_(SASS)"):which(names(lps)=="HH_Insurance_Informal_no_insurance")
insQns <- c("Civil_servants_(SASS)","Police_military","Private_Employee_(SSO)","Informal_CBHI","Informal_Private","HEFCard","GVTPoor","Informal_no_insurance")
insGroups <- lapply(insQ,function(I){!is.na(lps[,I])})


for (n in c(1,2,3,4,6,7,8)) {
  L <- insQ[n];
  png(filename=paste("./output/",insQns[n],".png",sep=""))
  b <- boxplot(lapply(lgroups,function(G){Score[G & !is.na(lps[,insQ[n]])]}),ylim=c(0,50),main=insQns[n])
  text(seq_along(lgroups),b$stats[3,]+1,paste("N=",b$n))
  text(seq_along(lgroups),b$stats[3,]-1,b$stats[3,])
  dev.off()
}

png("./output/GVTPoor_HEF.png")
HEFv <- list(lgroups[[1]]&is.na(lps$HH_Insurance_HEFCard),lgroups[[1]]&!is.na(lps$HH_Insurance_HEFCard))
boxplot(lapply(HEFv,function(I){Score[I]}),names=c("Government Poor","HEF Card"),ylan="PCA Score")
text(seq_along(lgroups),b$stats[3,]+1,paste("N=",b$n))
text(seq_along(lgroups),b$stats[3,]-1,b$stats[3,])
dev.off()


SummCosts <- function(V) {
  sapply(cgroups,function(G){
         costs <- !is.na(V)
         costs <- V[G & costs]
         Counts <- length(costs);
         Zeros <- sum(costs==0);
         DK <- costs==98 | costs==99;
         Mean <- mean(costs[!DK])
         Median  <-median(costs[!DK])
         return (list(Zeros=Zeros,DK=sum(DK),Counts=Counts,Mean=Mean,Median=Median))
       })
}


OutpTCOA <- SummCosts(lps$HH_Illness_1_Total_cost_Overall_average)
SaveTables(OutpTCOA,"q3.3.10.5_Out_Patient_Total_cost_Overall_average","Total_cost_Overall_average")
print(OutpTCOA)
Inp1TCOA <- SummCosts(lps$HH_Illness_2_Total_cost_Overall_average)
SaveTables(Inp1TCOA,"q3.5.10.5_In_Patient1_Total_cost_Overall_average","Total_cost_Overall_average")
print(Inp1TCOA)
Inp2TCOA <- SummCosts(lps$HH_Illness_3_Total_cost_Overall_average)
SaveTables(Inp2TCOA,"q3.7.10.5_In_Patient2_Total_cost_Overall_average","Total_cost_Overall_average")
print(Inp2TCOA)

Ill1 <- !is.na(lps$HH_Illness_1_Total_cost_Overall_average) 
Ill1 <- Ill1 & lps$HH_Illness_1_Total_cost_Overall_average!=0
Ill1 <- Ill1 & lps$HH_Illness_1_Total_cost_Overall_average<500000
Ill1[c(96,238,883)]=F;
png("./output/Outpatient_Cost_vs_PCA Score")
plot(Score[Ill1],lps$HH_Illness_1_Total_cost_Overall_average[Ill1],xlab="PCA Score",ylab="Total Oupatient Cost",main="Outpatient Cost vs PCA Score")
dev.off()
sum(Ill1)
lps$HH_Illness_1_Total_cost_Overall_average[Ill1]
lps$HH_Illness_1_Total_cost_Overall_average[Ill1]


sapply(names(cgroups),function(N){
         G <- cgroups[[N]];
         png(paste("./output/",N,"Outpatient_Cost_vs_PCA_Score.png",sep="_"))
         plot(Score[Ill1& G],lps$HH_Illness_1_Total_cost_Overall_average[Ill1 & G],xlab="PCA Score",ylab="Total Oupatient Cost",main=paste(N,"Outpatient Cost vs PCA Score"))
         dev.off()
       })
             
