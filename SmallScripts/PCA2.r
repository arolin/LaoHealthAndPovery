Score2 <- apply(lps,1,function(lps){
        ## persons in Household	1	2 to 10	> 10			
        ## Score	0	1	0			
        Score <- list(NPeople=0,Sex=0,Lit1=0,Lit2=0,NCH=0,NAdult=0,House=0,Roof=0,TV=0,Vid=0,Bike=0,Moto=0,Tractor=0,Toilet=0,Energy=0,Cereals=0,Cash=0,Shop=0,NChic=0,NPig=0,NCattel=0)
        if(as.numeric(lps["HH_NumPeople"]) >= 2 & as.numeric(lps["HH_NumPeople"]) <= 10){
          Score$NPeople <- 1;
        }

        ## Sex HoH	Woman	Man				
        ## Sex of HOH	0	1
        if(as.numeric(lps["HoH_Sex"])==2) {
          Score$Sex  <- 1;
        }

        ## 1	Cannot write/read
        ## 2	Can read
        ## 3	Can read / write
        ## Litteracy level	Cannot write/read	Can read	Can read/write			
        ## HoH	0	1	2
        Score$Lit1 <-  as.numeric(lps["HoH_Literacy_Level"]) -1;
        ## Spouse of HoH	0	1	2			
        Score$Lit2 <-  as.numeric(lps["HoH_Spouse_Literacy_Level"]) -1;

        paste("q2_3_",1:30,sep="")
        ## Children 0 to 14 years	4 or more	0 to 3				
        ## Score	0	1				
        NU14 <- sum(as.numeric(lps[paste("q2_3_",1:30,sep="")]) <= 14,na.rm=T)
        if(NU14<4) {
          Score$NCH <- 1;
        }
        
        ## Adults 20 to 60 years	0 or 1	2 or more				
        ## Score	0	1				
        N20to60 <- sum(as.numeric(lps[paste("q2_3_",1:30,sep="")]) >= 20 & as.numeric(lps[paste("q2_3_",1:30,sep="")])<=60,na.rm=T)
        if(N20to60>=2) {
          Score$NAdult <- 1;
        }

        ## House walls (*)	Bamboo Leaves Grass	Wood	Brick-stone ciment	if mixed, record the highest		
        ## Score	0	2	3			
        if (!is.na(lps["HH_Housing_Material_Brick"]) |
            !is.na(lps["HH_Housing_Material_Concrete"])) {
          Score$House <- 3;
        } else if (!is.na(lps["HH_Housing_Material_Wood"])) {
          Score$House <- 2;
        }

        
        ## House roof (*)	Grass  Leaves	Bamboo Wood	Iron Fibro	if mixed, record the highest		
        ## Score	0	2	3
        if (!is.na(lps["HH_Roofing_Material_Corrugated_iron"]) |
            !is.na(lps["HH_Roofing_Material_Tiles"])) {
          Score$Roof <-3;
        } else if (!is.na(lps["HH_Roofing_Material_Wood"])) {
          Score$Roof <-2;
        }
        
        ## TV	No	Yes				
        ## Score	0	2				
        if (as.numeric(lps["HH_Posses_Television"]) > 0) {
          Score$TV <- 2;
        }
        
        ## VDO/VCD	No	Yes				
        ## Score	0	2				
        if(as.numeric(lps["HH_Posses_Video"]) > 0) {
          Score$Vid <- 2;
        } 
        ## Bicycle	No	Yes				
        ## Score	0	1				
        if(as.numeric(lps["HH_Posses_Bicycle"]) >0) {
          Score$Bike <-  1;
        }
        
        ## Motorbike	No	Yes				
        ## Score	0	3				
        if(as.numeric(lps["HH_Posses_Motorbike"]) > 0) {
          Score$Moto <- 3;
        }
        
        ## Tractor/motor boat	No	Yes				
        ## Score	0	3				
        if (as.numeric(lps["HH_Posses_Tractor"])  > 0 | as.numeric(lps["HH_Posses_Tractor"]) > 0) {
          Score$Tractor <-3;
        }
        
        ## HH_Latrine_type_Modern_toilet
        ## HH_Latrine_type_Normal_toilet
        ## HH_Latrine_type_Dry_toilet   
        ## HH_Latrine_type_Other        
        ## HH_Latrine_type_None         
        ## Toilets	No	Basic (dry)	Modern			
        ## Score	0	2	3			
        if (!is.na(lps["HH_Latrine_type_Modern_toilet"])) {
          Score$Toilet <- 3;
        } else if (!is.na(lps["HH_Latrine_type_Normal_toilet"]) |
                   !is.na(lps["HH_Latrine_type_Dry_toilet"])) {
          Score$Toilet <- 2;
        }
        
        ## Energy fo cooking	Wood/ sawdust	Charcoal	Gaz/electricity			
        ## Score	0	2	4			
        if(as.numeric(lps["HH_Energy_type"]) == 3) {
          Score$Energy <-  4;
        } else if (as.numeric(lps["HH_Energy_type"]) == 2) {
          Score$Energy <-  2;
        } 

        
        ## Plant Cereals (Bag 30kg)	"0-500 Kg
        ## (16 Bags)"	"501-1000Kg
        ## (17-33 bags)"	"1001-1500Kg
        ## (34-50 bags)"	">1,500Kg
        ## (>50Bags)"		
        ## Score	0	1	2	3		
        if (as.numeric(lps["9.6PlantCereals(Bag30kg)"]) == 5) {
          Score$Cereals <- 3;
        }else if (as.numeric(lps["9.6PlantCereals(Bag30kg)"]) == 4) {
          Score$Cereals <- 2;
        }else if (as.numeric(lps["9.6PlantCereals(Bag30kg)"]) == 3) {
          Score$Cereals <- 1;
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
          Score$Cash <- 5;
        } else if (as.numeric(lps["HH_Cash_income"]) > 3500000) {
          Score$Cash <- 4;
        } else if (as.numeric(lps["HH_Cash_income"]) > 2500000) {
          Score$Cash <- 3;
        } else if (as.numeric(lps["HH_Cash_income"]) > 1500000) {
          Score$Cash <- 2;
        } else if (as.numeric(lps["HH_Cash_income"]) >  500000) {
          Score$Cash <- 1;
        }
        
        ## Own a shop	No	Yes				
        ## Score	0	1				
        if (!is.na(lps["HH_Income_Secondary_Shop_restaurant"])) {
          Score$Shop  <- 1;
        }
        
        ## Chick/duck/….Nber
        ## 0 to 10
        ## 11 to 20
        ## 21 to 30
        ## 31 to 40
        ## 41 and more	
        ## Score	0	1	2	3	4	
        if (as.numeric(lps["HH_Livestock_N_Chickens"]) >40) {
          Score$NChic  <-  4;
        } else if (as.numeric(lps["HH_Livestock_N_Chickens"]) >30) {
          Score$NChic  <-  3;
        } else if (as.numeric(lps["HH_Livestock_N_Chickens"]) >20) {
          Score$NChic  <-  2;
        } else if (as.numeric(lps["HH_Livestock_N_Chickens"]) >10) {
          Score$NChic  <-  1;
        }

        ## Pig/Goat          Nber	0
        ## 1 or 2
        ## 3 or 4
        ## 5 or 6
        ## 7 and more	
        ## Score	0	1	2	3	4	
        if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 6 ) {
          Score$NPig <-  4;
        } else if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 3 ) {
          Score$NPig <-  3;
        } else if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 1 ) {
          Score$NPig <-  2;
        } else if (as.numeric(lps["HH_Livestock_N_Pigs"]) > 0 ) {
          Score$NPig <-  1;
        }
        
        ## Buff/cow/horse   Nber	0
        ## 1
        ## 2 or 3
        ## 4 or more		
        ## Score	0	1	3	5		
        if ( (as.numeric(lps["HH_Livestock_N_Cattle"]) + as.numeric(lps["HH_Livestock_N_Buffalo"])) > 3) {
          Score$NCattel <-  5;
        } else if ( (as.numeric(lps["HH_Livestock_N_Cattle"]) + as.numeric(lps["HH_Livestock_N_Buffalo"])) > 1) {
          Score$NCattel <-  3;
        } else if ( (as.numeric(lps["HH_Livestock_N_Cattle"]) + as.numeric(lps["HH_Livestock_N_Buffalo"])) > 0) {
          Score$NCattel <-  1;
        }
        return (Score)
      }
      )

rebuildScore=T
if (rebuildScore) {
  ScoreTab <- data.frame(matrix(unlist(Score2), nrow=930, byrow=T),stringsAsFactors=FALSE)
  colnames(ScoreTab) <- names(Score2[[1]])
  ScoreMelt<-melt(Score2, measure.vars = 1:2)
  save(ScoreTab,file="ScoreTab.RData");
}

if(length(grep("Score.",names(lps)))==0) {
  lps <- cbind(lps,Score=rowSums(ScoreTab))
  lps <- cbind(lps,Score=ScoreTab)
  names(lps)[grep("Score.",names(lps))] <- paste("Score.",names(Score2[[1]]),sep="")
}else {
  lps$Score  <- rowSums(ScoreTab)
  lps[,grep("Score.",names(lps))] <- ScoreTab;
}
names(lps)[grep("Score",names(lps))]


p <-ggplot(lps,aes(Score))
p <- p + geom_histogram()
p <- p + facet_grid(. ~ Group)
p

head(lps)


png("./output/PCABreakdown.png")

x11()
ScoreMeans <- sapply(lgroups,function(G){apply(Score3[G,],2,mean)})
ScoreMedians <- sapply(lgroups,function(G){apply(Score3[G,],2,median)})
sord <- order(apply(ScoreMeans,1,function(X){s <- summary(X);s[6]-s[1];}),decreasing=T)
sord <- order(ScoreMeans[,4],decreasing=T)
b <- boxplot(Score3[,sord],las=2,main="PCA Score Breakdown")
pars <- data.frame()
pars[1,"col"] <- rgb(1,0,0)
pars[2,"col"] <- rgb(0,1,0)
pars[3,"col"] <- rgb(0,0,1)
pars[4,"col"] <- rgb(0,0,0)
legend(15.5,5.15,legend=paste(c("PreID","GeoID","No Assist","All"),"Mean"),pch=12,col=pars[,"col"])
for(i in 1:4) {
  points(apply(Score3[lgroups[[i]],sord],2,mean),pch=12,col=pars$col[i])
}

dev.off()






