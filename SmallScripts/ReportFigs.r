source("LoadData.R")


png("./Analysis/Figures/HHs sampled by village.png",width=640,height=480)
lps$MNCH <- !is.na(lps$Mother_ID)
p <- ggplot(data=lps)
p <- p + geom_bar(aes(x=factor(Village),fill=Group,color=MNCH),binwidth=1,size=.75)
p <- p + scale_color_manual("MNCH:",values=c("Black","Pink"),labels=c("Non MNCH","MNCH"))
p <- p + labs(title="Survey Respondents by Village",x="Village Number",y="Number of HHs")
p <- p + theme(axis.text.x = element_text(angle = 90,vjust=.5))
print(p )
dev.off()


x11()

png("./Analysis/Figures/Total HHs by village.png",width=640,height=480)
p <- ggplot(data=Villages)
p <- p + geom_bar(aes(fill=Pre.ID==2,x=factor(VillageNum),y=X..HH.Total),stat="identity")
p <- p + theme(axis.text.x = element_text(angle = 90,vjust=.5))
p <- p + scale_fill_manual("Region",values=c("#f8766d","#00ba38"),labels=c("PreID","GeoID"))
p <- p + labs(x="Village Num",y="Number of HHs",title="Census Counts on HHs in Surveyed Villages")
print(p)


Villages$HEF.Mbers <- as.numeric(sub(",","",as.character(Villages$HEF.Mbers)))

VCount <- as.data.frame(table(subset(lps,Group!="NoAssit")$Village,dnn=c("Village")))

VCount <- as.data.frame(table(subset(lps,Group=="PreID")$Village,dnn=c("Village")))
VCount$Freq/sapply(VCount$Village,function(V){subset(Villages,as.character(VillageNum)==as.character(V))$HEF.Mbers})

VCount <- as.data.frame(table(subset(lps,Group!="GeoID")$Village,dnn=c("Village")))
VCount$Freq/sapply(VCount$Village,function(V){subset(Villages,as.character(VillageNum)==as.character(V))$X..HH.Total})

tmp <- with(subset(lps,Group!="GeoID"),table(Village,Group,dnn=c("Village","Group")))
tmp[,3]/tmp[,1]


VCount$Freq/sapply(VCount$Village,function(V){subset(Villages,as.character(VillageNum)==as.character(V))$HEF.Mbers})
sapply
