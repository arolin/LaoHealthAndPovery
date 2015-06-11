print(aggregate(. ~ Group, data=lps[,c("Group",PossVarName)], FUN=mean),digits=2)

print(aggregate(. ~ Group,data=lps,c("Group","WealthScore")], FUN=mean))

WealthByGroup<-aggregate(. ~ Group, data=lps[,c("Group",PossVarName)], FUN=function(X){mean(X>0)*100})
WealthAll<-colSums(lps[,PossVarName]>0)/length(lps[,1])*100
WealthTable<-rbind(WealthByGroup,WealthAll)
rownames(WealthTable)<-c(levels(lps$Group),"All")
print(WealthTable,digits=2)
colnames(T)
