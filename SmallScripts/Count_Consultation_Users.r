
careCentersOfInterest <-careCenters;

careCentersOfInterest <-NationalCenters
careTimes<-list();
i<-1;
for (g in lgroups){
  gs<-0;
  c<-1:length(g);
  f[c]<-FALSE;
  n<-0;
  for (p in prefixes) {
    f<-1:length(g);
    f[]<-FALSE;
    for (cc in careCentersOfInterest ) {
      flag<-paste(p,"Consult_",cc,sep="")
      f[g] <- f[g] | !is.na(lps[g,flag]);
    }
    n<-n+sum(f[g])
  }
  print(n)
  careTimes[[i]]<-g;
  i<-1+i;
  #print(gs);
}

