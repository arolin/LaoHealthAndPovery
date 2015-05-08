###############################
#Data is in upto 30 subjects per row
#split respondents into sub rows

#Start index of each question
SubQIndexes<-c(35,65,95,125,185,215,785,815,845,875,905,935,965,995,1025,1055)
QNames = c("SRow","Name","Gender","Age","Marital_Status","Marital_Status_Other","Illness","NumIllnesses","Overnight Health_Centre","Nights Health_Centre","Overnight District_Hospital","Nights District_Hospital","Overnight Provincial_Hospital","Nights Porovincial_Hospital","Overnight National Hospital","Nights National_Hospital","Overnight Private_Clinic","Nights Private_Clinic","Care At- No care","Care At- Home-made medicine","Care At- Village modern medical practitioner","Care At- Village health volunteer","Care At- Traditional healer","Care At- Health centre","Care At- District hospital","Care At- Provincial hospital/Regional","Care At","Care At - National hospital","Care At - Private pharmacy","Care At - Private clinic","Care At - Abroad","Care At - Illegal medical practitioner","Care At - Other","Care At - Do not remember","Care At - Do not know","Care At - Other specify","Care At - Other Code")
QNames = c("SRow","Name","Gender","Age","Marital_Status","Marital_Status_Other","Illness","NumIllnesses","Overnight Health_Centre","Nights Health_Centre","Overnight District_Hospital","Nights District_Hospital","Overnight Provincial_Hospital","Nights Porovincial_Hospital","Overnight National Hospital","Nights National_Hospital","Overnight Private_Clinic","Nights Private_Clinic","Care At- No care","Care At- Home-made medicine","Care At- Village modern medical practitioner","Care At- Village health volunteer","Care At- Traditional healer","Care At- Health centre","Care At- District hospital","Care At- Provincial hospital/Regional","Care At","Care At - National hospital","Care At - Private pharmacy","Care At - Private clinic","Care At - Abroad","Care At - Illegal medical practitioner","Care At - Other","Care At - Do not remember","Care At - Do not know","Care At - Military Hospital","Care At - Religious Healer","Group")

#Length of the answer to each question
SubQLen<-SubQIndexes;
SubQLen[]=1;
SubQLen[4]=2;
#SubQLen[7]=18; - This has to be handled seperately as it is some times 18 and sometimes 19

maxRow=length(lps[,1]); #Turn on the next line to facilitate debug
#maxRow=2

#count the number of individuals by running through them all once
indi<-0;
#for each survey row
for (S in 1:maxRow) {
  #xtract the full surver row
  row <-lps[S,]; #enable the next line to use the raw placeholder names
  #row <-names(lps);  

   #FOR each of 30 possible dependents
   for (N in 0:29) {
     #check if name is not NA
     v<-row[SubQIndexes[1]+N]!="";
     if (!is.na(v) && v) {
       #increment count of individuals 
       indi=indi+1;
     }
   }
 }

 print(indi);

 #initialize a matrix of the appropriate size and make it a frame with column names
 IndiHealth=matrix(nrow=indi,ncol=38);
 IndiHealth<-data.frame(IndiHealth);
 names(IndiHealth)<- QNames;


 r=0;#target row
 #For every
 for (S in 1:maxRow ) {

  row <-lps[S,];
  ##row <-names(lps)


  #FOR each possible dependedn
  for (N in 0:29) {
    v<-row[SubQIndexes[1]+N]!="";
    if (!is.na(v) && v) {
      #if there is a dependent...
      r=r+1; #increment the row count in the target table
      
      #initialize depedent's data with a parent row number
      cdata<-c(row["Serial"]);
      
      name<-row[35+N];
      cdata<-c(cdata,         "") #Name is not working
      
      #For each repeated question
      for (Q in 2:(length(SubQIndexes))) {
        
        #append each answer
        QIdx <-SubQIndexes[Q];
        QLen <-SubQLen[Q];
        AnsIdx <- (1:QLen)+QIdx+ -1;
        cdata<-c(cdata,row[N*QLen+AnsIdx]);
      }  

      start = 245 +18*(N);
      idxs=0:16;
                
      if(is.na(row[start+17])) {          
        careAt <-c(row[start+idxs],NA,NA)
      } else if(row[start+17] ==19) {
        careAt <-c(row[start+idxs],NA,19)
      } else if (row[start+17]==18) {
        careAt <-c(row[start+idxs],18,NA)
      } else {          
        careAt <-c(row[start+idxs],NA,NA)    
      }
        
      cdata<-c(cdata,careAt,row["Group"]);
#     print(cdata);
      #copy the data into the target frame
      IndiHealth[r,]<-cdata;
   
    }#if dependent
  }#All 30 dependents
}#All Rows



IndiHealth$Gender=factor(IndiHealth$Gender,labels=c("F","M"));
IndiHealth$Marital_Status=factor(IndiHealth$Marital_Status,levels=c("1","2","3","4","5"),labels=c("Single","Married","Divorced/separated","Widowed","Other"));
save(IndiHealth,file="IndiHealth.bin");
