b<-1:sum(Geo_Poor_Indv)
a<-1:sum(HEF_PreID_Indv)

a[]=FALSE;
b[]=FALSE;

la = sum (Geo_Poor_Indv)
lb = sum (HEF_PreID_Indv)
pa <-.896
pb <-.919
a[1:(la*pa)]=TRUE;
b[1:(lb*pb)]=TRUE;

print (c(sum(a),la,sum(b),lb))


tt<-t.test(a,b)

chisq.test(a,b)
print(tt)

b<-1:sum(IndiHealth$Illness[Geo_Poor_Indv]==1)
a<-1:sum(IndiHealth$Illness[HEF_PreID_Indv]==1)

a[]=FALSE;
b[]=FALSE;

la = length(a)
lb = length(b)
pa <-.294
pb <-.185
a[1:(la*pa)]=TRUE;
b[1:(lb*pb)]=TRUE;

print (c(sum(a),la,sum(b),lb))
tt<-t.test(a,b)
print(tt)