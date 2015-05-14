---
title: "PrintTables"
author: "Leah Hale"
date: "02/05/2015"
output: html_document
---

#####
#analys of cost breakdown


```r
library("xtable")
source ("LoadData.R")
source ("AnalyzeGroups.R")
```

First question...how do breakdowns compare to the total


```r
si= which (names(lps)=="HH_Illness_1_National_Hospital_cost_Medicine")
ei= which (names(lps)=="HH_Illness_1_National_Hospital_cost_Others")
sumCosts<-rowSums (lps[,si:ei],na.rm = TRUE);
diffCosts <- sumCosts - lps$HH_Illness_1_National_Hospital_cost_Overall_average;
 sum (sumCosts == lps$HH_Illness_1_National_Hospital_cost_Overall_average,na.rm = TRUE)
```

```
## [1] 4
```

```r
hist(diffCosts)
```

![plot of chunk reasis](figure/reasis-1.png) 

```r
sum(!is.na(lps$HH_Illness_1_Consult_National_Hospital))
```

```
## [1] 9
```


```r
print(xtable(payTable),"html")
```

<!-- html table generated in R 3.2.0 by xtable 1.7-4 package -->
<!-- Tue May  5 06:59:43 2015 -->
<table border=1>
<tr> <th>  </th> <th> Preid_HEF </th> <th> GEO_Poor </th> <th> NoAssist </th> <th> All Respondants </th>  </tr>
  <tr> <td align="right"> National_Hospital_cost_Overall_average </td> <td> 3000 / 3000 [ 2 ] </td> <td>  </td> <td> 3632 / 1800 [ 5 ] </td> <td> 3451 / 1800 [ 7 ] </td> </tr>
  <tr> <td align="right"> Provincial_Hospital_cost_Overall_average </td> <td> 683 / 30 [ 101 ] </td> <td> 1235 / 400 [ 23 ] </td> <td> 1422 / 500 [ 133 ] </td> <td> 1115 / 280 [ 257 ] </td> </tr>
  <tr> <td align="right"> District_Hospital_cost_Overall_average </td> <td> 50 / 30 [ 77 ] </td> <td> 182 / 88 [ 160 ] </td> <td> 296 / 120 [ 79 ] </td> <td> 178 / 62 [ 316 ] </td> </tr>
  <tr> <td align="right"> Health_Center_cost_Overall_average </td> <td> 49 / 10 [ 89 ] </td> <td> 93 / 40 [ 36 ] </td> <td> 105 / 50 [ 121 ] </td> <td> 83 / 30 [ 246 ] </td> </tr>
  <tr> <td align="right"> Health_Volunteer_cost_Overall_average </td> <td> 126 / 20 [ 13 ] </td> <td> 17 / 12 [ 17 ] </td> <td> 88 / 18 [ 6 ] </td> <td> 68 / 15 [ 36 ] </td> </tr>
  <tr> <td align="right"> Traditional_Healer_cost_Overall_average </td> <td>  </td> <td> 1450 / 70 [ 3 ] </td> <td> 30 / 30 [ 2 ] </td> <td> 882 / 40 [ 5 ] </td> </tr>
  <tr> <td align="right"> Private_Clinic_cost_Overall_average </td> <td> 252 / 190 [ 8 ] </td> <td> 378 / 330 [ 9 ] </td> <td> 393 / 168 [ 42 ] </td> <td> 371 / 190 [ 59 ] </td> </tr>
  <tr> <td align="right"> Private_Pharmacist_cost_Overall_average </td> <td> 79 / 55 [ 35 ] </td> <td> 145 / 100 [ 25 ] </td> <td> 91 / 50 [ 67 ] </td> <td> 98 / 60 [ 127 ] </td> </tr>
  <tr> <td align="right"> Religious_Healer_cost_Overall_average </td> <td> 12000 / 12000 [ 1 ] </td> <td> 10767 / 16000 [ 3 ] </td> <td> 200 / 200 [ 3 ] </td> <td> 6414 / 300 [ 7 ] </td> </tr>
   </table>

```r
print(xtable(payTableSPSS),"html")
```

<!-- html table generated in R 3.2.0 by xtable 1.7-4 package -->
<!-- Tue May  5 06:59:43 2015 -->
<table border=1>
<tr> <th>  </th> <th> Preid total </th> <th> Preid_mean </th> <th> Preid_median </th> <th> Preid_num </th> <th> GEO total </th> <th> GEO_mean </th> <th> GEO_median </th> <th> GEO_num </th> <th> No total </th> <th> No_mean </th> <th> No_median </th> <th> No_num </th> <th> All total </th> <th> All_mean </th> <th> All_median </th> <th> All_num </th>  </tr>
  <tr> <td align="right"> National_Hospital_cost_Overall_average </td> <td align="right"> 6000000.00 </td> <td align="right"> 3000000.00 </td> <td align="right"> 3000000.00 </td> <td align="right"> 2.00 </td> <td align="right"> 0.00 </td> <td align="right">  </td> <td align="right">  </td> <td align="right"> 0.00 </td> <td align="right"> 18160000.00 </td> <td align="right"> 3632000.00 </td> <td align="right"> 1800000.00 </td> <td align="right"> 5.00 </td> <td align="right"> 24160000.00 </td> <td align="right"> 3451428.57 </td> <td align="right"> 1800000.00 </td> <td align="right"> 7.00 </td> </tr>
  <tr> <td align="right"> Provincial_Hospital_cost_Overall_average </td> <td align="right"> 68990000.00 </td> <td align="right"> 683069.31 </td> <td align="right"> 30000.00 </td> <td align="right"> 101.00 </td> <td align="right"> 28416000.00 </td> <td align="right"> 1235478.26 </td> <td align="right"> 400000.00 </td> <td align="right"> 23.00 </td> <td align="right"> 189065000.00 </td> <td align="right"> 1421541.35 </td> <td align="right"> 500000.00 </td> <td align="right"> 133.00 </td> <td align="right"> 286471000.00 </td> <td align="right"> 1114673.15 </td> <td align="right"> 280000.00 </td> <td align="right"> 257.00 </td> </tr>
  <tr> <td align="right"> District_Hospital_cost_Overall_average </td> <td align="right"> 3887000.00 </td> <td align="right"> 50480.52 </td> <td align="right"> 30000.00 </td> <td align="right"> 77.00 </td> <td align="right"> 29040000.00 </td> <td align="right"> 181500.00 </td> <td align="right"> 87500.00 </td> <td align="right"> 160.00 </td> <td align="right"> 23420000.00 </td> <td align="right"> 296455.70 </td> <td align="right"> 120000.00 </td> <td align="right"> 79.00 </td> <td align="right"> 56347000.00 </td> <td align="right"> 178313.29 </td> <td align="right"> 62500.00 </td> <td align="right"> 316.00 </td> </tr>
  <tr> <td align="right"> Health_Center_cost_Overall_average </td> <td align="right"> 4403000.00 </td> <td align="right"> 49471.91 </td> <td align="right"> 10000.00 </td> <td align="right"> 89.00 </td> <td align="right"> 3360000.00 </td> <td align="right"> 93333.33 </td> <td align="right"> 40000.00 </td> <td align="right"> 36.00 </td> <td align="right"> 12647000.00 </td> <td align="right"> 104520.66 </td> <td align="right"> 50000.00 </td> <td align="right"> 121.00 </td> <td align="right"> 20410000.00 </td> <td align="right"> 82967.48 </td> <td align="right"> 30000.00 </td> <td align="right"> 246.00 </td> </tr>
  <tr> <td align="right"> Health_Volunteer_cost_Overall_average </td> <td align="right"> 1634000.00 </td> <td align="right"> 125692.31 </td> <td align="right"> 20000.00 </td> <td align="right"> 13.00 </td> <td align="right"> 283000.00 </td> <td align="right"> 16647.06 </td> <td align="right"> 12000.00 </td> <td align="right"> 17.00 </td> <td align="right"> 530000.00 </td> <td align="right"> 88333.33 </td> <td align="right"> 17500.00 </td> <td align="right"> 6.00 </td> <td align="right"> 2447000.00 </td> <td align="right"> 67972.22 </td> <td align="right"> 15000.00 </td> <td align="right"> 36.00 </td> </tr>
  <tr> <td align="right"> Traditional_Healer_cost_Overall_average </td> <td align="right"> 0.00 </td> <td align="right">  </td> <td align="right">  </td> <td align="right"> 0.00 </td> <td align="right"> 4350000.00 </td> <td align="right"> 1450000.00 </td> <td align="right"> 70000.00 </td> <td align="right"> 3.00 </td> <td align="right"> 60000.00 </td> <td align="right"> 30000.00 </td> <td align="right"> 30000.00 </td> <td align="right"> 2.00 </td> <td align="right"> 4410000.00 </td> <td align="right"> 882000.00 </td> <td align="right"> 40000.00 </td> <td align="right"> 5.00 </td> </tr>
  <tr> <td align="right"> Private_Clinic_cost_Overall_average </td> <td align="right"> 2012000.00 </td> <td align="right"> 251500.00 </td> <td align="right"> 190000.00 </td> <td align="right"> 8.00 </td> <td align="right"> 3400000.00 </td> <td align="right"> 377777.78 </td> <td align="right"> 330000.00 </td> <td align="right"> 9.00 </td> <td align="right"> 16490000.00 </td> <td align="right"> 392619.05 </td> <td align="right"> 167500.00 </td> <td align="right"> 42.00 </td> <td align="right"> 21902000.00 </td> <td align="right"> 371220.34 </td> <td align="right"> 190000.00 </td> <td align="right"> 59.00 </td> </tr>
  <tr> <td align="right"> Private_Pharmacist_cost_Overall_average </td> <td align="right"> 2763000.00 </td> <td align="right"> 78942.86 </td> <td align="right"> 55000.00 </td> <td align="right"> 35.00 </td> <td align="right"> 3618000.00 </td> <td align="right"> 144720.00 </td> <td align="right"> 100000.00 </td> <td align="right"> 25.00 </td> <td align="right"> 6122000.00 </td> <td align="right"> 91373.13 </td> <td align="right"> 50000.00 </td> <td align="right"> 67.00 </td> <td align="right"> 12503000.00 </td> <td align="right"> 98448.82 </td> <td align="right"> 60000.00 </td> <td align="right"> 127.00 </td> </tr>
  <tr> <td align="right"> Religious_Healer_cost_Overall_average </td> <td align="right"> 12000000.00 </td> <td align="right"> 12000000.00 </td> <td align="right"> 12000000.00 </td> <td align="right"> 1.00 </td> <td align="right"> 32300000.00 </td> <td align="right"> 10766666.67 </td> <td align="right"> 16000000.00 </td> <td align="right"> 3.00 </td> <td align="right"> 600000.00 </td> <td align="right"> 200000.00 </td> <td align="right"> 200000.00 </td> <td align="right"> 3.00 </td> <td align="right"> 44900000.00 </td> <td align="right"> 6414285.71 </td> <td align="right"> 300000.00 </td> <td align="right"> 7.00 </td> </tr>
   </table>
sum(lps)
