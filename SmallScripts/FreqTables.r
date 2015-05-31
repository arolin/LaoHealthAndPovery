Q1 <- names(lps)[which(names(lpsraw)=="q1_3"):which(names(lpsraw)=="q1_9_2")]
qCheck2(Q1,1)

Q3 <- names(lps)[which(names(lpsraw)=="q3_1"):which(names(lpsraw)=="q3_15")]
qCheck2(Q3,3)

Q4 <- names(lps)[which(names(lpsraw)=="q4_2"):which(names(lpsraw)=="q4_45_other")]
qCheck2(Q4,4)

Q5 <- names(lps)[which(names(lpsraw)=="q5_1_1"):which(names(lpsraw)=="q5_28")]
qCheck2(Q5,5)

Q6 <- names(lps)[which(names(lpsraw)=="q6_1_1"):which(names(lpsraw)=="q6_7_5")]
qCheck2(Q6,6)

Q7 <- names(lps)[which(names(lpsraw)=="q7_1_a"):which(names(lpsraw)=="q7_21_reasonENG")]
qCheck2(Q7,7)

Q8 <- names(lps)[which(names(lpsraw)=="q8_1_1"):which(names(lpsraw)=="q8_21_reasonENG")]
qCheck2(Q8,8)

Q9 <- names(lps)[which(names(lpsraw)=="q9_1_1"):which(names(lpsraw)=="q9_15_other")]
qCheck2(Q9,"9_Characteristic_of_House_Hold")
