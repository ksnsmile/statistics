# 작업 디렉토리 변경(각자의 환경에 따라 다르므로 유의)
Data <- read.csv('Report_Driving in.csv')
Data


# 평균과 분산 구하기
mean(Data$AGE)
mean(Data$CHO2DRI)
mean(Data$PUBTRAN)
mean(Data$ACCIDENT)


pairs(Data, main = "자료명 : Driving in....")  


boxplot(Data$CHO2DRI ~ Data$GENDER)


cor(Data)
cor(Data, method = "spearman")
cor(Data, method = "kendall")

cor.test(Data$AGE,Data$PUBTRAN, method = "pearson", alternative = "g")
cor.test(Data$AGE,Data$PUBTRAN, method = "pearson", alternative = "two.sided")
cor.test(Data$AGE,Data$PUBTRAN, method = "pearson", alternative = "l")

cor.test(Data$AGE,Data$ACCIDENT, method = "pearson", alternative = "two.sided")
cor.test(Data$PUBTRAN,Data$ACCIDENT, method = "pearson", alternative = "two.sided")