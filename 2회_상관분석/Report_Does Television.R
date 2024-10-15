# 작업 디렉토리 변경(각자의 환경에 따라 다르므로 유의)
Data <- read.csv('Report_Does Television.csv')
Data


# 평균과 분산 구하기
mean(Data$TVHOURS)
var(Data)

pairs(Data, main = "자료명 : Does Television....")  

cor(Data)
cor(Data, method = "spearman")
cor(Data, method = "kendall")

cor.test(Data$TVHOURS,Data$OBEDIENC, method = "pearson", alternative = "g")
cor.test(Data$TVHOURS,Data$OBEDIENC, method = "pearson", alternative = "two.sided")
cor.test(Data$TVHOURS,Data$OBEDIENC, method = "pearson", alternative = "l")

cor.test(Data$TVHOURS,Data$ATTITUDE, method = "pearson", alternative = "two.sided")