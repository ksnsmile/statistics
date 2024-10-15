# 작업 디렉토리 변경(각자의 환경에 따라 다르므로 유의)
Data <- read.csv('1주차_3차시_R 기술통계.csv')
Data
str(Data)

# 평균과 분산 구하기
Mean_A <- mean(Data$RegionA)
Var_A <- var(Data$RegionA)

Mean_A
Var_A

min(Data$RegionA)
max(Data$RegionA)

Bottom= round(min(Data$RegionA), digits= 0)
Bottom

Top = round(max(Data$RegionA), digits= 0)
Top


# 도수분포표 작성
table(Data$RegionA)    # ???????


Boundaries <- seq(Bottom-5, Top+5, 5)
Boundaries

table(cut(Data$RegionA, Boundaries))


# 상대도수분포표 작성
Length <- length(Data$RegionA)
Length

table(cut(Data$RegionA, Boundaries)) / Length


# 히스토그램 작성
hist(Data$RegionA)

hist(Data$RegionA, prob= TRUE)

hist(Data$RegionA, breaks= Boundaries, prob= T, 
     main= "상대도수 히스토그램", ylab= "상대도수", xlab= "지역")


# 상자그림 작성
boxplot(Data$RegionA)

boxplot(Data)