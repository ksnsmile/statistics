#6.4. 분산분석
#6.4.1. 일원분류분산분석

# Data : InsectSprays in Datasets
# Effectiveness of Insect Sprays

# InsectSprays의 변수들을 $사용없이 그냥 바로 사용 가능
attach(InsectSprays)

# 데이터 불러오기 
data(InsectSprays)
str(InsectSprays)
boxplot(count~spray, horizontal = TRUE)

## Not assuming equal variances 일원 분산분석
oneway.test(count~spray)
## Assuming equal variances 보통 등분산성이 있다고 가정 하고 검증을 실시한다.
oneway.test(count~spray, var.equal = TRUE)
## which gives the same result as
#회귀모델로 변환해서 계산후 분산분석석
anova(lm(count~spray))

#바로 분산분석으로 한 것 
AovOut = aov(count ~ spray, data=InsectSprays)
summary(AovOut)

# Post Hoc tests : Tukey HSD(Honestly Significant Difference) is default in R
TukeyHSD(AovOut, conf.level = 0.95) # 그룹간 통계적으로 차이가 유의미한지 보는 것 

# t 검정을 수행하여서 그룹간의 차이가 유의미한지 확인한다.
pairwise.t.test(count,spray, data=InsectSprays)

# Test assumptions
# a. Homogeneity of variance

# 등분산성을 검증 하기 위한 것 
bartlett.test(count ~ spray, data=InsectSprays)

# b. Model checking plots #6개의 진단 그래프 표현 aovout에 있는
Opar <- par(mfrow = c(2,3)) #2행 3열 그래픽 패널 설정
plot(AovOut, which=c(1:6))
par(Opar)

# ANOVA as Linear Regression Analysis
summary(InsectSprays)
#with은 데이터 프레임이나 리스트 안에 있는 열들을  바로 쓸 수 있도록 해준것 
with(InsectSprays, tapply(count, spray, mean)) #spray 그룹별로 count의 평균을 계산한다
with(InsectSprays, tapply(count, spray, var)) # spray 그룹별로 count의 분산을 계산한다.
with(InsectSprays, bartlett.test(count ~ spray)) #등분산성을 확인 
LmOut = with(InsectSprays, lm(count ~ spray)) # lm 모델 적합 시킨다.
summary(LmOut) # the default summary display will be the linear regression
summary.aov(LmOut) # lmout 모델을 분산분석 한 것을 요약한 것 


#6.4.2. 이원분류분산분석

# 반복측정이 없는 경우
product = c(42.8, 38.6, 50.2, 48.2, 52.3, 43.5, 
            58.7, 50.8, 48.2, 40.3, 53.5, 51.2 )
region = factor(rep(rep(1:4, c(1,1,1,1)),3))
fertile = factor(rep(1:3, c(4,4,4)))
anova(lm(product ~ region + fertile))
aov(product  ~ region + fertile)
summary(aov(product  ~ region + fertile))


# 반복측정이 있는 경우
sell = c( 23, 20, 21, 22, 19, 20, 19, 18, 21, 
	    22, 20, 19, 20, 21, 22, 20, 19, 22,
          18, 18, 16, 21, 23, 20, 20, 22, 24)
design = factor( rep( rep(1:3, c(3,3,3)), 3))
size = factor( rep( 1:3, c(9,9,9)))
anova( lm ( sell ~ size * design ))
summary(aov( sell ~ size * design ))



# 추가적인 예)  With Data : mtcars in  Datasets

# Two-way Interaction Plot 
#mtcars 내장 데이터셋 사용
attach(mtcars);  Gear <- factor(gear);  Cyl <- factor(cyl) # gear와 cyl을 범주형으로 만듬 facotr이용해서
opar <- par(mfrow = c(1,2))

# type: 선과점을 표시 , col은 기어 개수별 다른 색상 사용 , leg.by='o'는 범례의 테두리 표시, leg.bg='beige' 범례배경 색상상
interaction.plot(Cyl, Gear, mpg, type="b", col=c(1:3), 
    leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
    xlab="Number of Cylinders", 
    ylab="Mean Miles Per Gallon", 
    main="Interaction Plot 1")
interaction.plot(Gear, Cyl, mpg, type="b", col=c(1:3), 
    leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
    xlab="Number of Gears", 
    ylab="Mean Miles Per Gallon", 
    main="Interaction Plot 2")
par(opar) # 다른 플롯의 레이아웃이 영향 받지 않도록 복원

table(gear, cyl)

# Randomized Block Design (B is the blocking factor) 
summary(aov(mpg ~ Cyl + Gear, data=mtcars))


# Randomized Block Design (B is the blocking factor) 
summary(aov(mpg ~ Cyl*Gear, data=mtcars))

# Plot Means with Error Bars
#install.packages("gplots")
library(gplots)
attach(mtcars)
Cyl <- factor(cyl)
plotmeans(mpg~Cyl,xlab="Number of Cylinders",
   ylab="Miles Per Gallon", main="Mean Plot with 95% CI")

# Homogeneity of Variances
# Bartlett Test of Homogeneity of Variances
bartlett.test( mpg ~ Cyl, data=mtcars)
# Figner-Killeen Test of Homogeneity of Variances
fligner.test( mpg ~ Cyl, data=mtcars) 

# Homogeneity of Variance Plot
install.packages("HH") 
library(HH)
hov(mpg ~ Cyl, data=mtcars)
hovPlot(mpg ~ Cyl, data=mtcars)


