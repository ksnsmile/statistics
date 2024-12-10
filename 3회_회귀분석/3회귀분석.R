#회귀분석
#단순선형회귀분석

# 나이와 최대 심장 박동수와의 관계
# Max = 220 - Age

Age <- c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37)
Max <- c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)
Data = data.frame( Age, Max )

Ma <- lm(Max ~ Age, Data)
Ma

names(Ma)

plot( Age, Max, main = "자료와 회귀직선" )
abline(coef(Ma))

summary(Ma)
anova(Ma)

deviance(Ma)       # sse 값 반환 또는 
sum( ( Max - predict(Ma))^2)

str(Ma)
str(summary(Ma))
str(anova(Ma))

anova(Ma)$Df
anova(Ma)$Sum
anova(Ma)$Mean
anova(Ma)$F
anova(Ma)$Pr

confint(Ma)        # 또는 confint(Ma, level = 0.95)
confint(Ma)        # 또는 confint(Ma, level = 0.95)
  
fitted.values(Ma)  # 또는 fitted(Ma)
residuals(Ma)

predict(Ma)
predict(Ma, interval="confidence",level = 0.95)
predict(Ma, interval="prediction",level = 0.95)
  
predict(Ma, data.frame(Age=20))
predict(Ma, data.frame(Age=20), interval="confidence") #모델이 예측한 평균값이 신뢰구간 내에 있을 확률
predict(Ma, data.frame(Age=20), interval="prediction") # 실제 y 값이 어느 구간에 있을 확률률


opar <- par(mfrow = c(2,3))
plot(Ma, which=c(1:6), las = 1)
par(opar)

Age <- seq(min(Age), max(Age),0.1)
Pc <- predict(Ma, data.frame(Age), interval="confidence")
Pp <- predict(Ma, data.frame(Age), interval="prediction")
matplot(Age, Pc, main="예측값의 신뢰구간", ylab="Max", type='l', lty=1, col="black")
matlines(Age, Pp, type='l', lty=2,col=c("black", "red", "red"))


#다중선형회귀분석
# 자료 mtcars 사용
install.packages("car")
library(car)
head(mtcars, 5L)
str(mtcars)
Fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
Fit

# 유용한 함수들 
coefficients(Fit) # model coefficients
confint(Fit, level=0.95) # CIs for model parameters 
fitted(Fit) # predicted values
residuals(Fit) # residuals
anova(Fit) # anova table 
vcov(Fit) # covariance matrix for model parameters 공분산 행렬
influence(Fit) # regression diagnostics 

# 이상치 판단에 유용한 함수들
outlierTest(Fit) # Bonferonni p-value for most extreme obs
qqPlot(Fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(Fit) # leverage plots 

# 영향력이 있는 관찰치 파악위한 함수들
avPlots(Fit)
# Cook's distance 그림 : 계산값 > 4/(n-k-1) 이면 영향력 있다고 판단
Cutoff <- 4/((nrow(mtcars)-length(Fit$coefficients)-2)) 
plot(Fit, which=4, cook.levels=Cutoff)
# 영향력 그림 
influencePlot(Fit, id.method="identify", main="영향력 그림", 
sub="원의 크기는 Cook's Distance에 비례" )


# 잔차의 정규성 파악
# 표준화 잔차의 QQ-plot
qqPlot(Fit, main="QQ Plot")

# 표준화 잔차의 분포
library(MASS)
Sresid <- studres(Fit) 
hist(Sresid, freq=FALSE,main="표준화 잔차의 분포", xlab="표준화 잔차")
xFit<-seq(min(Sresid),max(Sresid),length=40) 
yFit<-dnorm(xFit) 
lines(xFit, yFit) 

# 등분산성 검정
ncvTest(Fit)
# 표준화 잔차와 적합값의 그래프 
spreadLevelPlot(Fit)

# 다중공선성 계산
vif(Fit) 

# 오차의 자기상관 검정
durbinWatsonTest(Fit)

# Global test of model assumptions
#install.packages("gvlma")
library(gvlma)
Gvmodel <- gvlma(Fit) 
summary(Gvmodel) 


#다중선형회귀분석에서의 변수선택방법
# 단계적, 후진 및 전진 선택법
Fit <- lm(mpg~disp+hp+wt+drat, data=mtcars)
FitF <- lm(mpg ~ 1 , data=mtcars)

step(Fit, direction="backward")
step(Fit, direction="both")
step(FitF, direction="forward", scope=list(upper=Fit,lower=FitF))
step(FitF, direction="both", scope=list(upper=Fit,lower=FitF))


# 또는 
library(MASS)
Fit <- lm(mpg~disp+hp+wt+drat, data=mtcars)
FitF <- lm(mpg ~ 1 , data=mtcars)
stepAIC(Fit, direction="both")
stepAIC(FitF, direction="forward",scope=list(upper=Fit,lower=FitF))
stepAIC(Fit, direction="backward")


# 모든 가능한 회귀
#install.packages("leaps")
library(leaps)
FitA <- regsubsets(mpg ~ disp+hp+wt+drat, data=mtcars, nbest=2)
summary(FitA)

str(FitA)

plot(FitA,scale="r2")   
plot(FitA,scale="adjr2")

library(car)
subsets(FitA, statistic="rsq")