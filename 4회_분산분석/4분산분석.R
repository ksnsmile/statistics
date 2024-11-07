#6.4. 분산분석
#6.4.1. 일원분류분산분석

# Data : InsectSprays in Datasets
# Effectiveness of Insect Sprays

attach(InsectSprays)
data(InsectSprays)
str(InsectSprays)
boxplot(count~spray, horizontal = TRUE)

## Not assuming equal variances
oneway.test(count~spray)
## Assuming equal variances
oneway.test(count~spray, var.equal = TRUE)
## which gives the same result as
anova(lm(count~spray))

AovOut = aov(count ~ spray, data=InsectSprays)
summary(AovOut)

# Post Hoc tests : Tukey HSD(Honestly Significant Difference) is default in R
TukeyHSD(AovOut, conf.level = 0.95)

pairwise.t.test(count,spray, data=InsectSprays)

# Test assumptions
# a. Homogeneity of variance
bartlett.test(count ~ spray, data=InsectSprays)

# b. Model checking plots
Opar <- par(mfrow = c(2,3))
plot(AovOut, which=c(1:6))
par(Opar)

# ANOVA as Linear Regression Analysis
summary(InsectSprays)
with(InsectSprays, tapply(count, spray, mean))
with(InsectSprays, tapply(count, spray, var))
with(InsectSprays, bartlett.test(count ~ spray))
LmOut = with(InsectSprays, lm(count ~ spray))
summary(LmOut) # the default summary display will be the linear regression
summary.aov(LmOut) # we can ask for the corresponding ANOVA table


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

attach(mtcars);  Gear <- factor(gear);  Cyl <- factor(cyl)
opar <- par(mfrow = c(1,2))
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
par(opar)

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
#install.packages("HH") 
library(HH)
hov(mpg ~ Cyl, data=mtcars)
hovPlot(mpg ~ Cyl, data=mtcars)


