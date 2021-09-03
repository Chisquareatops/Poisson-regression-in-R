cellsData <- read.csv("C:/Program Files/R/R-3.5.1/cells.csv", header=TRUE, sep=",") 
cellsData

#Use linear regression to estimate the effect of dose on post-treatment cell count, 
#with adjustment for sex, age, and pre-treatment cell count

#summary(lm(fev ~ smoke + age, data=f))$coef #syntax from slides for example

summary(lm(count1 ~ dose + sex + count0 + age, data=cellsData))$coef

#JUST TO COMPARE: here it is with no adjustment
summary(lm(count1 ~ dose, data=cellsData))$coef
#looks like the effect just got SLIGHTLY smaller/less sig? but the coefficient is actually bigger...

#TEST NULL HYPOTHESIS FOR THIS MODEL using t-statistic
anova(lm(len ~ supp*dose, data=ToothGrowth))
anova(lm(len ~ supp, data=ToothGrowth))

anova(lm(count1 ~ dose+age+sex+count0, data=cellsData))
anova(lm(count1 ~ dose, data=cellsData))

F = ((3420724 - 2921292) / (38 - 35)) / (2921292 / 35)
p = 1-pf(F,2,35)
p

#tried the asme basic thing but with full anova using * not +
#F2 = ((3420724 - 1772830) / (38 - 24)) / (2921292 / 24)
F2

#trying with multiplication:
anova(lm(count1 ~ dose*age*sex*count0, data=cellsData))
anova(lm(count1 ~ dose, data=cellsData))

F = ((3420724 - 1772830) / (38 - 24)) / (1772830 / 24)
p = 1-pf(F,2,24)
p

#based on above, CANNOT reject null hypothesis

#probably better way, but same results... not sig
full=lm(count1 ~ dose+age+sex+count0, data=cellsData)
reduced=lm(count1 ~ dose, data=cellsData)
anova(reduced,full)

#CHECKING ASUMPTIONSA OF THE ABOVE TEST
plot(count1 ~ dose, data=cellsData)

fit=lm(count1 ~ dose, data=cellsData)
cellsData$resid = fit$residuals
plot(resid ~ dose, data=cellsData)
#plot of residuals shows strong evidence of non-constant variance

hist(fit$residuals,main="")
#hist does not look especially normal
#normality may mater in light of small sample size