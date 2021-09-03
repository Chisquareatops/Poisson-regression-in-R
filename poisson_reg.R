#QUESTION 1: do a Poisson regression on cell count and adjust for
#sex, age, and pre-treatment cell count

cellsData <- read.csv("~/cells.csv", header=TRUE, sep=",") 
cellsData

#run a poisson and include other vars
summary(m1 <- glm(count1 ~ dose + sex + age + count0, family="poisson", data = cellsData))
#to compare, linear model from last week
summary(lm(count1 ~ dose + sex + count0 + age, data=cellsData))$coef

#try to figure out if my Y could be poisson distributed
hist(cellsData$count1)
#mean and variance are equal in a poisson dist
mean(cellsData$count1)
var(cellsData$count1)

dose0 <- subset(cellsData$count1, cellsData$dose == 0)
dose10 <- subset(cellsData$count1, cellsData$dose == 10)
dose100 <- subset(cellsData$count1, cellsData$dose == 100)

mean(dose0)
var(dose0)

mean(dose10)
var(dose10)

mean(dose100)
var(dose100)

hiv.glm =glm(count1 ~ dose + sex + age + count0, data=cellsData, family=poisson(link="log"))
pchisq(hiv.glm$deviance, hiv.glm$df.residual, lower.tail=F)


test <- mean(cellsData$count1)

summary(m1 <- glm(count1 ~ as.factor(dose) + sex + age + count0, family="poisson", data = cellsData))

#USING TEETH DATA
toothData <- read.csv("~/Teeth.csv", header=TRUE, sep=",") 
toothData

model <- glm((EXTR > 0) ~ PDALL + AGE + GENDER, family=binomial(link='logit'), data=toothData)
summary(model)

model <- glm((EXTR > 0) ~ PDALL + AGE + GENDER, family=poisson(link="log"), data=toothData)
summary(model)