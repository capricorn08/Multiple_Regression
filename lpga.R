par(ask=TRUE)
opar <- par(no.readonly=TRUE)

# Listing 8.4 - Multiple linear regression
pgadata <- read.csv("pgascore2004.csv", header = TRUE)
dim(pgadata)
str(pgadata)

fit <- lm(tmoney ~ age +Avrputt + drive + driveacc + greenReg + saveacc + events, data=pgadata)
summary(fit)

# Listing 8.5 - Mutiple linear regression with a significant interaction term
fit <- lm(tmoney ~ Avrputt + driveacc + greenReg + saveacc , data = pgadata)
summary(fit)

# simple regression diagnostics
fit <- lm(tmoney ~ age +Avrputt + drive + driveacc + greenReg + saveacc + events, data=pgadata)
par(mfrow=c(2,2))
plot(fit)
newfit <- lm(tmoney ~ Avrputt + driveacc + saveacc + greenReg, data= pgadata)
par(opar)

# Listing 8.14 - All subsets regression
#install.packages("leaps")
library(leaps)

leaps <-regsubsets(tmoney ~ Avrputt + driveacc + saveacc + greenReg, data= pgadata,nbest=4)
leaps <-regsubsets(tmoney ~ Avrputt + driveacc + greenReg, data= pgadata,nbest=4)

plot(leaps, scale="adjr2")

library(car)
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")

library()
# Calculating standardized regression coefficients
states <- as.data.frame(pgadata[,c("tmoney", "Avrputt", 
                                   "driveacc", "saveacc", "greenReg")])
zstates <- as.data.frame(scale(states))
zfit <- lm(tmoney ~ Avrputt + driveacc + saveacc + greenReg, data=zstates)
coef(zfit)