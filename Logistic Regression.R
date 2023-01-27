vars <- c("Months Experience", "Success or Not", "Fits")
data <- read.table("CH14TA01.txt", header=FALSE, col.names = vars)
data



plot(data[,1], data[,2], main="Success (1) or Not (0) vs.
Months of Experience", xlab="Months of Experience",
     ylab="Success (1) or Not (0)",
     cex.lab=1.8, cex.main=2)


windows()
par(mfrow=c(3,4))
curve((1 + exp(-0 - 1 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-0 - 2 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-0 - 3 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-0 + 4 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-2 - 1 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-2 - 2 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-2 - 3 * x))^(-1), -5, 5, n=101)
curve((1 + exp(-2 + 4 * x))^(-1), -5, 5, n=101)
curve((1 + exp(4 - 1 * x))^(-1), -5, 5, n=101)
curve((1 + exp(4 - 2 * x))^(-1), -5, 5, n=101)
curve((1 + exp(4 - 3 * x))^(-1), -5, 5, n=101)
curve((1 + exp(4 + 4 * x))^(-1), -5, 5, n=101)



## Response here is Bernoulli (which is a special case of the Binomial)
logisticmodel <- glm(Success.or.Not
                     ~ Months.Experience, data = data, family=binomial)

summary(logisticmodel)




###  Plotting the logistic regression...
windows()
par(mfrow=c(1,1))
x <- seq(-50, 100, 1)
plot(data[,1], data[,2], main="Success (1) or Not (0) vs.
Months of Experience", xlab="Months of Experience", ylab="Success (1) or Not (0)",
     cex.lab=1.8, cex.main=2, xlim=c(-10, 50))
library(faraway)
lines(x, ilogit(-3.05970+0.16149*x))



probitmodel <- glm(Success.or.Not
                   ~ Months.Experience, data = data, family=binomial(link=probit))
summary(probitmodel)
lines(x, pnorm(-1.83787+0.09686*x), lty=2)


#To estimate the chance of success for someone with 30 yrs experience:
ilogit(-3.05970 +0.16149*30)

pnorm(-1.83787+0.09686*30)




##########################################
#
#
#  	Challenger... binomial data and 
#	logistic regression...  Go to YouTube 
# 	for the Challenger video...
#		
#
###########################################

library(faraway)
data(orings)
orings

plot(damage/6 ~ temp, orings, xlim = c(25, 85), ylim = c(0,1), xlab="Temperature", ylab = "Prob of Damage")
lmod <- lm(damage/6 ~ temp, orings)
abline(lmod)


## Logit model...
logitmod <- glm(cbind(damage, 6-damage) ~ temp, family=binomial, orings)
summary(logitmod)

windows()
plot(damage/6 ~ temp, orings, xlim = c(25, 85), ylim = c(0,1), xlab="Temperature", ylab="Prob of Damage")
x <- seq(25, 85, 1)
lines(x, ilogit(11.6630-0.2162*x))


## Probit...
probitmod <- glm(cbind(damage, 6-damage) ~ temp, family=binomial(link=probit), orings)
summary(probitmod)

lines(x, pnorm(5.5915-0.1058*x), lty=2)

##compare the predicted probabilities...
ilogit(11.6630-0.2162*31)
pnorm(5.5915-0.1058*31)




#####################################



# To comparing logistic regression models, we sometimes 
# use the deviance:   D = 2* log (L_L/L_S)... typically L_L is the 
# likelihood function for a model that fits perfectly, and the 
# L_S is the likelihood function for the model with which you're 
# currently working.  The residual deviance is the deviance 
# for the current model (twice the log of the likelihood for a saturated
# model divided by the likelihood of the current model), whereas the 
# while the null deviance is the deviance between the saturated model and 
# a model with no predictor variables and just an intercept/ bias term.
# If the response distribution is truly whatever you're supposing it is
# (in the Challenger example, we're assuming the response is conditionally
# binomial given the temperature), and if the n_i are large (like there are 
# multiple observations of orings at each temperature) then the deviance 
# behaves approximately chi-squared with n - s df ( n = # of data points; 
# s = # of parameters in the model... so for the null deviance, s=1).

# getting a p-value...
pchisq(deviance(logitmod), df.residual(logitmod), lower=F)    

# So conclude the simpler of the two models to be adequate (our model with
# beta_0 and beta_1 vs. a saturated model)

# p-value for the null model...
pchisq(38.9, 22, lower=F)    

# this small p-value indicates our logit model fits better than 
# one with only a bias term in the linear part eta.

# If the null model is adequate, the diffence between these deviances 
# behaves chi-squared with 1 df (it's actually just the deviance between our model
# and the null model.  For smaller n_i values (numbers of observations 
# per temperature setting like we have), this tends to be more accurately
# described by a chi-square distribution


# CIs for the parameters...  for beta_1 from the logit model...
c(-.2162 - 1.96*.0532, -.2162+1.96*.0532)

# Profile loglikelihood...  tends to be more accurate...
library(MASS)
confint(logitmod)