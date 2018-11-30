# Q1
My.Name <- 'Jimmy Goddard'

# Q2
nTrials <- 150
pSuccess <- 0.33
x <- seq(from = 0, to = nTrials, by = 1)
y <- dbinom(x, size = nTrials, prob = pSuccess)
plot(x, y)

# Q3
test.result <- cor.test(mtcars$mpg, mtcars$hp)
p.value <- test.result$p.value
alpha <- c(0.05, 0.025, 0.005) # for two tailed hypothesis
p.value < alpha
# The p-value of 1.787835e-07 is lower than all standard values of alpha, so we should reject the null
# hypothesis that the MPG and HP are _not_ correlated.  The p-value is significantly lower
# than the standard values of alpha
#
# This test confirms the value of the correlation between MPG and HP, -0.7761684, although
# it is an anti-correlation

# Q4
Y <- mtcars$mpg
X <- factor(mtcars$cyl)
model <- aov(Y ~ X)
summary(model)
# The p-value of the ANOVA test is 4.98e-09 which is significantly lower than alpha at 0.05, so we
# would reject the null hypothesis that the mean mpg across number of cylinders are equal. Therefore,
# the mpg does depend on the number of cylinders

# Q5
Y <- mtcars$hp
X <- factor(mtcars$wt)
model <- aov(Y ~ X)
summary(model)
# The p-value of this ANOVA test is 0.239 which is not lower than alpha at 0.05, so we would not be
# able to reject the null hypothesis that the mean hp across different weights are equal.