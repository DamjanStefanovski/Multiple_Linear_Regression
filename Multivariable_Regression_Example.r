#Swiss fertility data

library(datasets); data(swiss); require(stats); require(graphics)
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))


#Calling lm

summary(lm(Fertility ~ . , data = swiss))

summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients


#How can adjustment reverse the sign of an effect? Let's try a simulation.

n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef

# Sympsons precived paradox 
summary(lm(y ~ x1 + x2))$coef

library(ggplot2)

z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)






































































