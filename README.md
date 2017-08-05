#Properties of the residuals

#$E[e_i] = 0$.
#If an intercept is included, $\sum_{i=1}^n e_i = 0$
#If a regressor variable, $X_i$, is included in the model $\sum_{i=1}^n e_i X_i = 0$.
#Residuals are useful for investigating poor model fit.
#Positive residuals are above the line, negative residuals are below.
#Residuals can be thought of as the outcome ($Y$) with the linear association of the predictor ($X$) removed.
#One differentiates residual variation (variation after removing the predictor) from systematic variation (variation explained by the regression model).
#Residual plots highlight poor model fit.


## Code
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

sum(e)

sum(e * x)


#Residuals are the signed length of the red lines

plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)


#Residuals versus X

plot(diamond$carat, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)



#Non-linear data

x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2); 
plot(x, y); abline(lm(y ~ x))
plot(x, resid(lm(y ~ x))); 
abline(h = 0)


plot(x, resid(lm(y ~ x))); 
abline(h = 0)


#Heteroskedasticity

x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
plot(x, y); abline(lm(y ~ x))



# Diamond example

y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma


sqrt(sum(resid(fit)^2) / (n - 2))

# The proof of not BS ing.. you can always prove with the sum of the square of the deviations

# Regression variability and residual variability 
# The variability explained by the model, + the residual variability .. The one not explained by the model. 
# R2(sq) is the percentage of variatuon explained by the regression model 

# Do example ( anscombe) to see the following data 
# Basically same mean and variance of X and Y








