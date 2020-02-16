rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("sjstats")
setwd("./")

# ---- declare-functions --------------------------------------------------------
mean.pred.intervals <- function(x, y, pred.x) {
  n <- length(y) # Find sample size
  lm.model <- lm(y ~ x) # Fit linear model
  y.fitted <- lm.model$fitted.values # Extract the fitted values of y
  
  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  pred.y <- b1 * pred.x + b0 # Predict y at the given value of x (argument pred.x)
  
  # Find SSE and MSE
  sse <- sum((y - y.fitted)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Critical value of t
  
  mean.se.fit <- (1 / n + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the mean estimate
  pred.se.fit <- (1 + (1 / n) + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the prediction
  
  # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
  mean.conf.upper <- pred.y + t.val * sqrt(mse * mean.se.fit)
  mean.conf.lower <- pred.y - t.val * sqrt(mse * mean.se.fit)
  
  # Prediction Upper and Lower Confidence limits at 95% Confidence
  pred.conf.upper <- pred.y + t.val * sqrt(mse * pred.se.fit)
  pred.conf.lower <- pred.y - t.val * sqrt(mse * pred.se.fit)
  
  # Beta 1 Upper and Lower Confidence limits at 95% Confidence
  b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
  b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
  
  # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
  upper <- data.frame(rbind(round(mean.conf.upper, 2), round(pred.conf.upper, 2), round(b1.conf.upper, 2)))
  lower <- data.frame(rbind(round(mean.conf.lower, 2), round(pred.conf.lower, 2), round(b1.conf.lower, 2)))
  fit <- data.frame(rbind(round(pred.y, 2), round(pred.y, 2), round(b1, 2)))
  
  # Collect all into data.frame and rename columns
  results <- data.frame(cbind(lower, upper, fit), row.names = c('Mean', 'Prediction', 'Coefficient'))
  colnames(results) <- c('Lower', 'Upper', 'Fit')
  
  return(results)
}

# ---- q-3-data --------------------------------------------------------
x <- c(59,49,75,54,78,56,60,82,69,83,88,94,47,65,89,70)
y <- c(209,180,195,192,215,197,208,189,213,201,214,212,205,186,200,204)
q.three.dat <- data.frame(x,y)

# ---- q-3-a --------------------------------------------------------
mod <- lm(y ~ x, data=q.three.dat)
#summary(mod)

# ---- q-3-b --------------------------------------------------------
cor.val <- cor(q.three.dat$x, q.three.dat$y)

# ---- q-3-c --------------------------------------------------------
q.three.dat$xStand <- scale(q.three.dat$x)
q.three.dat$yStand <- scale(q.three.dat$y)
mod.stand <- lm(yStand ~ xStand, data=q.three.dat)
#summary(mod.stand)

# ---- q-3-f --------------------------------------------------------
# First find the value closest to the mean
index <- which(abs(q.three.dat$xStand)==min(abs(q.three.dat$xStand)))
# Now I want to find the CLM -- the confidence interval of the predicted mean value
clm.vals <- mean.pred.intervals(x = q.three.dat$x, y = q.three.dat$y, pred.x = 70)[1,1:2]


# ---- q-3-g --------------------------------------------------------
# First find the value closest to the mean
index.2 <- which(q.three.dat$xStand==max(q.three.dat$xStand))
# Now I want to find the CLI -- the confidence interval for the predicted value
cli.vals <- mean.pred.intervals(x = q.three.dat$x, y = q.three.dat$y, pred.x = 94)[2,1:2]

# ---- q-3-i --------------------------------------------------------
plot(q.three.dat$x, q.three.dat$y, xlab = "Population Density", ylab="Robbery Rate")

plot(mod)