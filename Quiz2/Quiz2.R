rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("sjstats")
setwd("./")

# ---- load-data --------------------------------------------------------
data.in <- read.csv("./dataQuiz2.csv")

# ---- declare-functions --------------------------------------------------------


# ---- q-1-b --------------------------------------------------------
model.full <- lm(WingSize ~ continent + latitude + Sex, data=data.in)
model.reduced <- lm(WingSize ~ continent + Sex, data=data.in)
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