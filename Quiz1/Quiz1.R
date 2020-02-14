rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("foreign")
library("knitr")
library("kableExtra")
library("reshape2")
setwd("~/Documents/Psych5013/Quiz1/")

# ---- q-3-data --------------------------------------------------------
x <- c(59,49,75,54,78,56,60,82,69,83,88,94,47,65,89,70)
y <- c(209,180,195,192,215,197,208,189,213,201,214,212,205,186,200,204)
q.three.dat <- data.frame(x,y)

# ---- q-3-a --------------------------------------------------------
mod <- lm(y ~ x, data=q.three.dat)
summary(mod)
# ---- q-3-b --------------------------------------------------------
cor(q.three.dat$x, q.three.dat$y)

# ---- q-3-c --------------------------------------------------------
q.three.dat$xStand <- scale(q.three.dat$x)
q.three.dat$yStand <- scale(q.three.dat$y)
mod.stand <- lm(yStand ~ xStand, data=q.three.dat)
summary(mod.stand)
