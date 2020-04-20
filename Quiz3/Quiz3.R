rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("sjstats")
library("ppcor")
library("corpcor")
library("car")
setwd("./")

# ---- load-data --------------------------------------------------------
data.in <- read.csv("./quiz3Data.csv")
# Make sure our data are factors
data.in$X1 <- factor(data.in$X1)
# create a linear age term
data.in$linAge <- -1
data.in$linAge[data.in$X1=="2"] <- 0
data.in$linAge[data.in$X1=="3"] <- 1
# Now create a quadratic age term
data.in$quadAge <- 1
data.in$quadAge[data.in$X1=="2"] <- -2

# ---- declare-functions --------------------------------------------------------

# ---- q-1-a --------------------------------------------------------
model.out <- lm(Y ~ X1*X2,  data=data.in)
kable(anova(model.out))

# ---- q-1-a-II --------------------------------------------------------
model.out <- lm(Y ~ X1*X2,  data=data.in)
kable(Anova(model.out, type = "II"))

# ---- q-1-a-III --------------------------------------------------------
model.out <- lm(Y ~ X1*X2,  data=data.in)
kable(Anova(model.out, type = "III"))

# ---- q-1-c --------------------------------------------------------
f.val.age <- anova(model.out)["X1","F value"]
p.val.age <- anova(model.out)["X1","Pr(>F)"]

# ---- q-1-d --------------------------------------------------------
f.val.sex <- anova(model.out)["X2","F value"]
p.val.sex <- anova(model.out)["X2","Pr(>F)"]

# ---- q-1-e --------------------------------------------------------
f.val.int <- anova(model.out)["X1:X2","F value"]
p.val.int <- anova(model.out)["X1:X2","Pr(>F)"]

# ---- q-1-g-lin --------------------------------------------------------
lin.age.effect <- lm(Y ~ linAge, data=data.in)
kable(summary(lin.age.effect)$coefficients)

# ---- q-1-g-quad --------------------------------------------------------
quad.age.effect <- lm(Y ~ quadAge, data=data.in)
kable(summary(quad.age.effect)$coefficients)

# ---- q-1-g-linan --------------------------------------------------------
lin.age.effect <- lm(Y ~ linAge, data=data.in)
kable(anova(lin.age.effect))

# ---- q-1-g-quadan --------------------------------------------------------
quad.age.effect <- lm(Y ~ quadAge, data=data.in)
kable(anova(quad.age.effect))

# ---- q-1-h --------------------------------------------------------
lin.age.effect <- lm(Y ~ linAge * X2, data=data.in)
kable(summary(lin.age.effect)$coefficients)

# ---- q-3-a --------------------------------------------------------
y <- runif(8)
factor.a <- factor(c(1,1,1,1,2,2,2,2), levels=c(2,1))
factor.b <- factor(c(1,2,3,4,1,2,3,4), levels=c(4,3,2,1))
data.out <- data.frame(y=y, factorA=factor.a, factorB=factor.b)
kable(model.matrix(y ~ factorA * factorB, data=data.out))

# ---- q-3-a-2 --------------------------------------------------------
y <- runif(8)
factor.a <- factor(c(1,1,1,1,2,2,2,2), levels=c(2,1))
factor.b <- factor(c(1,2,3,4,1,2,3,4), levels=c(4,3,2,1))
data.out <- data.frame(y=y, factorA=factor.a, factorB=factor.b)
kable(model.matrix(y ~ factorA * factorB +-1, data=data.out))
