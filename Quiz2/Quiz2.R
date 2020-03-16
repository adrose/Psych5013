rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("sjstats")
library("ppcor")
setwd("./")

# ---- load-data --------------------------------------------------------
data.in <- read.csv("./dataQuiz2.csv")

# ---- declare-functions --------------------------------------------------------


# ---- q-1-b --------------------------------------------------------
model.full <- lm(WingSize ~ continent + latitude + Sex, data=data.in)
model.reduced <- lm(WingSize ~ continent + Sex, data=data.in)


# ---- q-2-a --------------------------------------------------------
model.int <- lm(WingSize ~ continent * latitude + Sex, data=data.in)
model.int.sum <- summary(model.int)
print(model.int.sum)

# ---- q-2-b --------------------------------------------------------
cor.val <- cor(model.int$fitted.values, data.in$WingSize)

# ---- q-2-c --------------------------------------------------------
# First get the f value
f.value <- anova(model.int)['continent:latitude','F value']
p.value <- anova(model.int)['continent:latitude','Pr(>F)']

## Now calculate the partial cor value
p.cor.data <- data.frame(model.matrix(model.int)[,-1])
p.cor.data$Wingsize <- data.in$WingSize
p.cor.vals <- pcor(x=p.cor.data)$estimate['Wingsize',"continentna.latitude"]
squared.p.cor <- p.cor.vals*p.cor.vals

# ---- q-2-e --------------------------------------------------------
root.value <- rmse(model.int)

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