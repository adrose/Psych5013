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
data.in <- read.csv("./dataQuiz2.csv")
data.in$latitudeScale <- scale(data.in$latitude)
data.in$WingSizeScale <- scale(data.in$WingSize)

# ---- declare-functions --------------------------------------------------------


# ---- q-1-b --------------------------------------------------------
model.full <- lm(WingSize ~ continent + latitude + Sex, data=data.in)
model.reduced <- lm(WingSize ~ continent + Sex, data=data.in)


# ---- q-2-a --------------------------------------------------------
model.int <- lm(WingSize ~ continent * latitude + Sex, data=data.in)
model.int.rev <- lm(WingSize*-1 ~ continent * latitude + Sex, data=data.in)
model.int.scale <- lm(WingSizeScale ~ continent * latitudeScale + Sex, data=data.in)
model.int.sum <- summary(model.int)
print(model.int.sum)

# Now calc the Type III SoS
options(contrasts = c("contr.sum", "contr.poly"))
tmp <- Anova(model.int, type='III')["Sex","F value"]

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

# ---- q-2-f --------------------------------------------------------
# Here I need to find the largest residual value and the corresponding observation
max.resid.val <- which.max(abs(model.int$residuals))

# Now add a column of predicted values to our data.in df
data.in$PredictedValues <- predict(model.int)

# Now find the row elements
vals.out <- kable(data.in[max.resid.val,])
vals.out

# ---- q-2-f2 --------------------------------------------------------
# Here I need to find the largest residual value and the corresponding observation
max.resid.val <- which.max(abs(model.int$residuals))

# Now add a column of predicted values to our data.in df
data.in$PredictedValues <- predict(model.int)

# Now find the row elements
vals.out <- kable(data.in[max.resid.val,c("WingSize","PredictedValues")])
vals.out

# ---- q-2-g --------------------------------------------------------
# Here I am going to interpret the value of the interaction term
# So I just need to grab the interaction term value right here
int.val <- summary(model.int)$coefficients["continentna:latitude","Estimate"]

# ---- q-2-h --------------------------------------------------------
t.val.int <- summary(model.int.scale)$coefficients["continentna:latitudeScale","t value"]

# ---- q-2-i --------------------------------------------------------
anova.val <- anova(model.full, model.int)
anova.val.f <- anova.val$F[2]

# ---- q-2-j --------------------------------------------------------
data.in[43,] <- c("na",45,0,"Female",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
# Now predict from all of these models... because Terry does not make it clear which model to use
model.reduced.pred <-predict(model.reduced, newdata = data.in[43,])
model.full.pred <- predict(model.full, newdata = data.in[43,])
model.int.pred <- predict(model.int, newdata = data.in[43,])

# ---- q-2-k --------------------------------------------------------
data.in[43,] <- c("na",0,0,"Female",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.int <- predict(model.int, newdata = data.in[43,])
data.in[43,] <- c("na",100,0,"Female",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.p2 <- predict(model.int, newdata = data.in[43,])
b.1.slope <- (b.1.p2 - b.1.int) / 100

# ---- q-2-l --------------------------------------------------------
data.in[43,] <- c("eu",0,0,"Female",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.int <- predict(model.int, newdata = data.in[43,])
data.in[43,] <- c("eu",100,0,"Female",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.p2 <- predict(model.int, newdata = data.in[43,])
b.1.slope <- (b.1.p2 - b.1.int) / 100

# ---- q-2-m --------------------------------------------------------
data.in[43,] <- c("na",0,0,"Male",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.int <- predict(model.int, newdata = data.in[43,])
data.in[43,] <- c("na",100,0,"Male",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.p2 <- predict(model.int, newdata = data.in[43,])
b.1.slope <- (b.1.p2 - b.1.int) / 100

# ---- q-2-n --------------------------------------------------------
data.in[43,] <- c("eu",0,0,"Male",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.int <- predict(model.int, newdata = data.in[43,])
data.in[43,] <- c("eu",100,0,"Male",0,0,0)
data.in$latitude <- as.numeric(data.in$latitude)
b.1.p2 <- predict(model.int, newdata = data.in[43,])
b.1.slope <- (b.1.p2 - b.1.int) / 100

# ---- q-4 --------------------------------------------------------
library(matlib)
for.r.sqraed <- matrix(c(1,.389,.2565,.3890,1,.2317,.2565,.2317,1), nrow = 3, ncol = 3)
corY <- for.r.sqraed[1:2,3]
rInv <- inv(for.r.sqraed[1:2,1:2])
coef.det <- t(corY) %*% rInv %*% corY
ad.r.square <- 1 - ((1-coef.det)*(99)/97)


## Now do the semi partial cors down here
# here is the cor matrix
row.1 <- c(1,.389,.2565)
row.2 <- c(.389, 1, .2317)
row.3 <- c(.2565,.2317,1)
cor.mat <- rbind(row.1, row.2, row.3)
# Here is the semi partial cor formula for two variables
# (corrs[2,3]-corrs[2,1]*corrs[3,1])/(sqrt(1-corrs[2,1]^2))
# Here column 3 is the DV, column 2 is the var of int, and column 1 is the var to partial out of the IV
##         SATV  HSGPA   FGPA
## SATV  1.0000 0.8745 0.8144
## HSGPA 0.8745 1.0000 0.9226
## FGPA  0.8144 0.9226 1.0000
spcor.x2.x1 <- (cor.mat[2,3]-cor.mat[2,1]*cor.mat[3,1])/(sqrt(1-cor.mat[2,1]^2)) 
spcor.x1.x2 <- (cor.mat[1,3]-cor.mat[2,1]*cor.mat[3,2])/(sqrt(1-cor.mat[1,3]^2)) 


# Now grab our p cors from the cor matrix
p.cor.val <- cor2pcor(cor.mat)[2,3]^2
