rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("foreign")
library("knitr")
library("kableExtra")
library("reshape2")
setwd("~/Documents/Psych5013/HW1")


# ---- load-data ----------------------------------------------------------------------
table9_1 <- read.csv("./agrestifinlaychap9table1.csv")
table9_4 <- read.csv("./agrestifinlaychap9table4.csv")

# ---- train-model-1 ----------------------------------------------------------------------
model.1 <- lm(Violent_Crime_Rate ~ Poverty_Rate, data=table9_1)

# ---- graph-model-1 ----------------------------------------------------------------------
out.plot <- ggplot(table9_1, aes(x=Poverty_Rate, y=Violent_Crime_Rate)) +
  geom_point() +
  #geom_smooth(method='lm') +
  geom_abline(intercept = 209.9, slope = 25.5, color="red", linetype="dashed", size=1.5) +
  coord_cartesian(xlim=c(0,100)) +
  theme_bw()
out.plot

# ---- graph-model-12 ----------------------------------------------------------------------
out.plot <- ggplot(table9_1, aes(x=Violent_Crime_Rate, y=Murder_Rate)) +
  geom_point() +
  theme_bw()
out.plot

# ---- train-model-12 ----------------------------------------------------------------------
model.2 <- lm(Murder_Rate~Violent_Crime_Rate, data=table9_1)
