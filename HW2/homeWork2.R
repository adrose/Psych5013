rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("foreign")
library("knitr")
library("kableExtra")
library("reshape2")
setwd("~/Documents/Psych5013/HW2")


# ---- load-data ----------------------------------------------------------------------
table9_1 <- read.csv("./agrestifinlaychap9table1.csv")
table9_4 <- read.csv("./agrestifinlaychap9table4.csv")
