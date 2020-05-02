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

# ---- declare-functions --------------------------------------------------------

# ---- fubar --------------------------------------------------------
PROF <- c(rep(1, 10), rep(2,12))
SEX <- c(rep(1,6), rep(2, 4), rep(1, 4), rep(2, 8))
y <- runif(22)
mod <- lm(y ~ PROF * SEX)
