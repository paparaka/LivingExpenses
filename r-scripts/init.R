library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(data.table)
library(tidyr)
library(ggrepel)

# TODO replace with a function that loads the latest exchange rates

currencies <- read.csv("import-tables/currencies.csv")
rownames(currencies) <- currencies[,1]
currencies[,1] <- NULL

# example 
# 1 USD = ? EUR:
#currencies["USD","EUR"]

###############
#Taxes
source("r-scripts/tax.calc.R")
###############

########
#PLOTS 
#######
source("r-scripts/taxes-plot.R")
#source("r-scripts/salaries.R") TOFO need to fix the tax issue here
source("r-scripts/expenses-numbeo.R")
expenses <- Expenses.Plot(plot = F)



