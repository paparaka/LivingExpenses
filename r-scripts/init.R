library(plyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(data.table)

# TODO replace with a function that loads the latest exchange rates

currencies <- read.csv("import-tables/currencies.csv")
rownames(currencies) <- currencies[,1]
currencies[,1] <- NULL

# example 
# 1 USD = ? EUR:
#currencies["USD","EUR"]

###############
#Taxes

#load calculated tax data for those countries that do not have a function
tax.other <- read.csv("import-tables/taxes.csv")
source("r-scripts/tax.calc.R")
###############

########
#PLOTS 
#######
source("r-scripts/taxes-plot.R")
source("r-scripts/salaries.R")
source("r-scripts/expenses-numbeo.R")
expenses <- Expenses.Plot()



