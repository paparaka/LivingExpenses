#load calculated tax data for those countries that do not have a function


Progressive.Tax <- function(gross.Income = 17930,
                            brackets = c (0, 8680, 12360, 20600, 37750),
                            rates = c(.25, .3, .4, .45, .5)
                            ) {
  
  tax.Income = 0
  gross.Left = gross.Income
  tax.Owed = 0
  
  for (i in length(brackets):1) {
    if (gross.Left - brackets[i] > 0) {
      tax.Income <- gross.Left - brackets[i]
      tax.Owed <- tax.Owed + tax.Income* rates[i]
      gross.Left <- brackets[i]
    }
  }
  
  return (tax.Owed)
}


#############
# Linear Interpolation for the countries that dont have tax function
############

Tax.Linear.Interpol <- function(gross.Income = 44000, this.country = "Germany") {
  foo <- tax.other[tax.other$Country == this.country, c( "Net","Gross")]
  return (approx(foo$Gross, foo$Net, gross.Income)$y)
  
}

Load.All.Tax.Fun <- function(this.currency = "EUR") {
  
  tax.other <- read.csv("import-tables/taxes.csv")
  tax.fun <- read.csv("import-tables/tax-functions.csv")
  
  taxes.all <- data.frame(Gross = as.numeric(), 
                          Net = as.numeric(), 
                          Country = as.character(),
                          Currency = as.character() )
  
  for (i in 1:nrow(tax.fun)) {
    source(paste0("r-scripts/taxes/",tax.fun$filename[i]))
    
    foo <- data.frame(Gross = seq(tax.fun$Min[i],tax.fun$Max[i],tax.fun$Step[i]), Net = 0, 
                      Country = tax.fun$Country[i], Currency = tax.fun$Currency[i])
    foo$Net <- aaply(foo$Gross, 1, this.tax)
    
    taxes.all <- rbind(taxes.all, foo)
  }
  
  taxes.all <- rbind(taxes.all, tax.other)
  
  
  taxes.all[,c( "Net","Gross")] <- 
    taxes.all[,c("Net","Gross")] * currencies[as.character(taxes.all$Currency),this.currency]
  
  taxes.all$Currency <- this.currency
  
  taxes.all$rate <- 1- (taxes.all$Net / taxes.all$Gross)
  
  return(taxes.all)
  
}
