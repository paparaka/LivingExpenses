
#############
# Netherlands
############
this.tax <- function (gross.Income = 40000,
                      general.tax.Credit = 2001,
                      labour.tax.Credit = 2001,
                      expat30 = F) {
  
  if (expat30 == TRUE & gross.Income > 52436) {
    taxable.Income <- taxable.Income * .7
  } else {
    taxable.Income <- gross.Income 
  }
  
  
  income.Tax <-  Progressive.Tax(taxable.Income, 
                                 brackets = c (0, 19922, 33715, 66421 ),
                                 rates = c(.3655, .4040, .4040, .52))
  
  if (gross.Income < 45000) {
    labour.tax.Credit <- 2097
  } else if (gross.Income > 80000) {
    labour.tax.Credit <- 367
  } else {
    labour.tax.Credit = -.04 * gross.Income + 3726  # extrapolated from the xls file
  }
  
  if (gross.Income < 20000) {
    general.tax.Credit <- 2103
  } else if (gross.Income > 60000) {
    general.tax.Credit <- 1366
  } else {
    general.tax.Credit = -.02 * gross.Income + 2496 # extrapolated from the xls file
  }
  
  
  net.Income = gross.Income - income.Tax + general.tax.Credit + labour.tax.Credit
  
  return(net.Income)
}