
#############
# Austria
############
this.tax <- function (gross.Income = 50000,
                           personal.Allowance = 0) {
  
  taxable.Income <- gross.Income - personal.Allowance
  
  income.Tax <-  Progressive.Tax(taxable.Income, 
                                 brackets = c (0, 11000, 25000, 60000 ),
                                 rates = c(0, .365, .432, .5))
  
  national.insurance = taxable.Income*0.0186868
  
  net.Income = gross.Income - income.Tax - national.insurance
  
  return(net.Income)
  
  
}