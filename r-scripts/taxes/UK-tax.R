
#############
# UK
############
this.tax <- function (gross.Income = 40000,
                      personal.Allowance = 11000) {
  
  taxable.Income <- gross.Income - personal.Allowance
  
  income.Tax <-  Progressive.Tax(taxable.Income, 
                                 brackets = c (0, 31786, 150000 ),
                                 rates = c(.2, .4, .45))
  
  national.insurance <- Progressive.Tax(taxable.Income, 
                                        brackets = c (0, 155*52, 815*52 ),
                                        rates = c(0,.12, .02))
  
  net.Income = gross.Income - income.Tax - national.insurance
  
  return(net.Income)
  
}