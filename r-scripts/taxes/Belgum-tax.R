
#############
# Belgum
############
this.tax <- function (gross.Income = 50000,
                          personal.Allowance = 7070,
                          social.secuiry.rate = .1307) {
  
  taxable.Income <- gross.Income - personal.Allowance
  
  income.Tax <-  Progressive.Tax(taxable.Income, 
                                 brackets = c (0, 8680, 12360, 20600, 37750),
                                 rates = c(.25, .3, .4, .45, .5))
  
  social.security.Tax <- taxable.Income*social.secuiry.rate
  
  net.Income = gross.Income - income.Tax - social.security.Tax
  
  return(net.Income)
  
  
}