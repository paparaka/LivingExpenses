
#############
# California
############
this.tax <- function (gross.Income = 80000,
                              state.Deduction = 3906,
                              state.Exemption = 106,
                              federal.Deduction = 6300,
                              federal.Exemption = 4000) {
  
  taxable.State <- gross.Income - state.Deduction - state.Exemption
  
  state.Tax <-  Progressive.Tax( taxable.State , 
                                 brackets = c(0, 7749, 18371, 28995, 40250, 50689, 259844, 311812, 519867, 1000000 ),
                                 rates = c(.01, .02, .04, .06, .08, .093, .1030, .1130, .1230, .1330))
  
  
  taxable.Federal <- gross.Income - federal.Deduction - federal.Exemption
  
  federal.Tax <-  Progressive.Tax(taxable.Federal , 
                                  brackets = c(0, 9225, 37450, 90750, 189300, 411500, 413200),
                                  rates = c(.01,.15, .25, .28, .33, .35, .396))
  
  
  medicare.Tax = taxable.Federal * 0.0145 + 149.35 # extrapolated from the xls file
  
  if (taxable.Federal < 100000) {
    fica.Tax <- taxable.Federal * 0.062 + 638.6
  } else {
    fica.Tax <- 7347 # extrapolated from the xls file
  }
  
  net.Income = gross.Income - state.Tax - federal.Tax - medicare.Tax - fica.Tax
  
  return(net.Income)
  
}