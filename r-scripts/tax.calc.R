# Functions for calculating taxes

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
# Belgum
############
Belgum.Taxes <- function (gross.Income = 50000,
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

#############
# UK
############
UK.Taxes <- function (gross.Income = 40000,
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

#############
# Netherlands
############
NL.Taxes <- function (gross.Income = 40000,
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

#############
# Norway
############
Norway.Taxes <- function (gross.Income = 500000,
                          basic.allowance = 91450,
                          personal.allowance = 51750) {
  # references:
  #https://www.nordisketax.net/main.asp?url=files/nor/eng/i07.asp
  #http://europa.eu/youreurope/citizens/work/taxes/income-taxes-abroad/norway/index_en.htm
  #http://www.dinside.no/789330/slik-blir-din-skatt-i-2016
  
  taxable.Income <- gross.Income - basic.allowance - personal.allowance
  
  ordinary.Tax <- taxable.Income * .25
  
  bracket.Tax <-  Progressive.Tax(gross.Income, 
                                 brackets = c (0, 159800, 224900, 565400, 909500),
                                 rates = c(0, .0044, .017, .107, .137))
  
  national.Insurance <- gross.Income *.082
  
  net.Income = gross.Income - ordinary.Tax - bracket.Tax - national.Insurance
  
  return(net.Income)
  
  
}

#############
# Austria
############
Austria.Taxes <- function (gross.Income = 50000,
                      personal.Allowance = 0) {
  
  taxable.Income <- gross.Income - personal.Allowance
  
  income.Tax <-  Progressive.Tax(taxable.Income, 
                                 brackets = c (0, 11000, 25000, 60000 ),
                                 rates = c(0, .365, .432, .5))
  
  national.insurance = taxable.Income*0.0186868

  net.Income = gross.Income - income.Tax - national.insurance
  
  return(net.Income)
  
  
}

#############
# California
############
California.Taxes <- function (gross.Income = 80000,
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


#############
# Linear Interpolation for the countries that dont have tax function
############

Tax.Linear.Interpol <- function(gross.Income = 44000, this.country = "Germany") {
  foo <- tax.other[tax.other$Country == this.country, c( "Net","Gross")]
  return (approx(foo$Gross, foo$Net, gross.Income)$y)
  
}

