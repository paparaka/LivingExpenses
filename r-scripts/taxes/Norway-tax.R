
#############
# Norway
############
this.tax <- function (gross.Income = 500000,
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