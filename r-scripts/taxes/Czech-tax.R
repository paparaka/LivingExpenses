
#############
# Czech Republic
############

CzechRepublic.convertCZK <- function(czk){
	return(czk/24.2430) # 24.2430 CZK = 1 USD as of 2016-02-17
}

CzechRepublic.Taxes <- function (gross.Income, months = 12) {

  socialInsuranceBase  	  <- min(gross.Income, CzechRepublic.convertCZK(108024)*months)
  employerHealthInsurance <- 0.09 * gross.Income
  employerSocialInsurance <- 0.25 * socialInsuranceBase
  supergross <- gross.Income + employerHealthInsurance + employerSocialInsurance
  
  generalDeduction	<- CzechRepublic.convertCZK(2070)*months
  incomeTax			<- max( 0, 0.15 * supergross - generalDeduction)
  
  healthInsurance 	<- 0.045 * supergross
  socialInsurance 	<- 0.065 * socialInsuranceBase
  solidarityBracket <- CzechRepublic.convertCZK(108024)*months
  if( gross.Income > solidarityBracket)
  	incomeTax <- incomeTax + 0.07 * (gross.Income - solidarityBracket)

  net.Income = gross.Income - incomeTax - healthInsurance - socialInsurance
  
  return(net.Income)
}
