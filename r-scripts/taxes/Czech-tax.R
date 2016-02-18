
#############
# Czech Republic
############
# provided by Adam Nohejl

this.tax <- function (gross.Income, months = 12) {

  socialInsuranceBase  	  <- min(gross.Income, (108024) * months)
  employerHealthInsurance <- 0.09 * gross.Income
  employerSocialInsurance <- 0.25 * socialInsuranceBase
  supergross <- gross.Income + employerHealthInsurance + employerSocialInsurance
  
  generalDeduction	<- (2070) * months
  incomeTax			<- max( 0, 0.15 * supergross - generalDeduction)
  
  healthInsurance 	<- 0.045 * supergross
  socialInsurance 	<- 0.065 * socialInsuranceBase
  solidarityBracket <- (108024) * months
  if( gross.Income > solidarityBracket)
  	incomeTax <- incomeTax + 0.07 * (gross.Income - solidarityBracket)

  net.Income = gross.Income - incomeTax - healthInsurance - socialInsurance
  
  return(net.Income)
}
