
#############
# Czech Republic
############
# provided by Adam Nohejl

this.tax <-  function (gross.Income) {
  # Checked vs. http://www.penize.cz/kalkulacky/mzdova-kalkulacka for year 2016
  
  YEAR_MONTHS	 <- 12
  gross.Income.monthly	    <- gross.Income/YEAR_MONTHS
  fourTimesAvgWage	        <- 108024
  solidarityTreshold	        <- fourTimesAvgWage*YEAR_MONTHS
  socialInsuranceTreshold   <- solidarityTreshold			# "happens" to be the same
  socialInsuranceMonths     <- max(ceiling(socialInsuranceTreshold/gross.Income.monthly),YEAR_MONTHS)
  socialInsuranceBase  	    <- gross.Income.monthly*YEAR_MONTHS
  employerHealthInsurance   <- 0.09 * gross.Income
  employerSocialInsurance   <- 0.25 * socialInsuranceBase
  supergross <- gross.Income + employerHealthInsurance + employerSocialInsurance
  
  generalDeduction	<- 2070*YEAR_MONTHS
  incomeTax			<- max( 0, 0.15 * supergross - generalDeduction)
  
  healthInsurance 	<- 0.045 * gross.Income
  socialInsurance 	<- 0.065 * socialInsuranceBase
  solidarityTax     <- 0.07 * max(0 , gross.Income - solidarityTreshold)

  net.Income = gross.Income - incomeTax - healthInsurance - socialInsurance - solidarityTax
  
  return(net.Income)
}
