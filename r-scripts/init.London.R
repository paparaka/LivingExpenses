library(psych)

uk <- read.csv("survey-london.csv", header = TRUE,
                   colClasses = c("character",rep("factor", 8),rep("numeric",4), "NULL",
                                 "factor",rep("numeric",5),"factor", "NULL",
                                 rep("numeric",4),rep("factor", 2),"numeric", "NULL",
                                 rep("numeric",3),"factor", "NULL"))



uk$Employment[uk$Employment == "Slave ( Grad student)"] <- "Student"
uk$Employment <- factor(uk$Employment)

uk$Age <- factor(uk$Age,levels(uk$Age)[c(4,1:3)])


uk$WorkArea[uk$WorkArea == "Legal"] <- "Other"
uk$WorkArea <- factor(uk$WorkArea, labels = c("Marketing", "Business / Finance", "Education", "Engineering", "Health", "IT","Other"))

uk$Sherlock <- NULL



############################
## Engineered variables   ##
############################
uk$London <- FALSE # London: True or False
uk$London [uk$City == "London"] <- TRUE 

# calculate the monthly expenses for Rent + Utilities + Transportation
uk <- within(uk,{
  eat <- Eat.Out * Eat.Out.Price
  food.avg <- (Food.min + Food.max)/2
  Montly.Expenses.Computed <-   rowSums( cbind (Rent,Council.Tax,Renters.Insurance,Electicity,Gas,
                                                Water,Internet, Mobile, Commute.Price, eat, food.avg), 
           na.rm=TRUE)
  
  eat <- NULL
  food.avg <- NULL

})

# group incomes in 3 categories
uk$Income2 <- NA
uk$Income2[uk$Income %in% c("", "0 - 20,000")] <- "Low"
uk$Income2[uk$Income %in% c("20,001 -  30,000", "30,001 -  40,000", "40,001 -  50,000")] <- "Medium"
uk$Income2[uk$Income %in% c("50,001 -  60,000", "60,001 -  80,000")] <- "High"
uk$Income2 <- factor(uk$Income2 )

# compute average utilities 

with( describe(uk[c("Electicity", "Gas", "Water", "Internet", "Mobile")]) ,{
  c( sum(mean), sqrt(sum(se^2)) )
 # which(Employment == 'Employed' & City == "London")
})



