library(ggplot2)

ggplot(uk, aes(x = Rent)) + stat_bin(binwidth = 250) 

ggplot(uk, aes(x = Council.Tax)) + stat_bin(binwidth = 20)

ggplot(uk, aes(x = Internet)) + stat_bin(binwidth = 10)

ggplot(uk, aes(x = Mobile)) + stat_bin(binwidth = 5)

ggplot(uk, aes(x = Food.min)) + stat_bin(binwidth = 50)
ggplot(uk, aes(x = Food.max)) + stat_bin(binwidth = 50)


ggplot(uk, aes(x = Commute.Price)) + stat_bin(binwidth = 25)
ggplot(uk, aes(x = Monthly.Expenses)) + stat_bin(binwidth = 250)



ggplot(uk, aes(x = Montly.Expenses.Computed)) + 
  geom_histogram(aes(y = ..density..),      
                 binwidth = 250,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  facet_grid( London ~ .)


ggplot(uk, aes(x = Income, y = Montly.Expenses.Computed, group = London, colour = London)) + 
  geom_point() + stat_smooth(method = "lm")



ggplot(uk, aes(x = Food.min)) + 
  geom_density(alpha=.2, fill="#FF6666") +
  geom_density(aes(x= Food.max), alpha=.2, fill="#66FFFF")
