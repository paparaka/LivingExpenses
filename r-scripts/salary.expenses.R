
# 
# my.countries <- c("California", "Germany", "UK")
my.cities <- c("San Francisco", "London")

foo1 <- expenses%>%
  group_by(Country) %>%
  summarize(
    Basic = quantile(SUM)[3]
  )
names(foo1) <- c("Location", "Basic.Expense")

foo2 <- expenses[expenses$City %in% my.cities, c("City","SUM")]
names(foo2) <- c("Location", "Basic.Expense")

foo <- rbind(foo1,foo2)

goo <- as.data.frame(goingout[,list(Country,Model.Avg)])
names(goo) <- c("Location", "Model.Avg")

expense.model <- merge(foo, goo)
expense.model$Expenses <- expense.model$Basic.Expense + expense.model$Model.Avg


my.order <- c("San Francisco", "California", "London", "UK", "Germany", "Japan")
expense.model$Location <- factor(expense.model$Location, levels = my.order)

p1 <- ggplot(expense.model, aes(x = Location, y = Expenses, fill = Location)) + 
  geom_bar(stat = "identity") +
  labs(title ="Basic + Going Out Expenses, Monthly (USD)",
       x = "",
       y = " ")+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,10000,500)) +
theme_bw(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")


ggsave("export-png/combined expenses.png", p1, width = 12, height = 9, dpi = 300 )

#############

salaries.data.net2 <- merge(salaries.data.net, expense.model)
salaries.data.net2$Currency <- NULL
salaries.data.net2$Basic.Expense <- NULL
salaries.data.net2$Model.Avg <- NULL



p2 <- ggplot (salaries.data.net2,
                aes(y = (Salary.Mean/12 - Expenses), x = Position, fill = Position)) + 
    coord_flip() +
    geom_bar(position = "dodge",stat = "identity") +
    geom_errorbar(aes(ymin = (Salary.Low/12- Expenses), ymax = (Salary.High/12- Expenses) ), alpha = .5) +
    facet_grid(Location ~ .) +
    scale_fill_brewer(palette = "Set1") +
    theme_bw(base_size = 16) +
    theme(legend.position = "none") +
    labs(title = "Monthly Disposable Income in USD",
         x = "",
         y = "")+
    scale_y_continuous(expand = c(0,0),
                       breaks = c(-500,seq(0,1e5, 1000))
                      )

ggsave("export-png/disposable income.png", p2, width = 8, height = 10, dpi = 300)
