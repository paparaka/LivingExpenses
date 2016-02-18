
expense.freq <- list(beer = 3, coffee = 2, dinner = 2, lunch = 5 ) # frequency of expenses per week

goingout <- expenses %>%
  group_by(Country) %>%
  summarize(
    Meal.Inexpensive = quantile(Meal.Inexpensive)[4],
    Beer = quantile(Beer)[4],
    Cappucino = quantile(Cappucino)[4],
    Lunch = quantile(Lunch)[4],
    Dinner = quantile(Dinner)[4],
    Alcohol = quantile(Alcohol)[4],
    Coffee = quantile(Coffee)[4]
  )

goingout <- rbind(goingout,
                  cbind(data.frame(Country = "London"),expenses[expenses$City == "London",c("Meal.Inexpensive", "Beer", "Cappucino", "Lunch", "Dinner", "Alcohol", "Coffee")]),
                  cbind(data.frame(Country = "San Francisco"),expenses[expenses$City == "San Francisco",c("Meal.Inexpensive", "Beer", "Cappucino", "Lunch", "Dinner", "Alcohol", "Coffee")])
)

goingout <- data.table(goingout)
setkey(goingout, Country)

goingout[, "Model.1" := Meal.Inexpensive * (expense.freq$dinner + expense.freq$lunch) * 4 +
           Beer * expense.freq$beer * 4 +
           Cappucino * expense.freq$coffee * 4 ]
goingout[, "Model.2" := Lunch *  expense.freq$lunch * 4 +
           Dinner * expense.freq$dinner * 4 +
           Alcohol * expense.freq$beer * 4 +
           Coffee * expense.freq$coffee * 4 ]
goingout[, "Model.Avg" := (Model.1 + Model.2)/2 ]

goingout2 <- data.table(gather(goingout, expense, value, -Country))
setkey(goingout2, Country)
my.order <- c("San Francisco", "California", "London", "UK", "Germany", "Japan")
goingout2$Country <- factor(goingout2$Country, levels = my.order)

p <- ggplot(goingout2[expense %in% c("Model.1","Model.2","Model.Avg"),], 
            aes(x = Country, y = value, group = expense, fill = expense)) +
  geom_bar(stat = "identity", position = "dodge", colour = "white") +
  labs(title ="Going out model",
       x = "",
       y = "value")+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,1000,100))
p <- Expenses.Comon.Plot(p, "bottom")
p
ggsave("export-png/going out model.png", p, width = 12, height = 9, dpi = 300 )