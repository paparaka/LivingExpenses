#source("r-scripts/init.R")
#display.brewer.all()


#######

salaries <- read.csv("import-tables/salaries.csv")
salaries$Sample <- as.integer(salaries$Sample )


salaries <- salaries %>%
  group_by(Position, Location, Currency) %>%       
  summarize(
    Salary.Mean = sum(Salary.Mean*Sample)/sum(Sample),
    Salary.Low = sum(Salary.Low*Sample)/sum(Sample),
    Salary.High = sum(Salary.High*Sample)/sum(Sample)
  )

# currency convertion
salaries[,c( "Salary.Mean","Salary.Low","Salary.High")] <-
  salaries[,c( "Salary.Mean","Salary.Low","Salary.High")] * currencies[as.character(salaries$Currency),"USD"]


############ PLOTS ##############
#################################


salaries.data <- subset(as.data.frame(salaries),  !(Location %in% c("Denmark", "Canada", "USA")) &
                (Position %in% c("Business Analyst", "Data Scientist", "Junior Software Engineer", 
                                 "Senior Software Engineer ","Software Engineer")))

salaries.data$Position <- factor(salaries.data$Position)
salaries.data$Position <- factor(salaries.data$Position, levels = levels(salaries.data$Position)[c(1:3,5,4)])
salaries.data$Location <- factor(salaries.data$Location, levels = levels(salaries.data$Location)[c(5,7,1,4,6,3,2)])


p1 <- ggplot (salaries.data,
        aes(y = Salary.Mean, x = Position, fill = Position)) + 
  coord_flip() +
  geom_bar(position = "dodge",stat = "identity") +
  geom_errorbar(aes(ymin = Salary.Low, ymax = Salary.High), alpha = .5) +
  facet_grid(Location ~ .) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(title = "Yearly Gross Salary in USD",
       x = "",
       y = "")+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,1e7, 25000))

ggsave("export-png/Data salaries gross.png", p1, width = 8, height = 10, dpi = 300)
ggsave("export-pdf/Data salaries gross.pdf", p1, width = 8, height = 10)


# salaries NET per month
# using the tax functions for each country, calculate the net salary:

salaries.data.net <- salaries.data

salaries.data.net[salaries.data.net$Location == "San Francisco",
                  c("Salary.High","Salary.Low","Salary.Mean")] <-
  adply(salaries.data.net[salaries.data.net$Location == "San Francisco",
                                        c("Salary.High","Salary.Low","Salary.Mean")],
        1, California.Taxes)
  
  
salaries.data.net[salaries.data.net$Location %in% c("London","UK"),
                  c("Salary.High","Salary.Low","Salary.Mean")] <-
  adply(salaries.data.net[salaries.data.net$Location %in% c("London","UK"),
                                          c("Salary.High","Salary.Low","Salary.Mean")],
        1, UK.Taxes)


salaries.data.net[salaries.data.net$Location %in% c("Germany"),
                  c("Salary.High","Salary.Low","Salary.Mean")] <-
  as.data.frame(aaply(salaries.data.net[salaries.data.net$Location %in% c("Germany"),
                          c("Salary.High","Salary.Low","Salary.Mean")],
        c(1,2), Tax.Linear.Interpol, this.country = "Germany"))


p2 <- ggplot (salaries.data.net,
              aes(y = Salary.Mean/12, x = Position, fill = Position)) + 
  coord_flip() +
  geom_bar(position = "dodge",stat = "identity") +
  geom_errorbar(aes(ymin = Salary.Low/12, ymax = Salary.High/12), alpha = .5) +
  facet_grid(Location ~ .) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(title = "Monthly Net Salary in USD",
       x = "",
       y = "")+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,1e5, 1000))

ggsave("export-png/Data salaries net.png", p2, width = 8, height = 10, dpi = 300)
ggsave("export-pdf/Data salaries net.pdf", p2, width = 8, height = 10)

# ggplot (salaries.data.net,
#               aes(y = Salary.Mean, x = Position, fill = Position)) + 
#   coord_flip() +
#   geom_point() +
#   geom_point(data = salaries.data, aes(y = Salary.Mean, x = Position, colour = "red")) +
#   facet_grid(Location ~ .) 

