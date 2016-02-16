

# generate the data for the other countries
tax.uk <- data.frame(Gross = seq(20000,120000,5000), Net = 0, Country = "UK", Currency = "GBP")
tax.uk$Net <- aaply(tax.uk$Gross, 1, UK.Taxes)

tax.n <- data.frame(Gross = seq(200000,1500000,50000), Net = 0, Country = "Norway", Currency = "NOK")
tax.n$Net <- aaply(tax.n$Gross, 1, Norway.Taxes)

tax.ca <- data.frame(Gross = seq(20000,150000,5000), Net = 0, Country = "California", Currency = "USD")
tax.ca$Net <- aaply(tax.ca$Gross, 1, California.Taxes)

tax.be <- data.frame(Gross = seq(20000,120000,5000), Net = 0, Country = "Belgum", Currency = "EUR")
tax.be$Net <- aaply(tax.be$Gross, 1, Belgum.Taxes)

tax.nl <- data.frame(Gross = seq(20000,120000,5000), Net = 0, Country = "Netherlands", Currency = "EUR")
tax.nl$Net <- aaply(tax.nl$Gross, 1, NL.Taxes)

tax.a <- data.frame(Gross = seq(20000,120000,5000), Net = 0, Country = "Austria", Currency = "EUR")
tax.a$Net <- aaply(tax.a$Gross, 1, Austria.Taxes)



taxes.all <- rbind(tax.uk, tax.n, tax.be, tax.nl, tax.a, tax.ca, tax.other)

taxes.all[,c( "Net","Gross")] <- 
  taxes.all[,c("Net","Gross")] * currencies[as.character(taxes.all$Currency),"EUR"]

taxes.all$Currency <- "EUR"

taxes.all$rate <- 1- (taxes.all$Net / taxes.all$Gross)


##################

p1 <- ggplot( taxes.all[taxes.all$Country != "California",], aes(x = Gross, y = rate*100, colour = Country, shape = Country)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(30000,120000),
                     breaks = seq(0,200000, 10000)) +
  scale_y_continuous(limits = c(15,54),
                     breaks = seq(0,100,5)) +
  labs(title = "",
       x = "Gross Salary EUR",
       y = "Effective rate [%]",
       colour = "",
       shape = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 14) 
p1

ggsave("export-png/taxes-rate-eur.png", p1, width = 8, height = 7, dpi = 300)
ggsave("export-pdf/taxes-rate-eur.pdf", p1, width = 8, height = 7)

##################

taxes.sub <- subset(taxes.all, Country %in% c("California", "UK", "Germany"))
taxes.sub[,c( "Net","Gross")] <- 
  taxes.sub[,c("Net","Gross")] * currencies[as.character(taxes.sub$Currency),"USD"]


p2 <- ggplot(taxes.sub, 
       aes(x = Gross, y = rate*100, colour = Country, shape = Country)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_x_continuous(limits = c(30000,120000),
                     breaks = seq(0,200000, 10000)) +
  scale_y_continuous(limits = c(12,46),
                     breaks = seq(0,100,5)) +
  labs(title = "",
       x = "Gross Salary USD",
       y = "Effective rate [%]",
       colour = "",
       shape = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")
p2

ggsave("export-png/taxes-rate-usd.png", p2, width = 8, height = 7, dpi = 300)
ggsave("export-pdf/taxes-rate-usd.pdf", p2, width = 8, height = 7)


