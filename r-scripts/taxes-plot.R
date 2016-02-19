
taxes.all <- Load.All.Tax.Fun()
##################

plot.taxes <- taxes.all[taxes.all$Country != "California" & 
                          taxes.all$Gross < 120000 &
                          taxes.all$Gross > 10000,]

plot.labels <- plot.taxes %>%
  group_by(Country) %>%
  summarize(
    Gross.max = max(Gross),
    Net.max = max(Gross),
    rate.max = max(rate),
    Gross.min = min(Gross),
    Net.min = min(Gross),
    rate.min = min(rate)
  )

head.labels <-c("Czech", "UK", "France", "Norway", "Germany")

plot.labels$Gross <- plot.labels$Gross.max
plot.labels$rate <- plot.labels$rate.max

plot.labels$Gross[plot.labels$Country %in% head.labels] <- plot.labels$Gross.min[plot.labels$Country %in% head.labels]
plot.labels$rate[plot.labels$Country %in% head.labels] <- plot.labels$rate.min[plot.labels$Country %in% head.labels]



p1 <- ggplot(plot.taxes, 
              aes(x = Gross, y = rate*100, colour = Country, shape = Country)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_x_continuous(#limits = c(10000,130000),
                     breaks = seq(0,200000, 10000)) +
  scale_y_continuous(#limits = c(15,54),
                     breaks = seq(0,100,5)) +
  labs(title = "",
       x = "Gross Salary EUR",
       y = "Effective rate [%]",
       colour = "",
       shape = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_label_repel(
    data = plot.labels,
    aes(label = Country,
        x = Gross, y = rate*100 ),
    fontface = 'bold',  size = 4, # color = 'white',
#     nudge_x = 0,
#     nudge_y = 0,
    box.padding = unit(0.25, "lines"),
    point.padding = unit(0.5, "lines")
    #segment.color = NA
  ) 
p1

#

ggsave("EU-taxes.png", p1, width = 8, height = 7, dpi = 300)
#ggsave("export-pdf/taxes-rate-eur.pdf", p1, width = 8, height = 7)

# ##################
# 
# taxes.sub <- subset(taxes.all, Country %in% c("California", "UK", "Germany"))
# taxes.sub[,c( "Net","Gross")] <- 
#   taxes.sub[,c("Net","Gross")] * currencies[as.character(taxes.sub$Currency),"USD"]
# 
# 
# p2 <- ggplot(taxes.sub, 
#        aes(x = Gross, y = rate*100, colour = Country, shape = Country)) +
#   geom_line(size = 1.5) +
#   geom_point(size = 4) +
# #   scale_x_continuous(limits = c(0,130000),
# #                      breaks = seq(0,200000, 10000)) +
# #   scale_y_continuous(limits = c(12,46),
# #                      breaks = seq(0,100,5)) +
#   labs(title = "",
#        x = "Gross Salary USD",
#        y = "Effective rate [%]",
#        colour = "",
#        shape = "") +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw(base_size = 14) +
#   theme(legend.position = "bottom")
# #p2
# 
# #ggsave("export-png/taxes-rate-usd.png", p2, width = 8, height = 7, dpi = 300)
# #ggsave("export-pdf/taxes-rate-usd.pdf", p2, width = 8, height = 7)
# 






