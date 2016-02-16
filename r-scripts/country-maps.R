library(ggmap)

foo <- geocode(as.character(expenses$City))
expenses$lon <- foo$lon
expenses$lat <- foo$lat

my.maps <- data.frame(Country = c("California","Germany","Japan","UK"),
                      center = c("California","Germany","Japan","Manchester"),
                      stringsAsFactors = FALSE)

Plot.Expenses.Maps <- function(this.country = "California") {
  
  map <- get_map(location = my.maps$center[my.maps$Country == this.country], 
                 zoom = 6, maptype = "terrain", source = "google", col="color")
  
  p <- ggmap(map, extent = 'device') +
    geom_point(data = expenses[expenses$Country == this.country,], 
               aes(x = lon, y = lat, size = SUM), 
               alpha = 0.8,
               color = "#2c7fb8") +
    scale_size(range = c(5, 15)) +
    theme(legend.position = "none")  +
    scale_colour_brewer(palette = "Set1") 
  
  return(p)
}
#Plot.Expenses.Maps()

for (i in 1:nrow(my.maps)) {
  
  p <- Plot.Expenses.Maps(my.maps$Country[i])
  
  ggsave(paste0("export-png/numbeo-map-",my.maps$Country[i],".png"),p, width = 9, height = 9, dpi = 300)
  ggsave(paste0("export-pdf/numbeo-map-",my.maps$Country[i],".pdf"),p, width = 9, height = 9)

}