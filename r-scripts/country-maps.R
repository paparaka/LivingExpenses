#library(ggmap)


Plot.Expenses.Maps <- function(expenses,this.country = "California", this.center = "California", this.zoom = 6) {
  
  map <- get_map(location = this.center, 
                 zoom = this.zoom, maptype = "terrain", source = "google", col="color")
  
  p <- ggmap(map, extent = 'device') +
    geom_point(data = expenses[expenses$Country == this.country,], 
               aes(x = lon, y = lat, size = Expenses.Sum), 
               alpha = 0.8,
               color = "#2c7fb8") +
    scale_size(range = c(5, 15)) +
    theme(legend.position = "bottom")  +
    scale_colour_brewer(palette = "Set1") 
  
  return(p)
}
#Plot.Expenses.Maps()

plot.Maps <- function (expenses) {
  require(ggmap)
  
  foo <- geocode(paste(as.character(expenses$City), as.character(expenses$Country)) )
  expenses$lon <- foo$lon
  expenses$lat <- foo$lat
  
  my.maps <- data.frame(Country = c("California","Germany","Japan","UK","Netherlands", "Belgium","Switzerland"),
                        center = c("California","Germany","Japan","Manchester","Netherlands", "Belgium","Switzerland"),
                        zoom = c(rep(6,4),8,7,7),
                        stringsAsFactors = FALSE)
  
  
  for (i in 1:nrow(my.maps)) {
    
    #TODO add try,catch for preventing interruption after errors
    
    p <- Plot.Expenses.Maps(expenses,my.maps$Country[i],my.maps$center[i],my.maps$zoom[i])
    
    ggsave(paste0("export-png/numbeo-map-",my.maps$Country[i],".png"),p, width = 9, height = 9, dpi = 300)
    ggsave(paste0("export-pdf/numbeo-map-",my.maps$Country[i],".pdf"),p, width = 9, height = 9)
  
  }
}

plot.Maps(expenses)