
Expenses.Comon.Plot <- function(p, this.legend = "bottom") {
  
  #cols <- c("Japan" = "#d7191c","UK" = "#2c7bb6","USA" = "#7b3294", "Germany" = "#fdae61")
  
  p <- p  +
    theme_bw(base_size = 16) +
    theme(legend.position = this.legend,
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1)) +
    labs(
      x = "",
      y = " ",
      fill = " ") +
    scale_fill_brewer(palette = "Set1") 
  #scale_fill_manual(values = cols)
  
  
  return (p)
}

# Expenses.Bar.Plot <- function(this.var, this.name) {
#   
#   expenses$City <- factor(expenses$City, levels = expenses$City[order(expenses[,this.var], decreasing = T)])
#   
#   p <- ggplot(expenses, aes_string(x = "City", y = this.var, fill = "Country")) + 
#     geom_bar(stat = "identity") +
#     labs(title = paste("Estimated monthly",this.name,"(USD)"))+
#     scale_y_continuous(expand = c(0,0))
#   
#   p <- Expenses.Comon.Plot(p)
#   
#   return(p)
# }

Expenses.Box.Plot  <- function(expenses, this.var, this.name, this.currency) {
  
  p <- ggplot(expenses, aes_string(x = "Country", y = this.var, fill = "Country")) + 
    geom_boxplot()+
    #geom_jitter(width = 0.2) +
    geom_text_repel(aes(label = City), arrow = NULL) +
    labs(title = paste("Estimated ",this.name,"(",this.currency,")"))
  
  p <- Expenses.Comon.Plot(p, "none")
  
  return(p)
}

Expenses.Load.n.PLot <- function(filename = "import-tables/expenses.csv", currency = "USD") {
  
  expenses <- read.csv(filename, header = TRUE, sep = ",")
  # str(expenses)
  
  expenses.plot <- data.frame(vars = c("SUM","Rent","Utilities","Food","Transport",
                                       "Meal.Inexpensive", "Beer", "Cappucino"),
                              names = c("monthly Basic living expenses","monthly Rent","monthly Utilities","monthly Food expenses","monthly Commute expenses", 
                                        "Meal Inexpensive", "Beer Draft", "Cappucino"),
                              stringsAsFactors = FALSE
  )
  
  expenses[,expenses.plot$vars] <- 
    expenses[,expenses.plot$vars] * currencies[as.character(expenses$Currency), currency]
  expenses$Currency <- currency
  
  #TODO implement a try() catch() method to skip errors
  
  for (i in 1:nrow(expenses.plot)) {
    
    #p1 <- Expenses.Bar.Plot(expenses.plot$vars[i],expenses.plot$names[i])
    p2 <- Expenses.Box.Plot (expenses, expenses.plot$vars[i],expenses.plot$names[i],currency)
    
    # ggsave(paste0("export-png/numbeo-bar-",expenses.plot$names[i],".png"),p1, width = 12, height = 9, dpi = 300)
    # ggsave(paste0("export-pdf/numbeo-bar-",expenses.plot$names[i],".pdf"),p1, width = 12, height = 9)
    ggsave(paste0("export-png/numbeo-box-",expenses.plot$names[i],".png"),p2, width = 12, height = 9, dpi = 300)
    ggsave(paste0("export-pdf/numbeo-box-",expenses.plot$names[i],".pdf"),p2, width = 12, height = 9)
  }
  
  return(expenses)
}





