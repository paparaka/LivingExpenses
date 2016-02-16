#source("r-scripts/init.R")

expenses <- read.csv("import-tables/expenses.csv", header = TRUE, sep = ",")
str(expenses)

expenses.plot <- data.frame(vars = c("SUM","Rent","Utilities","Food","Transport"),
                            names = c("Basic living expenses","Rent","Utilities","Food expenses","Commute expenses"),
                            stringsAsFactors = FALSE
)

expenses[,expenses.plot$vars] <- 
  expenses[,expenses.plot$vars] * currencies[as.character(expenses$Currency),"USD"]
expenses$Currency <- "USD"

###########################
expenses.comon.plot <- function(p, this.legend = "bottom") {
  
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

expenses.bar.plot <- function(this.var, this.name) {
  
  expenses$City <- factor(expenses$City, levels = expenses$City[order(expenses[,this.var], decreasing = T)])
  
  p <- ggplot(expenses, aes_string(x = "City", y = this.var, fill = "Country")) + 
    geom_bar(stat = "identity") +
    labs(title = paste("Estimated monthly",this.name,"(USD)"))+
    scale_y_continuous(expand = c(0,0))
  
  p <- expenses.comon.plot(p)
  
  return(p)
}

expenses.box.plot <- function(this.var, this.name) {
  
  p <- ggplot(expenses, aes_string(x = "Country", y = this.var, fill = "Country")) + 
    geom_boxplot()+
    #geom_jitter(width = 0.2) +
    geom_text_repel(aes(label = City), arrow = NULL) +
    labs(title = paste("Estimated monthly",this.name,"(USD)"))
  
  p <- expenses.comon.plot(p, "none")
  
  return(p)
}


for (i in 1:nrow(expenses.plot)) {
  
  p1 <- expenses.bar.plot(expenses.plot$vars[i],expenses.plot$names[i])
  p2 <- expenses.box.plot(expenses.plot$vars[i],expenses.plot$names[i])
  
  ggsave(paste0("export-png/numbeo-bar-",expenses.plot$names[i],".png"),p1, width = 12, height = 9, dpi = 300)
  ggsave(paste0("export-pdf/numbeo-bar-",expenses.plot$names[i],".pdf"),p1, width = 12, height = 9)
  ggsave(paste0("export-png/numbeo-box-",expenses.plot$names[i],".png"),p2, width = 12, height = 9, dpi = 300)
  ggsave(paste0("export-pdf/numbeo-box-",expenses.plot$names[i],".pdf"),p2, width = 12, height = 9)
}


