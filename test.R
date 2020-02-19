tt %>% 
  ggplot() + 
  aes(x = 1, y = sejrea_dureesej) +
  geom_violin(fill = "lightblue", col= "blue") +
  geom_boxplot(width = 0.2)
  labs(title = "Durée de séjour",
       y = "jours"
  ) + 
  theme_light() + 
  theme(plot.title = element_text(size=16, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12, face="bold"),
        legend.position = "none") 
  
remp <- function(varx){
  lnx <- length(varx)
  lcx <- length(na.omit(varx))
  pcr <- round(100*lcx/lnx,1)
  return(pcr)
}