ggtree <- function(t){
  
  if(!(t %in% unique(df$Tree)) ){
    error_message <- paste(c("Tree", as.character(t), "is not in df"), collapse = ' ')
    stop(error_message)
  }
  ggtree <- filter(df, Tree == t) %>% ggplot(aes(x = jour)) +
    ggtitle(paste(c("Tree: ", as.character(t), ", Species: ", df$Species[which(df$Tree == t)[1]]), collapse= "")) +
    geom_point(aes(y = MZ, color = "Mature cells")) +
    geom_point(aes(y = WZ, color = "Wall-thickening cells")) +
    geom_point(aes(y = EZ, color = "Enlarging cells")) +
    geom_point(aes(y = CZ, color = "Cambial cells")) +
    scale_colour_manual("",
                        breaks = c("Cambial cells", "Enlarging cells", "Wall-thickening cells", "Mature cells"),
                        values = c("green", "yellow", "red", "orange")) +
    xlab("Day") + ylab("Cell count") +
    theme_fivethirtyeight()
  ggtree
}
