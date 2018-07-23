#################FUNCTIONS ABR_2007
#Function: What are the missing values?
getnas <- function(d){
  a <- d[1,]
  
  for(i in 1:nrow(d)){
    
    x <- d[i,]
    
    for(j in 1:length(x)){
      if(is.na(x[j])){
        
        a = rbind(a, x)
      }
      
    }
  }
  a = ABR_07_NA[2:nrow(a),]
  a
}



####Plot a species
plotspecies <- function(n)
{
  d <- filter(df, Species ==as.character(n))
  plot(d$jour, d$MZ, type = "l")
  lines(d$jour,d$CZ,type = "l", col= "green")
  lines(d$jour,d$EZ,type = "l", col= "orange")
  lines(d$jour,d$WZ,type = "l", col= "red")
}
