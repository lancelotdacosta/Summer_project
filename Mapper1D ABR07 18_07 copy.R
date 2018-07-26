#this code relies on the big data frame of cell count data
#if we reuse it need to to not use 

library("dplyr")
library("igraph")
library("TDAmapper")
library("fastcluster")

#FUNCTIONS

#returns the mean of a dataframe
meandata <- function(data, days){
  
  avgdata <- data
  
  for(r in 2:(nrow(avgdata))){
    
    if(r <= days) {
      for(c in 3: ncol(avgdata)) {
        avgdata[r,c] = mean(data[1:(r-1),c])
      }
    } else{
      for(c in 3: ncol(avgdata)) {
        avgdata[r,c] = mean(data[(r-days):(r-1),c])}
    }
  }
  avgdata
}
#left to do: min, max


#returns mean of meteorological data of dataTDA
meanTDA <- function(days){
  mean_dataTDA <- dataTDA
  
  if(days !=0) { #if days=0 does not change data
    mean_meteo_ABR07 <- meandata(meteo_ABR07, days)
    
    for(jour in unique(mean_meteo_ABR07$jour)){
      
      mean_dataTDA[which(mean_dataTDA$jour == jour),3:9] <- mean_meteo_ABR07[which(mean_meteo_ABR07$jour == jour),3:9]
      
    }
  }
  mean_dataTDA$annee <- NULL
  mean_dataTDA$jour <- NULL
  mean_dataTDA$Tree <- NULL
  
  mean_dataTDA
}

#CODE

#load large dataframe
load("large_matrix_ABR07.RData")

#prepare data for TDA
dataTDA <- select(df, annee, jour, vent, pluie, tsec, hum, rgl, tmin, tmax, Tree, CZ, EZ, WZ, MZ) #selects the columns I need for TDA
dataTDA$jour <- as.numeric(dataTDA$jour)

#load meteorological data
meteo_ABR <- read.delim("/Users/lancelotdacosta/Desktop/Data/Meteorological data/DonneesMeteoJournalieres_Abreschviller_AE.txt")
meteo_ABR07 <- filter(meteo_ABR, annee == 2007)


#MAPPER1D

map1 <- function(dist_mx, 
                 lens,
                 label = c("Data","Lens"),
                 intervals = 10,
                 overlap = 50,
                 bins = 10) {
  #mapper1D function
  m1 <- mapper1D(
    distance_matrix = dist_mx,
    filter_values = lens,
    num_intervals = intervals,
    percent_overlap = overlap,
    num_bins_when_clustering = bins)
  
  g1 <- graph.adjacency(m1$adjacency, mode="undirected")
  
  #prepration of plot subtitle
  i <- paste("Intervals: ", as.character(intervals))
  j <- paste("overlap: ", as.character(overlap))
  k <- paste("bins w clustering: ", as.character(bins))
  
  #plot of graph
  plot(g1, layout = layout.auto(g1), sub = paste(c(i,j,k), collapse = ", "))
  
  #plot title and text
  mtext(paste("Data: ", label[1])) #Data
  title(paste("Lens: ", label[2])) #Lens
  
  m1
}


#####LENSES##########################

#mapper1D for meteorological means of 0 until 30 days,
#projection onto each coordinate axis, with overlaps from 0 to 90
#1386 lenses

i <- 1 #lens counter
for(days in seq(0,30,3)){
  data <- meanTDA(days)
  data_name <- paste(c("Meteorology mean over", as.character(days), "days"), collapse = " ")
  dist_mx <- dist(data)
  
  for(col in 2:ncol(data)){
    lens <- data[,col]
    lens_name <- names(data)[col]
    label <- c(data_name, lens_name)
    
    for(overlap in seq(10,90,10)){
      map1(dist_mx, lens, label, overlap = overlap)  
      message(i)
      i <- i +1
    }
  }
}