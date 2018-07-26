##depends: dplyr, igraph, TDAmapper, fastcluster

load("large_matrix_ABR07.RData")
dataTDA <- select(df, annee, jour, vent, pluie, tsec, hum, rgl, tmin, tmax, Tree, CZ, EZ, WZ, MZ) #selects the columns I need for TDA
dataTDA$jour <- as.numeric(dataTDA$jour)
dist_mx <- dist(dataTDA)


####MAPPER 1D
#map1 <- function(d, lens, string = ""){
#m <- mapper1D(d,lens)
#g <- graph.adjacency(m$adjacency, mode="undirected")
#plot(g, layout = layout.auto(g), main = string)
#}

#lens1 <- dataTDA$vent  + dataTDA$pluie + dataTDA$tsec + dataTDA$hum + dataTDA$rgl + dataTDA$CZ + dataTDA$EZ + dataTDA$WZ+ dataTDA$MZ
#string1 <- "mean(vent, pluie, tsec, hum, rgl, CZ, EZ, WZ, MZ)"
#map1(distance, lens1, string1)

####Return average value, max value or min value over x days

meandata <- function(data, days){

avgdata <- data
  
for(r in 2:( nrow(avgdata))){
  
  if(r <= days) {
    for(c in 1: ncol(avgdata)) {
      avgdata[r,c] = mean(data[1:(r-1),c])
    }
  } else{
    for(c in 1: ncol(avgdata)) {
      avgdata[r,c] = mean(data[(r-days):(r-1),c])}
  }
}
avgdata
}

maxdata <- function(data, days){
  
  maxdata <- data
  
  for(r in 2:( nrow(maxdata))){
    
    if(r <= days) {
      for(c in 1: ncol(maxdata)) {
        maxdata[r,c] = max(data[1:(r-1),c])
      }
    } else{
      for(c in 1: ncol(maxdata)) {
        maxdata[r,c] = max(data[(r-days):(r-1),c])}
    }
  }
  maxdata
}

mindata <- function(data, days){
  
  mindata <- data
  
  for(r in 2:( nrow(mindata))){
    
    if(r <= days) {
      for(c in 1: ncol(mindata)) {
        mindata[r,c] = max(data[1:(r-1),c])
      }
    } else{
      for(c in 1: ncol(mindata)) {
        mindata[r,c] = max(data[(r-days):(r-1),c])}
    }
  }
  mindata
}


####MAPPER 2D
map2 <- function(dist_mx, lens, label = list("",""), overlap = 50){
  m <- mapper2D(dist_mx, lens, percent_overlap = overlap)
  g <- graph.adjacency(m$adjacency, mode="undirected")
  plot(g, layout = layout.auto(g), main = paste(label[[1]], label[[2]], sep = " , "), sub = paste("Percent overlap", as.character(overlap), sep = " : "))
}


####Code Mapper 2D
#pdf(file="Meandata1wpluie")
#i <- 1

#m1w <- meandata(dataTDA, 7)

#######Lens n######
#dist_mx <- dist(m1w)
lens <- list(dataTDA$pluie, dataTDA$CZ)
label <- list("pluie","CZ")
#overlap <- 30
map2(dist_mx, lens, label)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m1w$pluie, dataTDA$EZ)
label <- list("avgpluie1w","EZ")
#overlap <- 30
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m1w$pluie, dataTDA$WZ)
label <- list("avgpluie1w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m1w$pluie, dataTDA$MZ)
label <- list("avgpluie1w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()



pdf(file = "meandata2wpluie")
m2w <- meandata(dataTDA, 14)

#######Lens n######
dist_mx <- dist(m2w)
lens <- list(m2w$pluie, dataTDA$CZ)
label <- list("avgpluie2w","CZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m2w$pluie, dataTDA$EZ)
label <- list("avgpluie2w","EZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m2w$pluie, dataTDA$WZ)
label <- list("avgpluie2w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m2w$pluie, dataTDA$MZ)
label <- list("avgpluie2w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()


pdf(file= "meandata3wpluie")

m3w <- meandata(dataTDA, 21)


#######Lens n######
dist_mx <- dist(m3w)
lens <- list(m3w$pluie, dataTDA$CZ)
label <- list("avgpluie3w","CZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m3w$pluie, dataTDA$EZ)
label <- list("avgpluie3w","EZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m3w$pluie, dataTDA$WZ)
label <- list("avgpluie3w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m3w$pluie, dataTDA$MZ)
label <- list("avgpluie3w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()

pdf(file = "meandata4wpluie")

m4w <- meandata(dataTDA, 28)

#######Lens n######
dist_mx <- dist(m4w)
lens <- list(m4w$pluie, dataTDA$CZ)
label <- list("avgpluie4w","CZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m4w$pluie, dataTDA$EZ)
label <- list("avgpluie4w","EZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m4w$pluie, dataTDA$WZ)
label <- list("avgpluie4w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m4w$pluie, dataTDA$MZ)
label <- list("avgpluie4w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()

#16 so far
####influence of overlap on avgpluie2w | CZ EZ WZ MZ

dist_mx <- dist(m2w)


pdf(file = "CZ")
lens <- list(m2w$pluie, dataTDA$CZ)
label <- list("avgpluie2w","CZ")
for(o in seq(10, 90, 10)){
  overlap <- o
  map2(dist_mx, lens, label, overlap)
  message(i)
  message("DONE!")
  i <- i+1
}
dev.off()

pdf(file = "EZ")
lens <- list(m2w$pluie, dataTDA$EZ)
label <- list("avgpluie2w","EZ")
for(o in seq(10, 90, 10)){
  overlap <- o
  map2(dist_mx, lens, label, overlap)
  message(i)
  message("DONE!")
  i <- i+1
}
dev.off()

pdf(file = "WZ")
lens <- list(m2w$pluie, dataTDA$WZ)
label <- list("avgpluie2w","WZ")
for(o in seq(10, 90, 10)){
  overlap <- o
  map2(dist_mx, lens, label, overlap)
  message(i)
  message("DONE!")
  i <- i+1
}
dev.off()

pdf(file = "MZ")
lens <- list(m2w$pluie, dataTDA$MZ)
label <- list("avgpluie2w","MZ")
for(o in seq(10, 90, 10)){
  overlap <- o
  map2(dist_mx, lens, label, overlap)
  message(i)
  message("DONE!")
  i <- i+1
}
dev.off()

#52 lenses so far


pdf(file="Meandata1wtsec")


#######Lens n######
dist_mx <- dist(m1w)
lens <- list(m1w$tsec, dataTDA$CZ)
label <- list("avgtsec1w","CZ")
overlap <- 30
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m1w$tsec, dataTDA$EZ)
label <- list("avgtsec1w","EZ")
#overlap <- 30
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m1w$tsec, dataTDA$WZ)
label <- list("avgtsec1w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m1w$tsec, dataTDA$MZ)
label <- list("avgtsec1w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()



pdf(file = "meandata2wtsec")

#######Lens n######
dist_mx <- dist(m2w)
lens <- list(m2w$tsec, dataTDA$CZ)
label <- list("avgtsec2w","CZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m2w$tsec, dataTDA$EZ)
label <- list("avgtsec2w","EZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m2w$tsec, dataTDA$WZ)
label <- list("avgtsec2w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m2w$tsec, dataTDA$MZ)
label <- list("avgtsec2w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()


pdf(file= "meandata3wtsec")


#######Lens n######
dist_mx <- dist(m3w)
lens <- list(m3w$tsec, dataTDA$CZ)
label <- list("avgtsec3w","CZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m3w$tsec, dataTDA$EZ)
label <- list("avgtsec3w","EZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m3w$tsec, dataTDA$WZ)
label <- list("avgtsec3w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m3w$tsec, dataTDA$MZ)
label <- list("avgtsec3w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()

pdf(file = "meandata4wtsec")


#######Lens n######
dist_mx <- dist(m4w)
lens <- list(m4w$tsec, dataTDA$CZ)
label <- list("avgtsec4w","CZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m4w$tsec, dataTDA$EZ)
label <- list("avgtsec4w","EZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m4w$tsec, dataTDA$WZ)
label <- list("avgtsec4w","WZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1



#######Lens n######
#dist_mx <- dist()
lens <- list(m4w$tsec, dataTDA$MZ)
label <- list("avgtsec4w","MZ")
#overlap <- #
map2(dist_mx, lens, label, overlap)
message(i)
message("DONE!")
i <- i+1

dev.off()

###68 lenses so far


