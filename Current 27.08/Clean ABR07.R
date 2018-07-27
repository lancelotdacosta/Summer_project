#clean ABR07 data

#load cell count data ABR 2007-2009
load("/Users/lancelotdacosta/Desktop/Data/Cell Count Data/Cellcounts_ABR_2007to2009.RData")

#set working directory
setwd("~/CMP project/ABR07")

##Extract year 2007
ABR_07 <- filter(ABR.raw.data, Year == 2007)

#STEP1################Add missing samples in cellular data
#So that each tree in ABR_07 has 31 samples with three redial files in each sample
#Tree 50 is missing samples 10 & 11 of year 2007
for(i in 1:3){
  for(j in 10:11) {
    NewRow <- list("ABR", 2007, "Abies alba", 50, j, 156 +(j-10)*7, i,NA,NA,NA,NA,NA)
    Index <- which(ABR_07$Tree == 50 & ABR_07$Sample ==9 & ABR_07$RF ==3) + i +(j-10)*i -1
    ABR_07 <- rbind(ABR_07[1:Index,],NewRow,ABR_07[-(1:Index),])
  }
}
str(ABR_07)

#Tree 44 is missing sample 24 of year 2007
for(i in 1:3){
  
  NewRow <- list("ABR", 2007, "Picea abies", 44, 24, 247+7, i,NA,NA,NA,NA,NA)
  Index <- which(ABR_07$Tree == 44 & ABR_07$Sample ==23 & ABR_07$RF ==3) + i -1
  ABR_07 <- rbind(ABR_07[1:Index,],NewRow,ABR_07[-(1:Index),])
}
rm(NewRow)
rm(Index)
#from now on each tree has the same number of samples and the same number of RF in each sample

#STEP2###############Replace missing values in cell count data when not all are NA
#We replace all the missing values in ABR_07 using the following algorithm
#If not all values are NA for a given cell, sample and tree, we assign the mean of the non-NA values to the NA value
#If all values are NA for a given cell, sample and tree, we do a linear interpolation on the previous available sample
#and next available sample

#replace the missing values in cambial cells
for(r in seq(1, nrow(ABR_07),3)){
  v <- c(ABR_07$CZ[r],ABR_07$CZ[r+1], ABR_07$CZ[r+2])
  v_na <- is.na(v)
  
  if(sum(v_na)>0 & sum(v_na)<3) {
    v[v_na]=mean(v[!v_na])
  }
  
  ABR_07$CZ[r] = v[1]
  ABR_07$CZ[r+1] = v[2]
  ABR_07$CZ[r+2] = v[3]
}

#replace the missing values in enlarging cells

for(r in seq(1, nrow(ABR_07),3)){
  v <- c(ABR_07$EZ[r],ABR_07$EZ[r+1], ABR_07$EZ[r+2])
  v_na <- is.na(v)
  
  if(sum(v_na)==1) {
    v[which(v_na)]=sum(v[which(!v_na)])/2
  }
  
  if(sum(v_na)==2) {
    v[which(v_na)]=v[which(!v_na)[1]]
  }
  
  ABR_07$EZ[r] = v[1]
  ABR_07$EZ[r+1] = v[2]
  ABR_07$EZ[r+2] = v[3]
}

#replace the missing values in wall-thickening cells
for(r in seq(1, nrow(ABR_07),3)){
  v <- c(ABR_07$WZ[r],ABR_07$WZ[r+1], ABR_07$WZ[r+2])
  v_na <- is.na(v)
  
  if(sum(v_na)==1) {
    v[which(v_na)]=sum(v[which(!v_na)])/2
  }
  
  if(sum(v_na)==2) {
    v[which(v_na)]=v[which(!v_na)[1]]
  }
  
  ABR_07$WZ[r] = v[1]
  ABR_07$WZ[r+1] = v[2]
  ABR_07$WZ[r+2] = v[3]
}

#replace the missing values in mature cells
for(r in seq(1, nrow(ABR_07),3)){
  v <- c(ABR_07$MZ[r],ABR_07$MZ[r+1], ABR_07$MZ[r+2])
  v_na <- is.na(v)
  
  if(sum(v_na)==1) { #If there is one NA we replace by the mean of the other two
    v[which(v_na)]=sum(v[which(!v_na)])/2
  }
  
  if(sum(v_na)==2) {#If there are two NAs we replace by the non-missing value
    v[which(v_na)]=v[which(!v_na)[1]]
  }
  
  #Note: if there are three NAs in a row we don't do anything (yet)
  ABR_07$MZ[r] = v[1]
  ABR_07$MZ[r+1] = v[2]
  ABR_07$MZ[r+2] = v[3]
}
rm(v_na)


#STEP3
#Where there are 3NAs in a row in cell count values,
#we replace them by linear interpolation between the next and previous sample where the data is available
#our algorithm relies on the fact (which we check in the next loop) that there are no "3 NAs in a row"
#in the first or last sample of the year.

#
for (s in 1: nrow(ABR_07)) {
  v <- ABR_07[s,1:ncol(ABR_07)-1]
  if(  sum(  is.na(   v)) >=1 & (ABR_07$Sample[s] %in% c(1,max(ABR_07$Sample))) ){
    warning("Error: the data set contains 3NAs in a row in either the first sample or the last sample of some tree.")
    warning("This happens at row:")
    error(s)}
}

#We do linear interpolation in the different type of cells

for(r in seq(1, nrow(ABR_07),3)){
  T <- ABR_07$Tree[r]
  S <- ABR_07$Sample[r]
  S1 <- S-1
  triplet1 <- filter(ABR_07, Tree==T, Sample == S1)
  
  #cambial cells
  if(is.na(ABR_07$CZ[r]))
  {
    triplet2 <- triplet1
    S2 <- S
    
    b <- TRUE
    while(b){
      S2 <- S2+1
      triplet2 <- filter(ABR_07, Tree==T, Sample == S2)
      b <- is.na(triplet2$CZ[1])
    }
    
    delta <- S2 - S1
    
    a <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S1 ),]$CZ
    b <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S2 ),]$CZ
    
    for (t in 1:delta-1) {
      ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S + t -1 ),]$CZ = (1-t/delta)*a + t/delta*b
    }
    
  }
  
  #Enlarging cells
  if(is.na(ABR_07$EZ[r]))
  {
    triplet2 <- triplet1
    S2 <- S
    
    b <- TRUE
    while(b){
      S2 <- S2+1
      triplet2 <- filter(ABR_07, Tree==T, Sample == S2)
      b <- is.na(triplet2$EZ[1])
    }
    
    delta <- S2 - S1
    
    a <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S1 ),]$EZ
    b <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S2 ),]$EZ
    
    for (t in 1:delta-1) {
      ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S + t -1 ),]$EZ = (1-t/delta)*a + t/delta*b
    }
    
  }
  #Wall thickening cells
  if(is.na(ABR_07$WZ[r]))
  {
    triplet2 <- triplet1
    S2 <- S
    
    b <- TRUE
    while(b){
      S2 <- S2+1
      triplet2 <- filter(ABR_07, Tree==T, Sample == S2)
      b <- is.na(triplet2$WZ[1])
    }
    
    delta <- S2 - S1
    
    a <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S1 ),]$WZ
    b <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S2 ),]$WZ
    
    for (t in 1:delta-1) {
      ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S + t -1 ),]$WZ = (1-t/delta)*a + t/delta*b
    }
    
  }
  #Mature cells
  if(is.na(ABR_07$MZ[r]))
  {
    triplet2 <- triplet1
    S2 <- S
    
    b <- TRUE
    while(b){
      S2 <- S2+1
      triplet2 <- filter(ABR_07, Tree==T, Sample == S2)
      b <- is.na(triplet2$MZ[1])
    }
    
    delta <- S2 - S1
    
    a <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S1 ),]$MZ
    b <- ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S2 ),]$MZ
    
    for (t in 1:delta-1) {
      ABR_07[which(ABR_07$Tree == T & ABR_07$Sample == S + t -1 ),]$MZ = (1-t/delta)*a + t/delta*b
    }
    
  }
}

#save our file
save(ABR_07, file = "ABR07_clean.RData")