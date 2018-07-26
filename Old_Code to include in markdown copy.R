###code to include in the markdown

```{r main, include= FALSE}
#STEP1#
#Add samples where missing samples

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

#STEP2#
#Replace NAs in cell count data

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

#multiply each row by (no of trees * no of radial files per sample)
df <- meteo_ABR_07[rep(1:nrow(meteo_ABR_07), each=length(unique(ABR_07$Tree))*length(unique(ABR_07$RF))),]

#add columns to df to later insert the cell count data there
df <- mutate(df, Site= "", Year =0, Species = "", Tree =0, Sample = 0, DY=0, RF=0, CZ=0,EZ=0,WZ=0,MZ=0,PR=0)

#Converts the trees from Factor to Numeric to be able to sort the data frame by trees in ascending order
ABR_07$Tree <- as.numeric(as.character(ABR_07$Tree))
ABR_07$RF <- as.numeric(as.character(ABR_07$RF))
ABR_07$Species <- as.character(ABR_07$Species)
ABR_07$Site <- as.character(ABR_07$Site)
ABR_07$RF <- as.numeric(as.character(ABR_07$RF))

#Sorts ABR_07 by Tree and Sample number
ABR_07 <- ABR_07[order(ABR_07$Tree),]
ABR_07 <- ABR_07[order(ABR_07$Sample),]

###Import ABR_07 data into data frame df
for(i in 1:nrow(ABR_07)){
  Y <- ABR_07$Year[i]
  D <- ABR_07$DY[i]
  T <- ABR_07$Tree[i] - min(ABR_07$Tree) +1
  R <- ABR_07$RF[i]
  
  Row_to_be_modified <- which(df$annee == Y & df$jour == D)[1] + (T-1)*3 + R-1
  df[ Row_to_be_modified , (ncol(df)-ncol(ABR_07)+1):ncol(df)] <- ABR_07[i,]
}


###We enter the relevant Trees, species are radial file at each line of df
Trees <- unique(ABR_07$Tree)        #list of trees in ABR_07
n_radialfiles <- length(unique(ABR_07$RF)) #number of radial files per sample
i <- 1      #some counter
t <- 1      #some other counter
Site <- ABR_07$Site[1] #Site: eg. ABR
for(row_number in 1:nrow(df)){
  
  if(df$Sample[row_number] !=0) 
  {row_number <- row_number + length(Trees)*n_radialfiles-1}
  
  else{
    df$Site <- Site       # assumes we are treating a cell count data (eg. ABR_07) from only one site
    df$RF[row_number]= i #sets the radial file number
    df$Species[row_number] = ABR_07$Species[which(ABR_07$Tree == Trees[t])[1]] #sets the species corresponding to the tree
    df$Tree[row_number] = Trees[t]
    
    if(i == n_radialfiles){i <- 1
    
    if(t == length(Trees)) {t = 1} 
    else{t = t+1}
    
    } else{i <- i+1}
    
  }
}
rm(i) #don't delete the variable Trees, we need it later
rm(t)
rm(n_radialfiles)
rm(Site) 


df$Year <- NULL ### Deletes df$Year as we already have the info in df$annee
df$DY <- NULL #### Deletes df$DY as we already have the info in df$jour


#####Linear interpolation

#We interpolate cambial cells before the first sample and cambial and mature cells after the last sample
Sample1 <- filter(df, Sample ==1)
dayfirstsample <- df$jour[which(df$Sample==1)[1]]

Sample31 <- filter(df, Sample ==max(df$Sample))
daylastsample <- df$jour[which(df$Sample==max(df$Sample))[1]]

for(T in Trees){
  for(R in unique(df$RF)){
    df$CZ[which(df$jour < dayfirstsample & df$Tree == T & df$RF == R)] <- Sample1$CZ[which(Sample1$Tree ==T & Sample1$RF == R)[1]]
    df$CZ[which(df$jour > daylastsample & df$Tree == T & df$RF == R)] <- Sample31$CZ[which(Sample31$Tree ==T & Sample31$RF == R)[1]]
    df$MZ[which(df$jour > daylastsample & df$Tree == T & df$RF == R)] <- Sample31$MZ[which(Sample31$Tree ==T & Sample31$RF == R)[1]]
  }
}
rm(dayfirstsample)
rm(daylastsample)
rm(Sample31)


#We do linear interpolation in between the samples
Sample_i_plus_1 <- Sample1
rm(Sample1)

for(sample_no in 1:(max(df$Sample)-1)){
  
  first_row <- which(df$Sample == sample_no & df$Tree == max(df$Tree) & df$RF == max(df$RF))[1] +1
  #(last observation for the i-th sample) +1
  
  last_row <- which(df$Sample == sample_no +1 & df$Tree == min(df$Tree) & df$RF == min(df$RF))[1] -1
  #(first observation for the (i+1)-th sample) -1
  
  Sample_i <- Sample_i_plus_1
  Sample_i_plus_1 <- filter(df, Sample == (sample_no + 1))
  
  Day_sample_i <- Sample_i$jour[1]
  Day_sample_i_plus_1 <- Sample_i_plus_1$jour[1]
  
  for(row_number in first_row : last_row){
    
    day <- df$jour[row_number]
    tree <- df$Tree[row_number]
    rf <- df$RF[row_number]
    
    f1 <- filter(Sample_i, Tree == tree, RF==rf )
    f2 <- filter(Sample_i_plus_1, Tree == tree, RF==rf )
    
    d1 <- Day_sample_i_plus_1-day
    d2 <- day-Day_sample_i
    d <- Day_sample_i_plus_1-Day_sample_i
    
    #linear interpolation
    df$CZ[row_number] <- (d1*f1$CZ[1] +d2*f2$CZ[1])/d
    df$EZ[row_number] <- (d1*f1$EZ[1] +d2*f2$EZ[1])/d
    df$WZ[row_number] <- (d1*f1$WZ[1] +d2*f2$WZ[1])/d
    df$MZ[row_number] <- (d1*f1$MZ[1] +d2*f2$MZ[1])/d
    
  }
}
rm(Sample_i_plus_1)
```