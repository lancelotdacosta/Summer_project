#####Linear interpolation

load("/Users/lancelotdacosta/CMP project/ABR07/cell_count_preinterpolated_ABR07.RData")

#We interpolate cambial cells before the first sample and cambial and mature cells after the last sample
Sample1 <- filter(df, Sample ==1)
dayfirstsample <- df$jour[which(df$Sample==1)[1]]


Sample31 <- filter(df, Sample ==max(df$Sample)) #the last sample during the growing season
daylastsample <- df$jour[which(df$Sample==max(df$Sample-1))[1]]

Sample32 <- filter(df, Sample ==max(df$Sample)) #corresponding to the sample of anatomical data
daysample_anat_data <- df$jour[which(df$Sample==max(df$Sample-1))[1]] #day of sampling where we put the anatomical data

for(T in Trees){
  for(R in unique(df$RF)){
    #interpolate Cambial cells before the growing season
    df$CZ[which(df$jour < dayfirstsample & df$Tree == T & df$RF == R)] <- Sample1$CZ[which(Sample1$Tree ==T & Sample1$RF == R)[1]]
    #interpolate the cambial cells after the growing season
    df$CZ[which(df$jour > daylastsample & df$Tree == T & df$RF == R)] <- Sample31$CZ[which(Sample31$Tree ==T & Sample31$RF == R)[1]]
    #interpolate the mature cells after the anatomical data sample
    df$MZ[which(df$jour > daysample_anat_data & df$Tree == T & df$RF == R)] <- Sample31$MZ[which(Sample32$Tree ==T & Sample32$RF == R)[1]]
  }
}



#We do linear interpolation in between the samples
Sample_i_plus_1 <- Sample1

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

#replaces integers by numeric
df$jour <- as.numeric(df$jour)

save(df, file = "cell_count_ABR07.RData")