#Merge cell count data, meteorological data & anatomical data

#################Insert cell count data and meteorological data into a new dataframe df

#Loads cell count and meteorological data ABR07
load("/Users/lancelotdacosta/CMP project/ABR07/ABR07_clean.RData")
meteo_ABR <- read.delim("/Users/lancelotdacosta/Desktop/Data/Meteorological data/DonneesMeteoJournalieres_Abreschviller_AE.txt")

#extract year 2007
meteo_ABR_07 <- filter(meteo_ABR, annee == 2007)

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


###We enter the relevant Trees, species and radial file at each line of df
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


####Insert anatomical data into dataframe

#reads anatomical data for ABR07 (only the variables which we need)
anat_ABR07 <- read.csv(file = "/Users/lancelotdacosta/Desktop/Data/Anatomical data/TDG_ABR2007_Raw.csv", sep = ";")[ ,c('Year', 'Tree', 'PathName', 'CellRank')]

#record the mean number of mature cells for each tree at the end of the year
#Here we assume anatomical data is only from 1 year and df as well (2007 in the case of ABR07)

#Now we want to insert anatomical data as a 32nd sample in the data frame

day_last_sample <- df$jour[which(df$Sample == max(df$Sample))[1]] #how many times were the trees sampled on this site (assumes this is a constant number) e.g. it is 31 in ABR07
sampling_period <- df$jour[which(df$Sample == 2)[1]] - df$jour[which(df$Sample == 1)[1]] #day of second sample - day of first sample #gives 7 in ABR07 as these are sampled weekly
#we assume sampling has been done at regular intervals

for(tree in unique(anat_ABR07$Tree)){
  
  anat_tree <- filter(anat_ABR07, Tree == tree) 
  mature_cells <- vector(mode="numeric", length= length(unique(anat_tree$PathName)))
  counter <- 1
  
  for(pname in unique(anat_tree$PathName)){
    anat_tree_pname <- filter(anat_tree, PathName == pname)
    
    #takes the number of mature cells on each radial file of th anatomical data and puts it in the vector
    mature_cells[counter] <- max(anat_tree_pname$CellRank)
    counter <-  counter +1
  }

  #inserts as number of mature cells the mean of the anatomical data
  df$MZ[which(df$jour == day_last_sample + sampling_period & df$Tree == tree)] <- mean(mature_cells)
  
}
#adds the number to the last sample corresponding to anatomical data
df$Sample[which(df$jour == day_last_sample + sampling_period)] <- max(df$Sample) +1

#save this data frame and then interpolate

save(df, file = "cell_count_preinterpolated_ABR07.RData")
