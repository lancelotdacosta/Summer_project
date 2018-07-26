
#Prepares fluxes
#Outputs a datframe of fluxes from the big dataframe that gives cell count

#WARNING: make sure plyr is installed before dplyr !!!!!! Otherwise useful dplyr functions are overwritten
library(plyr) #actually, need to load this library before dplyr otherwise this causes problems
#plyr is needed for the function rbind.fill that binds a list of dataframes together

#loads the big data frame
load("/Users/lancelotdacosta/CMP project/ABR07/cell_count_full_ABR07.RData")

list_of_dataframes <- vector("list", length(unique(df_improved$Tree))*length(unique(df_improved$RF))) #we again assume that all trees are always sampled with the same amount of radial files
i <- 1 #a counter

for(tree in unique(df_improved$Tree)) {
  for(rf in unique(df_improved$RF)) {
    
    flux_tree_rf <- filter(df_improved, Tree == tree, RF ==rf) # subsets big data frame by cell count data tree and radial file
    
    #We copy cell count data. We use which() instead of select() so that the output is a vector not a dataframe
    CZ <- flux_tree_rf[, which(names(flux_tree_rf)=="CZ")]
    EZ <- flux_tree_rf[, which(names(flux_tree_rf)=="EZ")]
    WZ <- flux_tree_rf[, which(names(flux_tree_rf)=="WZ")]
    MZ <- flux_tree_rf[, which(names(flux_tree_rf)=="MZ")]
    
    #computes the change of XZ at each day of the year and stores it in Delta_XZ
    CZ_copy <- CZ
    EZ_copy <- EZ
    WZ_copy <- WZ
    MZ_copy <- MZ
    
    CZ <- CZ[2:length(CZ)] #removes the first element of the vector
    EZ <- EZ[2:length(EZ)]
    WZ <- WZ[2:length(WZ)]
    MZ <- MZ[2:length(MZ)]
    
    CZ <- c(CZ, CZ[length(CZ)]) #adds a copy of the last element at the end of the vector
    EZ <- c(EZ, EZ[length(EZ)])
    WZ <- c(WZ, WZ[length(WZ)])
    MZ <- c(MZ, MZ[length(MZ)])
    
    CZ_copy <- CZ_copy[1:(length(CZ_copy)-1)] #removes the last element of the vector
    EZ_copy <- EZ_copy[1:(length(EZ_copy)-1)]
    WZ_copy <- WZ_copy[1:(length(WZ_copy)-1)]
    MZ_copy <- MZ_copy[1:(length(MZ_copy)-1)]
    
    CZ_copy <- c(CZ_copy[1], CZ_copy) #adds a copy of the first element at the beginning of the vector
    EZ_copy <- c(EZ_copy[1], EZ_copy)
    WZ_copy <- c(WZ_copy[1], WZ_copy)
    MZ_copy <- c(MZ_copy[1], MZ_copy)
    
    #this is the change in XZ cells at each day of the year
    Delta_CZ <- (CZ - CZ_copy)/2
    Delta_EZ <- (EZ - EZ_copy)/2
    Delta_WZ <- (WZ - WZ_copy)/2
    Delta_MZ <- (MZ - MZ_copy)/2
    
    flux_tree_rf <- mutate(flux_tree_rf, #changes the data frame to be the dataframe of fluxes with Tree tree and RF rf
                           CZ = Delta_CZ + Delta_EZ +Delta_WZ +Delta_MZ, #the fluxes follow these mathematical equations
                           EZ = Delta_EZ +Delta_WZ +Delta_MZ,
                           WZ = Delta_WZ + Delta_MZ,
                           MZ = Delta_MZ)
    
    list_of_dataframes[[i]] <- flux_tree_rf
    i <- i +1
  }
}


flux <- rbind.fill(list_of_dataframes)

#changes names of the columns
names(flux)[names(flux)=="CZ"] <- "->CZ"
names(flux)[names(flux)=="EZ"] <- "CZ->EZ"
names(flux)[names(flux)=="WZ"] <- "EZ->WZ"
names(flux)[names(flux)=="MZ"] <- "WZ->MZ"


for(tree in unique(df_improved_improved$Tree)) {
  for(rf in unique(df_improved$RF)) {
    
    flux_tree_rf <- filter(df_improved, Tree == tree, RF ==rf) # subsets big data frame by cell count data tree and radial file

    #We copy cell count data. We use which() instead of select() so that the output is a vector not a dataframe
    CZ <- flux_tree_rf[, which(names(flux_tree_rf)=="CZ")]
    EZ <- flux_tree_rf[, which(names(flux_tree_rf)=="EZ")]
    WZ <- flux_tree_rf[, which(names(flux_tree_rf)=="WZ")]
    MZ <- flux_tree_rf[, which(names(flux_tree_rf)=="MZ")]
    
    #computes the change of XZ at each day of the year and stores it in Delta_XZ
    CZ_copy <- CZ
    EZ_copy <- EZ
    WZ_copy <- WZ
    MZ_copy <- MZ
    
    CZ <- CZ[2:length(CZ)] #removes the first element of the vector
    EZ <- EZ[2:length(EZ)]
    WZ <- WZ[2:length(WZ)]
    MZ <- MZ[2:length(MZ)]
    
    CZ <- c(CZ, CZ[length(CZ)]) #adds a copy of the last element at the end of the vector
    EZ <- c(EZ, EZ[length(EZ)])
    WZ <- c(WZ, WZ[length(WZ)])
    MZ <- c(MZ, MZ[length(MZ)])
    
    CZ_copy <- CZ_copy[1:(length(CZ_copy)-1)] #removes the last element of the vector
    EZ_copy <- EZ_copy[1:(length(EZ_copy)-1)]
    WZ_copy <- WZ_copy[1:(length(WZ_copy)-1)]
    MZ_copy <- MZ_copy[1:(length(MZ_copy)-1)]
    
    CZ_copy <- c(CZ_copy[1], CZ_copy) #adds a copy of the first element at the beginning of the vector
    EZ_copy <- c(EZ_copy[1], EZ_copy)
    WZ_copy <- c(WZ_copy[1], WZ_copy)
    MZ_copy <- c(MZ_copy[1], MZ_copy)
    
    #this is the change in XZ cells at each day of the year
    Delta_CZ <- (CZ - CZ_copy)/2
    Delta_EZ <- (EZ - EZ_copy)/2
    Delta_WZ <- (WZ - WZ_copy)/2
    Delta_MZ <- (MZ - MZ_copy)/2
    
    flux_tree_rf <- mutate(flux_tree_rf, #changes the data frame to be the dataframe of fluxes with Tree tree and RF rf
                           CZ = Delta_CZ + Delta_EZ +Delta_WZ +Delta_MZ, #the fluxes follow these mathematical equations
                           EZ = Delta_EZ +Delta_WZ +Delta_MZ,
                           WZ = Delta_WZ + Delta_MZ,
                           MZ = Delta_MZ)
   
     list_of_dataframes[[i]] <- flux_tree_rf
    i <- i +1
  }
}


flux <- rbind.fill(list_of_dataframes)

#changes names of the columns
names(flux)[names(flux)=="CZ"] <- "->CZ"
names(flux)[names(flux)=="EZ"] <- "CZ->EZ"
names(flux)[names(flux)=="WZ"] <- "EZ->WZ"
names(flux)[names(flux)=="MZ"] <- "WZ->MZ"

#save the fluxes dataframe
save(flux, file = "fluxes_ABR07.RData")
