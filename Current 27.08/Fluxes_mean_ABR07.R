#computes the flux mean

#loads the fluxes dataframe
load("/Users/lancelotdacosta/CMP project/ABR07/fluxes_ABR07.RData") #called flux


list_of_dataframes <- vector("list", length(unique(flux$Tree))*length(unique(flux$jour))) #an empty list
i <- 1 #a counter

for(tree in unique(flux$Tree)) {
  for(day in unique(flux$jour)) {
    
    flux_tree_day <- filter(flux, Tree == tree, jour == day) # subsets big data frame by cell count data tree and day
    
    flux1 <- flux_tree_day[, which(names(flux_tree_day)=="->CZ")]
    flux2 <- flux_tree_day[, which(names(flux_tree_day)=="CZ->EZ")]
    flux3 <- flux_tree_day[, which(names(flux_tree_day)=="EZ->WZ")]
    flux4 <- flux_tree_day[, which(names(flux_tree_day)=="WZ->MZ")]
    precision <- flux1 <- flux_tree_day[, which(names(flux_tree_day)=="PR")]
    
    tree_day <- flux_tree_day[1,]
    
    tree_day[, which(names(tree_day)=="->CZ")] <- mean(flux1)
    tree_day[, which(names(tree_day)=="CZ->EZ")] <- mean(flux2)
    tree_day[, which(names(tree_day)=="EZ->WZ")] <- mean(flux3)
    tree_day[, which(names(tree_day)=="WZ->MZ")] <- mean(flux4)
    tree_day[, which(names(tree_day)=="PR")] <- mean(precision)
    
    list_of_dataframes[[i]] <- tree_day
    i <- i +1
  }
}


flux_mean <- rbind.fill(list_of_dataframes)

save(flux_mean, file = "fluxes_mean_ABR07.RData")

#removes all variables except functions
#rm(list = setdiff(ls(), lsf.str()))