#setwd("/Users/annemarieeckes/Documents/OneDrive - University Of Cambridge/Cambridge/Data/Dendrochronology/Vosgues_Cellular_count/Donon Data/Cell Count Data/")
dir()
load("Cellcounts_ABR_2007to2009.RData")

raw_2007<-ABR.raw.data[which(ABR.raw.data$Year==2007),]
tree_numbers<-unique(raw_2007$Tree)

#for ( i in 1:length(tree_numbers)){
tree_numbers=48

# get all days of the year in the subset
day<- unique(raw_2007[which(raw_2007$Tree==tree_numbers),]$DY)
day =93
#for (i in 1:length(day)){
# day seems to be occuring at similar frequency as sample?
# so can use either for selecting the triplet


# all three return TRUE for 149
# only one returns TRUE for 268
triplet<-raw_2007[which(raw_2007$Tree==tree_numbers & raw_2007$DY == 268 ) ,]
# now, evaluate the exoetence of 'NA's across CZ, EZ, WZ, and MZ:

# extend here 
#- create a vector with the cell type names 
# case - all three return TRUE:
if(is.na(triplet$CZ)) # deal with later as it will be replaced with values between Day-1 and Day+1

  #is.na(triplet$MZ)

# if only one NA present:
if(sum(is.na(triplet$MZ))==1){
  # take the ones which are not NA, create mean and then replace NA with that value
  replace_na <- sum(triplet[which(triplet$MZ!='NA'),]$MZ)/2
  replace(triplet, is.na(triplet), replace_na)
}


# if two NAs present:

if(sum(is.na(triplet$MZ))==2){
  # take the ones which are not NA, create mean and then replace NA with that value
  replace_na <- sum(triplet[which(triplet$MZ!='NA'),]$MZ)/2
  replace(triplet, is.na(triplet), replace_na)
}

# extend here..

# after evaluating all Cz, Ez,etx....
# put tiplet back into big data frame bz doing the below:
# select where the conditions are TRUE for NA for given tree for given day:
# which( raw_2007$Tree== tree_number[i] & raw_2007$DY == 268 )



raw_2007_tree<-raw_2007[which(raw_2007$Tree==tree_numbers),]


#} # end day loop
#} # end tree number loop
