#extracts tree description to add age and height to each tree in the big data frame

#1) Add Age diameter & height to our big cell count data dataframe
library(readxl)

#path to the cellcount data
path <- "/Users/lancelotdacosta/Desktop/Summer project/data/Cellular_resolution_France_Vosgues copy/Donon Data/Cell Count Data/ABR2007 Cell Count - 2014-12-18.xlsx"

#extracts raw cell count data
ABR07_data <- path %>%
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

#extracts tree description of ABR07
Tree_description <- ABR07_data$`Tree description`

Tree_age_dimensions <- select(Tree_description, Tree, Age, Diameter, Height)


#loads big cell count dataframe called df
load("/Users/lancelotdacosta/CMP project/ABR07/cell_count_ABR07.RData")


list_of_dataframes <- vector("list", length(unique(df$Tree)))
i <- 1 #a counter

for(tree in unique(df$Tree)){
  
  temp1 <- filter(Tree_age_dimensions, Tree ==tree)
  
  temp2 <- df %>%
    filter(Tree == tree) %>%
    mutate(Age = temp1$Age[1] - 3, #removes 3 to the ages as they were estimated in 2010 -> date sensitive!!!!!!
           Diameter= temp1$Diameter[1],
           Height= temp1$Height[1])
  
  list_of_dataframes[[i]] <- temp2
  i <- i+1
}

df_improved <- rbind.fill(list_of_dataframes)


#Adds daylength our our big cell count data dataframe

#Add day length
library(suncalc)

#gets latitude and longitude from the site
latitude <- as.numeric(filter(ABR07_data$`General description`, Investigators == "Latitude")[1,2])
longitude <- as.numeric(filter(ABR07_data$`General description`, Investigators == "Longitude")[1,2])


list_of_dataframes <- vector("list", length(unique(df_improved$annee))*length(unique(df_improved$jour)))
i <- 1 #a counter

for(year in unique(df_improved$annee)){ #works with data from different years
  
  start_date <- paste(c(as.character(year-1), "-12-31"), collapse= "")  #last day of the year before
  
  for(day in unique(df_improved$jour)){
      
      #filter by year and day
      temp <- filter(df_improved, annee== year, jour == day)
      
      #Date of the day
      Date_of_day <- as.Date(day, origin = start_date)
     
      #Get length of the day
      sunrise_sunset <- getSunlightTimes(date = Date_of_day, lat = latitude, lon= longitude, keep = c("sunrise", "sunset"))
      Length_of_day <- as.numeric(difftime(sunrise_sunset$sunset, sunrise_sunset$sunrise), units="hours")
      
      #Add length of day variables to temp
      temp <- mutate(temp, daylength = Length_of_day)
      
      #Add temp to our list of dataframes
      list_of_dataframes[[i]] <- temp                           
      i <- i+1
}
}

#merge our dataframes
df_improved <- rbind.fill(list_of_dataframes)



#3) Adds altitude to our big cell count data dataframe
#Add a variable for altitude
df_improved <- mutate(df_improved, 
      Altitude = as.numeric(filter(ABR07_data$`General description`, Investigators == "Altitude (m a.s.l.)")[1,2]))


save(df_improved, file = "cell_count_full_ABR07.RData")
