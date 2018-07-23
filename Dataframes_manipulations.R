# working with dataframes, select rows and columns based on indeces, headers, conditions..

# helpful page:
#https://stackoverflow.com/questions/33079512/replace-column-values-for-subset-of-rows-using-a-vector


#starting wtih dataset
ABR.raw.data


# briefly look at data
head(ABR.raw.data)
tail(ABR.raw.data)

# create dummy columns
ABR.raw.data$NEW<-0
ABR.raw.data$also_new<-1
ABR.raw.data$another_one<-2


# briefly look at data again
head(ABR.raw.data)
tail(ABR.raw.data)

unique(ABR.raw.data$Year)


# maybe useful. need to read up more on it.
# mutate_if(ABR.raw.data, ABR.raw.data$Year == 2007,ABR.raw.data[,13:15]<-to_insert[1:3])
# for the above, need library(dplyr)
# assign() function may also be useful


ABR.raw.data[which(ABR.raw.data$Year == 2007),]


# the fields that are to be inserted must be of the same kind
#( eg. here a data.frame, as ABR.raw.data is a data frame)

to_insert_2<-data.frame(x=1,y=2,z=3)
to_insert_3<-as.data.frame(matrix(data=NA,nrow=4,ncol=3))
to_insert_3[3,]<-9
to_insert_3[1,]<-8


# _____________

to_insert_2

ABR.raw.data[ABR.raw.data$Year == 2007 & ABR.raw.data$Species == 'Pinus sylvestris',13:15 ] <-to_insert_2
head(ABR.raw.data[ABR.raw.data$Year == 2007,])
tail(ABR.raw.data[ABR.raw.data$Year == 2007,]) # only year 2007, and species Pinus species Pinus was replaced
# _____________




# select specific rows from to_insert_3
to_insert_3

ABR.raw.data[ABR.raw.data$Year == 2007 & ABR.raw.data$Tree == 48 & ABR.raw.data$Sample==1,13:15 ]<-to_insert_3[3,]
head(ABR.raw.data) # only year 2007, tree 48 and sample 1 has been replaced with the thirs row 3 of data frame to_insert_3


# _____________
ABR.raw.data[1:10,13:15]<-to_insert[1:3]
