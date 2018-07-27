# https://readxl.tidyverse.org/index.html <- readxl
# https://dplyr.tidyverse.org/ <- Dplyr: A data manipulation package

#setwd()
#install.packages("readxl")

#--0. Read xls/xlsx file
library(tidyverse)
library(readxl)

path <- "~/Dropbox (Cambridge University)/2018_Cambridge/[Demonstration]/Junggae/Namji_2004-2017.xls"

nj <- path %>% 
      excel_sheets() %>% 
      set_names() %>% 
      map(read_excel, path = path)

new_col_name <- c("Station","week","Date","pH","DO","BOD","COD",
                  "SS","TN","TP","TOC","WT","EC","DiN","NH3N","NO3","DiP","POP4","Chl")

njList  <- lapply(nj, setNames, nm = new_col_name)
njList2 <- lapply(njList, function(x) cbind(x, year = ""))

for (i in 1:14) {
  njList2[[i]]$year <- 2003 + i
                }
njDF <- do.call(rbind, njList2)



nj15 <- njDF %>% filter(year == 2015)
nj16 <- njDF %>% filter(year == 2016)

ggplot(nj15, aes(pH, DO, colour = Chl)) + 
  geom_point()

ggplot(nj15, aes(pH, DO)) + 
  geom_point(color = "green")


njDF %>%
  ggplot(aes(x = Date, y = pH, colour = year)) +
  geom_line() +
  # Solution 2 : same plot as solution 1
  #geom_point() +
  #geom_smooth(method = "loess", aes(color = year, fill = year)) + 
  labs(#title = "????",
    x = "", y = "pH") +
  #facet_wrap(~ year, scale = "free_y") +
  #expand_limits(y = 0) + 
  theme_bw() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 20,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text = element_text(size=20)) 

library(reshape2)
njM <- njDF %>% 
        melt(id = c("Station", "week", "Date", "year"),
             variable.name = "Type", value.name = "Value")

njM <- njM %>% 
        select(-one_of("week"))

njM %>%
  ggplot(aes(x = Date, y = Value, colour = year)) +
  geom_line() +
  #geom_point() +
  #geom_smooth(method = "loess", aes(color = year, fill = year)) + 
  labs(#title = "????",
    x = "", y = "pH") +
  facet_wrap(~ Type, scale = "free_y") +
  #expand_limits(y = 0) + 
  theme_bw() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 20,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text = element_text(size=20)) 
