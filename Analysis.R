#install.packages("dplyr")
#install.packages("rvest")
#install.packages("xml2")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("tidy")
#install.packages("ranger")
#install.packages("caret")
#install.packages("readr")

library(dplyr)
library(rvest)
library(xml2)
library(ggplot2)
library(tidyverse) 
library(tidyr) 
library(ranger) 
library(caret)
library(readr)

#Project 1
#Jose De La Fuente

#Are videoGames Sales more affected by User or Critic Scores?

#I originally wanted to see if certain genres became more popular as time went on
#all the way to the pandemic, but i found it difficult to scrape data 
#from Metacritic, as seen in the code below. I then shifted my focus to Analyzing
# sales numbers and the type of reviews. I found an excel spreadsheet containing
#both types of scores as well as Sales numbers of games.
#Unfortunately, the data was outdated as it pulled data up until 2017.

#ATTEMPT 1

#VGChartz


#All Game Data

#VGdata= data.frame()

#for (page_result in seq(from=1, to = 1226, by = 1)){
#  VGLink = paste0("https://www.vgchartz.com/games/games.php?page=",page_result,"&order=ReleaseDate&ownership=Both&direction=DESC&showtotalsales=1&shownasales=1&showpalsales=0&showjapansales=0&showothersales=0&showpublisher=0&showdeveloper=0&showreleasedate=1&showlastupdate=0&showvgchartzscore=0&showcriticscore=1&showuserscore=1&showshipped=1")
#  VGPage = read_html(VGLink)
#  Name= VGPage %>% html_nodes("td~ td+ td a:nth-child(1)") %>% html_text()
#  Release= VGPage %>% html_nodes("td:nth-child(10)") %>% html_text() 
#  VGNASales=  VGPage %>% html_nodes("td:nth-child(9)") %>% html_text()
#  VGTotalSales= VGPage %>% html_nodes("td:nth-child(8)") %>% html_text()
#  VGShipped= VGPage %>% html_nodes("td:nth-child(7)") %>% html_text()
  
  
#  VGdata = rbind(VGdata, data.frame( Name, Release,VGNASales, VGTotalSales, VGShipped,stringsAsFactors = FALSE))
  
#  print(paste("Page:", page_result)) 
#}

#Metascore

#Metadata= data.frame()

#for (page_result in seq(from=0, to = 193, by = 1)){
#  MetaLink = paste("https://www.metacritic.com/browse/games/score/metascore/all/all/filtered?sort=desc&page=",page_result)
#  MetaPage = read_html(MetaLink)
#  Name= MetaPage %>% html_nodes(".title h3") %>% html_text()
#  Release= MetaPage %>% html_nodes(".platform+ span") %>% html_text() 
#  MetaPos=  MetaPage %>% html_nodes(".clamp-metascore .positive") %>% html_text()
#  MetaNeg= MetaPage %>% html_nodes(".clamp-metascore .negative") %>% html_text()
#  MetaMix= MetaPage %>% html_nodes(".clamp-metascore .mixed") %>% html_text()
  
  
# Metadata = rbind(Metadata, data.frame( Name, Release, MetaPos, MetaNeg, MetaMix, stringsAsFactors = FALSE))
  
#  print(paste("Page:", page_result)) 
#}

#Scraping from VG Chartz took too long and had a lot of missing values in its data.
#Metacritic kept giving me HTTP Connection Errors and i could not solve as to why.


#Attempt 2

#With this spreadsheet found online, I was able to refocus my project to just Sales and Scores.
 
Sales_Data <- read_csv("C:/Users/joeyd/Desktop/School/R class/Project1/Sales Data.csv")
View(Sales_Data)

#Check for missing values as spreadsheet is a combination of 2 datasets.
colSums(is.na(Sales_Data))

#clean missing values as we do not need that data
Sales_Data <- Sales_Data[complete.cases(Sales_Data), ]
colSums(is.na(Sales_Data))

#see structure of spreadsheet
str(Sales_Data)

#I see year is not numeric even though it should be.
unique(Sales_Data$Year_of_Release)

#Remove NA
Sales_Data <- Sales_Data[Sales_Data$Year_of_Release != "N/A", ]
unique(Sales_Data$Year_of_Release)

#Convert as integer
Sales_Data$Year_of_Release <- as.integer(Sales_Data$Year_of_Release)

#Check Summary of Sales and review data for any outliers or strange results
summary(Sales_Data$NA_Sales)
summary(Sales_Data$EU_Sales)
summary(Sales_Data$JP_Sales)
summary(Sales_Data$Other_Sales)
summary(Sales_Data$Global_Sales)

summary(Sales_Data$Critic_Score)
summary(Sales_Data$Critic_Count)
summary(Sales_Data$User_Count)
summary(Sales_Data$User_Score)

#User score is not numeric, must change

Sales_Data$User_Score <- as.numeric(Sales_Data$User_Score)
summary(Sales_Data$User_Score)

#User Score is out of 10, Critic Score out of 100.

Sales_Data$User_Score <- Sales_Data$User_Score * 10

#Graph for global sales

ggplot(Sales_Data) + geom_histogram(aes(Global_Sales), fill = "blue")

#Very Skewed data as a higher number of games will have less Sales. Must log scale axis
#to see better visualization of sales numbers with the number of games in the dataset.

ggplot(Sales_Data) + geom_histogram(aes(Global_Sales), fill = "blue") + 
  scale_x_log10()

#now we will see the number of releases per year
Sales_Data %>% group_by(Year_of_Release) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(Year_of_Release, n), stat = "identity", 
           fill = "blue") + theme(axis.text.x = element_text(angle = 90))

#There is a peak in number of video games released around the years 2005-2010.
#Next, lets see the sales for each year

color <- c("Titles released" = "red", "Global sales" = "blue")

Sales_Data %>% group_by(Year_of_Release) %>% 
  summarise(Sales_Data = sum(Global_Sales), count = n()) %>% 
  ggplot() + geom_line(aes(Year_of_Release, count, group = 1, color = "Titles released")) + 
  geom_line(aes(Year_of_Release, Sales_Data, group = 1, color = "Global sales")) + 
  xlab("Year of Release") + ylab("Titles released") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  scale_color_manual(values = color) + labs(color = "")

summary(Sales_Data$Year_of_Release)

#We see that global sales were pretty consistent with the rise and fall of 
#titles released from the years 1990-2018 with the peak being between 2005-2010

#Platform Sales- Visualization of Total Sales by Platform
Sales_Data %>% group_by(Platform) %>% 
  summarise(Sales_Data = sum(Global_Sales)) %>% ggplot() + 
  geom_bar(aes(reorder(Platform, Sales_Data), Sales_Data), stat = "identity", 
           fill = "blue") + 
  xlab("Platform") + ylab("Global sales") + 
  coord_flip()

#Group Platforms by Company
Sales_Data <- Sales_Data %>% mutate(platform2 = case_when(
  Platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA") ~ "Nintendo",
  Platform %in% c("X360", "XB", "XOne") ~ "XBox",
  Platform %in% c("PS3", "PS4", "PS2", "PS", "PSP", "PSV") ~ "PS",
  Platform == "PC" ~ "PC",
  Platform == "DC" ~ "Sega"
))

#Visualize Sales per year for major companies
Sales_Data %>% group_by(platform2, Year_of_Release) %>%
  summarise(Sales_Data = sum(Global_Sales)) %>% 
  ggplot() + 
  geom_line(aes(Year_of_Release, Sales_Data, group = platform2, color = platform2)) +
  xlab("Year of release") + ylab("Global Sales") + labs(color = "") + 
  theme(legend.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5, size = 6))
#Sales by Gaming Genre
Sales_Data %>% group_by(Genre) %>%
  summarise(Sales_Data = sum(Global_Sales)) %>%  
  ggplot() + 
  geom_bar(aes(reorder(Genre, Sales_Data), Sales_Data), stat = "identity", 
           fill = "blue") + 
  ylab("Global Sales") + xlab("Genre") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5)) + 
  coord_flip()

#Sales per Console in each genre
Sales_Data %>% group_by(Platform, Genre) %>% 
  summarise(Sales_Data = sum(Global_Sales)) %>% 
  ggplot() + geom_raster(aes(Genre, Platform, fill = Sales_Data)) + 
  ylab("") + xlab("") + 
  scale_fill_gradient(low = "#ebebe5", high = "red") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 5),
        legend.text = element_text(size = 7)) + labs(fill = "Sales")

#Now we will look at just how sales are affected by user and 
#critic scores on metacritic

colors <- c("Critic score" = "blue", "User score" = "red")
ggplot(Sales_Data) + 
  geom_smooth(aes(Critic_Score, Global_Sales, color = "Critic score")) + 
  geom_smooth(aes(User_Score, Global_Sales, color = "User score")) +
  labs(color = "") + xlab("Score") + ylab("Global sales") + 
  scale_color_manual(values = colors)

#The graph clearly shows that there seems to be a stronger relationship between 
#critic score and sales than between user score and sales. We also see that the 
#relationship does not seem to be linear. It seems that critic scores that are 
#more than 75 are correlated with very high sales, while the correlation between
#score and user sales is weaker.




