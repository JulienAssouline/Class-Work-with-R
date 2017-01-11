
library(readxl)

# Data for this class project was gather at http://comtrade.un.org

#Imports for 2015

# importing canadian imports data 
excel_sheets("Canada 2015 import and export.xlsx")

Canada2015 <- read_excel("Canada 2015 import and export.xlsx", sheet = "comtrade-2")

head(Canada2015)
tail(Canada2015)

Canada2015 <- as.data.frame(Canada2015, stringsAsFactors=FALSE)

Canada2015$`Trade Flow`[Canada2015$`Trade Flow` == "Re-Export"] <- "Export"
Canada2015$`Trade Flow`[Canada2015$`Trade Flow` == "Re-Import"] <- "Import"

# Subsetting the imports
Canada2015_Import <- Canada2015[Canada2015$`Trade Flow` == "Import",]

head(Canada2015_Import)
tail(Canada2015_Import)

colnames(Canada2015_Import)[colnames(Canada2015_Import) == "Trade Value (US$)"] <- "Trade_value"

library(dplyr)

#Getting the mean trade value for imports by country
Canada2015_Import_2 <- Canada2015_Import %>% group_by(Country) %>% summarise(Trade_Value_mean = mean(Trade_value)) %>% as.data.frame()

Canada2015_Import_2

# joining with map database to get the longitude and lattitude coordinates of the countries
map <- map_data("world")
head(map)

colnames(map)[colnames(map) == "region"] <- "Country"

Canada2015_Import_3 <- left_join(Canada2015_Import_2, map, by = c("Country"))

head(Canada2015_Import_3)
tail(Canada2015_Import_3)


head(map)

head(Canada2015_Import_3)

# Creating a choropleth map of which shows which countries Canada has imported the most from  
ggplot(data = Canada2015_Import_3) +
  borders(database = "world", 
          colour = "grey60",
          fill="grey90") + 
  geom_polygon(aes(x=long, y=lat, group = group, fill = Trade_Value_mean),
               color = "grey60") +
  scale_fill_gradient( low= "Grey", high = "red", name = "Trade Value ($US)", labels = comma) + 
  ggtitle("Canadian Imports in 2015") + 
  xlab("") + ylab("") + 
  theme(panel.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = .5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + labs(caption = "Data: http://comtrade.un.org")



library(dplyr)

library(ggplot2)

map <- map_data("world")
head(map)

#Exports for 2015

# Subsetting the Canadian Exports
Canada2015_Export <- Canada2015[Canada2015$`Trade Flow` == "Export",]

head(Canada2015_Export)

colnames(Canada2015_Export)[colnames(Canada2015_Export) == "Trade Value (US$)"] <- "Trade_value" 

# Getting the average trade value by country
Canada2015_Export_2 <- Canada2015_Export %>% group_by(Country) %>% summarise(Trade_Value_mean = mean(Trade_value)) %>% as.data.frame()

head(Canada2015_Export_2) 

# Joining the data with map database to get longitude and lattitude info

Canada2015_Export_3 <- left_join(Canada2015_Export_2, map, by = c("Country")) 

head(Canada2015_Export_3) 

library(scales)

# Mapping which countries Canada esports the most with

ggplot(data = Canada2015_Export_3) +
  borders(database = "world", 
          colour = "grey60",
          fill="grey90") + 
  geom_polygon(aes(x=long, y=lat, group = group, fill = Trade_Value_mean),
               color = "grey60") +
  scale_fill_gradient(low = "grey", high = "blue", name = "Trade Value ($US)", labels = comma) + 
  ggtitle("Canadian Exports in 2015") + 
  xlab("") + ylab("") + 
  theme(panel.background = element_blank(),
        plot.title = element_text(face = "bold", hjust = .5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Trends US and Canada imports


excel_sheets("Trade values between US & Canada.xlsx")
Canada_US_Trade <- read_excel("Trade values between US & Canada.xlsx", sheet = "comtrade (3)") 

head(Canada_US_Trade)


Canada_US_Trade$`Trade Flow`[Canada_US_Trade$`Trade Flow` == "Re-Export"] <- "Export" 
Canada_US_Trade$`Trade Flow`[Canada_US_Trade$`Trade Flow` == "Re-Import"] <- "Import"


colnames(Canada_US_Trade)[colnames(Canada_US_Trade) == "$Trade Value (US$)"] <- "Trade_value"

# Subsetting Imports and exports

Canada_US_Trade_Import <- Canada_US_Trade[Canada_US_Trade$`Trade Flow` == "Import",]

Canada_US_Trade_Export <- Canada_US_Trade[Canada_US_Trade$`Trade Flow` == "Export",]

head(Canada_US_Trade_Import)
head(Canada_US_Trade_Export)

Canada_US_Trade_Import <- as.data.frame(Canada_US_Trade_Import, stringsAsFactors=FALSE)
Canada_US_Trade_Export <- as.data.frame(Canada_US_Trade_Export, stringsAsFactors=FALSE)

# Getting mean imports by year

Canada_US_Trade_Import_mean <- Canada_US_Trade_Import %>% group_by(Year) %>% summarise(Trade_Value_yearly_mean = mean(Trade_value)) %>% as.data.frame()

head(Canada_US_Trade_Import_mean)

# Getting mean exports by year

Canada_US_Trade_Export_mean <- Canada_US_Trade_Export %>% group_by(Year) %>% summarise(Trade_Value_yearly_mean = mean(Trade_value)) %>% as.data.frame()


head(Canada_US_Trade_Export_mean)

# plotting trend of imports between US and Canada

v <- ggplot(data = Canada_US_Trade_Import_mean, aes(x = Year, y = Trade_Value_yearly_mean))
v + geom_line(colour = "Purple", size = 1) + theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 11, face = "bold")) + 
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 9)) + 
  theme(axis.title.y=element_text(size=9)) + 
  theme(panel.grid.major.y=element_blank()) + ylab("Import Trade Value ($US)") +
  scale_y_continuous(labels = comma) + ggtitle("Canadian US Import Value 1989-2015") + labs(caption = "Data: http://comtrade.un.org")


# plotting trend of exports between US and Canada

z <- ggplot(Canada_US_Trade_Export_mean, aes(x = Year, y = Trade_Value_yearly_mean))
z + geom_line(colour = "Dark Green", size = 1) + ggtitle("Canadian US Export Value 1989-2015") + labs(caption = "Data: http://comtrade.un.org") +  theme(plot.background=element_rect(fill = "grey93", colour = "grey93")) +
  theme(plot.title=element_text(size = 12, face = "bold", hjust = .5)) +  
  theme(axis.text.x=element_text(size = 8)) +
  theme(axis.text.y=element_text(size = 8)) +
  theme(axis.title.x=element_text(size = 10)) + 
  theme(axis.title.y=element_text(size=10)) +
  theme(panel.grid.major.y=element_blank()) + ylab("Export Trade Value ($US)") + 
  scale_y_continuous(labels = comma) 



