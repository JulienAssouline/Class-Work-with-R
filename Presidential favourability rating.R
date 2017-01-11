library(readxl)

# Data for this class project was gathered here: http://www.gallup.com/poll/193376/trump-leads-clinton-historically-bad-image-ratings.aspx

# Importing favorability ratings 

excel_sheets("US presidencial favouribility ratings.xlsx")
favouribility_ratings <- read_excel("US presidencial favouribility ratings.xlsx", sheet = "Sheet3") 
favouribility_ratings <- as.data.frame(favouribility_ratings, stringsAsFactors = TRUE) 
head(favouribility_ratings)

favouribility_ratings <- favouribility_ratings[-c(14:16),]
favouribility_ratings
library(ggplot2)

# Plotting trend of unfavorable ratings for each election year 

p <- ggplot()
p +  geom_line(data = favouribility_ratings, aes(x = Year, y = Dem_Total_unfavorable), color = "blue", size = 1) +
geom_point(data = favouribility_ratings, aes(x = Year, y = Dem_Total_unfavorable), size = 3, shape = 21, fill = "blue", colour = "white") +
geom_line(data = favouribility_ratings, aes(x = Year, y = Rep_Total_unfavorable), color = "red", size = 1) +
geom_point(data = favouribility_ratings, aes(x = Year, y = Rep_Total_unfavorable), color = "white", shape = 21, fill = "red", size = 3) +
Julien_theme() + 
scale_x_continuous(breaks = seq(1956,2016,4)) + 
labs(caption = "data: Gallup, no data for 1988,1996, and 2000") + ggtitle("Trump and Clinton are the Most Disliked Candidates in History", subtitle = "Total unfavorable ratings for each election year") +
xlab("") + ylab("") +
annotate("Text", label = "Trump", x = 2013.5, y = 60, colour = "red", size = 4, fontface = "bold") +
annotate("Text", label = "H.Clinton", x = 2018, y = 52, colour = "blue", size = 4, fontface = "bold") +
annotate("Text", label = "Romney", x = 2010, y = 45, colour = "red", size = 3) +
annotate("Text", label = "Obama", x = 2012, y = 35, colour = "blue", size = 3) +
annotate("Text", label = "Obama", x = 2008, y = 33, colour = "blue", size = 3) +
annotate("Text", label = "McCain", x = 2008, y = 39, colour = "red", size = 3) +
annotate("Text", label = "Kerry", x = 2004, y = 42, colour = "blue", size = 3) +
annotate("Text", label = "G.W.Bush", x = 2004, y = 36, colour = "red", size = 3) +
annotate("Text", label = "B.Clinton", x = 1992, y = 30, colour = "blue", size = 3) +
annotate("Text", label = "G.H.W.Bush", x = 1992, y = 42, colour = "red", size = 3) +
annotate("Text", label = "Mondale", x = 1984, y = 36, colour = "blue", size = 3) +
annotate("Text", label = "Reagon", x = 1985, y = 28, colour = "red", size = 3) +
annotate("Text", label = "Carter", x = 1981, y = 30, colour = "blue", size = 3) +
annotate("Text", label = "Reagon", x = 1980, y = 39, colour = "red", size = 3) +
annotate("Text", label = "Carter", x = 1976, y = 14, colour = "blue", size = 3) +
annotate("Text", label = "Ford", x = 1976, y = 26, colour = "red", size = 3) +
annotate("Text", label = "McGovern", x = 1972, y = 43, colour = "blue", size = 3) +
annotate("Text", label = "Nixon", x = 1972, y = 20, colour = "red", size = 3) +
annotate("Text", label = "Humphrey", x = 1970.5, y = 28, colour = "blue", size = 3) +
annotate("Text", label = "Nixon", x = 1968, y = 20, colour = "red", size = 3) +
annotate("Text", label = "Johnson", x = 1964, y = 11, colour = "blue", size = 3) +
annotate("Text", label = "Goldwater", x = 1964, y = 49, colour = "red", size = 3) +
annotate("Text", label = "Kennedy", x = 1960, y = 12, colour = "blue", size = 3) +
annotate("Text", label = "Nixon", x = 1962, y = 20, colour = "red", size = 3) +
annotate("Text", label = "Stevenson", x = 1956, y = 33, colour = "blue", size = 3) +
annotate("Text", label = "Eisenhower", x = 1956, y = 10, colour = "red", size = 3) 


