library(xml2)
library(rvest)

# Scraping grey cup attendance from wikipedia
greycup_url <- "https://en.wikipedia.org/wiki/List_of_Grey_Cup_champions" 

greycup_attendance <- greycup_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[1]') %>%
  html_table(fill = TRUE) %>%
  as.data.frame()

greycup_attendance

library(dplyr)
library(tidyr)

# Getting rid of the commas by seperating the values and the removing unwanted column
greycup_attendance1 <- separate(data = greycup_attendance, col = Date, c("Date", "Year"), sep = ",") 

greycup_attendance1

greycup_attendance1$Date <- NULL

# Adding years to blank values 

greycup_attendance2[8,2] <- "1916"
greycup_attendance1[9,2] <- "1917"
greycup_attendance1[10,2] <- "1918"
greycup_attendance1[11,2] <- "1919"
greycup_attendance1[55,2] <- "1962"


greycup_attendance1 <- greycup_attendance1[-c(110),]

# Getting rid of '!' symbole
greycup_attendance1 <- separate(data = greycup_attendance1, col = Score, c("wtv", "Score"), sep = "!") 

greycup_attendance1$wtv <- NULL

greycup_attendance1[54,4] <- "21-14"
greycup_attendance1[98,4] <- "38-35"

# seperating win loss records
greycup_attendance2 <- separate(data = greycup_attendance1, col = Score, c("win", "lose")) 

# removing commas 
greycup_attendance2$Attendance = gsub(",","", greycup_attendance2$Attendance)

# Making values numeric
greycup_attendance3 <- transform(greycup_attendance2, win = as.numeric(win), 
                                lose = as.numeric(lose),
                                Year = as.numeric(Year),
                                Attendance = as.numeric(Attendance)
)

head(greycup_attendance2)

# Game column is messy and full of unwanted elements. Such as being described as the 3rd World cup. 
# I wanted to make a numeric value, so I had to get rid of all the junk serounding it.
greycup_attendance4 <- separate(greycup_attendance3, col = Game, c("wtv", "Game"), sep = "!", extra = "merge", fill = "left") 
greycup_attendance4$wtv <- NULL
greycup_attendance4

greycup_attendance4$Game = gsub("th", "", greycup_attendance4$Game)
greycup_attendance4$Game = gsub("st", "", greycup_attendance4$Game)
greycup_attendance4$Game = gsub("nd", "", greycup_attendance4$Game)
greycup_attendance4$Game = gsub("rd", "", greycup_attendance4$Game)
greycup_attendance4$Game = gsub("th", "", greycup_attendance4$Game)
greycup_attendance4$Game = gsub("[C]", "", greycup_attendance4$Game)
greycup_attendance4$Game = gsub("[I]", "", greycup_attendance4$Game)

greycup_attendance4[109,1] <- 104
greycup_attendance4[104,1] <- 99
greycup_attendance4[32,1] <- 28
greycup_attendance4[33,1] <- 28

# Making game column numeric
greycup_attendance4 <- transform(greycup_attendance4, Game = as.numeric(Game))
head(greycup_attendance4)

# Looking at score difference

greycup_attendance4$difference_score <- greycup_attendance4$win - greycup_attendance4$lose

head(greycup_attendance4)

# Creating labels for annotating by creating data frames

annot_cup <- read.table(text = 
                       "YEAR|Attendance|text
                        1917|10088|Play was suspended due to<br>World War 1 from 1916-1918<br>and 1919 due to rules dispute", sep = "|", header = TRUE, stringsAsFactors = FALSE) 
head(annot_cup)
annot_cup$text <- gsub("<br>", "\n", annot_cup$text)


annot_cup1 <- read.table(text = "Year|Attendance
                                 2015|36634", sep = "|", header = TRUE, stringsAsFactors = FALSE)

head(annot_cup1)

annot_cup2 <- read.table(text = "Year|Attendance
                                 1977|68205", sep = "|", header = TRUE, stringsAsFactors = FALSE)


### I did extra work and exploration of the data, but at the end of the day, I ended up only examining the Grey cup attendance.

library(ggalt)
library(ggplot2)
library(ggrepel)

# Plotting trend in Grey Cup attendance


m <- ggplot()
m + geom_line(data = greycup_attendance4, aes(x = Year, y = Attendance), colour = "darkred", size = 1) +
  annotate("Text", label = "1975-1977, 110% increase!", x = 1962, y = 50000, size = 3) + 
  geom_text_repel(data = annot_cup, aes(x = YEAR, y = Attendance, label = text), size = 3, arrow = arrow(length = unit(0.01, 'npc')), nudge_y = 10000) +
  annotate("Text", label = "Lowest in 17 years", x = 2012, y = 33000, size = 3) +
  geom_point(data = annot_cup1, aes(x = Year, y = Attendance), colour = "darkred") + 
  geom_point(data = annot_cup2, aes(x = Year, y = Attendance), colour = "darkred") +
  annotate("Text", label = "68,205 highest attendance (1977)", x = 1992, y = 69205, size = 3) + 
  Julien_theme() + ggtitle("Grey Cup Attendance 1909-2015") + labs(caption = "Data: Wikipedia") + xlab("") 
  
