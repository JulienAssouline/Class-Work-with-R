

library(readxl)
library(tidyr)
library(dplyr)

# Importing unemployment rates

excel_sheets("US unemployment rates.xlsx")
US_Unemployment_1948_2016 <- read_excel("US unemployment rates.xlsx", sheet = "Sheet2") 
head(US_Unemployment_1948_2016)
US_Unemployment_1948_2016 <- as.data.frame(US_Unemployment_1948_2016, stringsAsFactors = FALSE)

# Getting the Average unemployment rate by month

US_Unemployment_1948_2016$Avg <- (US_Unemployment_1948_2016$Jan + US_Unemployment_1948_2016$Fab + US_Unemployment_1948_2016$Mar + 
                                US_Unemployment_1948_2016$Apr + US_Unemployment_1948_2016$May + US_Unemployment_1948_2016$`Jun `+ 
                                US_Unemployment_1948_2016$Jul + US_Unemployment_1948_2016$Aug + US_Unemployment_1948_2016$Sep + 
                                US_Unemployment_1948_2016$Oct + US_Unemployment_1948_2016$Nov + US_Unemployment_1948_2016$Dec)/12
head(US_Unemployment_1948_2016)


# Transforming a wide data frame into a long one

US_Unemployment_1948_2016_long <- gather(US_Unemployment_1948_2016, Month, Unemployment, Jan:Dec, factor_key = TRUE)

# Calculating the 10 day rolling average unemployment rate
US_Unemployment_1948_2016_long$Unemployment1 <- filter(US_Unemployment_1948_2016_long$Unemployment/10, rep(1,10)) 
head(US_Unemployment_1948_2016_long)
str(US_Unemployment_1948_2016_long)

# Transforming values into dates
US_Unemployment_1948_2016_long$Month <- as.Date(US_Unemployment_1948_2016_long$Month, format = "%B")
US_Unemployment_1948_2016_long

# Importing women and men difference in unemployment rates
Women_and_men_unemployment_rate <- read.csv(file.choose())
Women_and_men_unemployment_rate

US_Unemployment_1948_2016 

library(ggplot2)

Unemployment_2007 <- subset(US_Unemployment_1948_2016, Year %in% c(2007))
head(Unemployment_2007)

# I didn't end up using the log format of the unemployment database for any analysis. 

# Plotting yearly average US unemployment rates

r <- ggplot(US_Unemployment_1948_2016, aes(x = Year, y = Avg))
r + geom_line(color = "royalblue3", size = 1) +
  geom_point(data = Unemployment_2007, aes(x = Year, y = Avg), colour = "red3", size = 1.5) + 
  annotate("Text", label = "Start of recession", x = 2009, y = 4.4, colour = "red3", size = 3, fontface = "bold") +
  scale_x_continuous(breaks = seq(1948,2014,6)) + Julien_theme() +
  ggtitle("US Unemployment Rates 1948-2015", subtitle = "Yearly Average Unemployment") + xlab("") + ylab("Average Unemployment") +
  labs(caption = "Data: Bureau of Labor Statistics")

# plotting trend in women and men unemplyment rates. 

a <- ggplot()
a + geom_line(data = Women_and_men_unemployment_rate, aes(x = Year, y = Percent.of.labor.force.women), colour= "chocolate4", size =1) + 
  geom_line(data = Women_and_men_unemployment_rate, aes(x = Year, y = Percent.of.labor.force.men), colour = "purple", size = 1) +
  annotate("Text", label = "MEN", x = 2012, y = 9, colour = "purple", size = 3, fontface = "bold") +
  annotate("Text", label = "WOMEN", x = 2010, y = 8, colour = "chocolate4", size = 3, fontface = "bold") +
  Julien_theme() + ylab("Percent of Labor Force Unemployed") +
  ggtitle("More men are unemployed than women", subtitle = "Men and women unemployment percentage 2006-2015") +
  labs(caption = "Data: Bureau of Labor  Statistics") + 
  scale_x_continuous(breaks = seq(2006,2015,2)) + 
  scale_y_continuous(breaks = seq(4.6, 11, 1))

# Importing U-6 unemployment rates. 
# This way of measuring unemployment rates takes into account people who are not actively looking for work, 
# and people who are looking for full time work but are only working part-time.
`U-6` <- read.csv(file.choose()) 
`U-6`
str(`U-6`)

# Plotting U-6 Unemployment rate trend.
b <- ggplot()
b + geom_line(data = `U-6`, aes(x = Year, y = Average), colour= "red", size =1) + 
  Julien_theme() + ylab("U-6 Unemployment rate") +
  ggtitle("U-6 Unemployment Measure 2006-2016") +
  labs(caption = "Data: Bureau of Labor  Statistics.
                  2016 does not include December.") +
  scale_y_continuous(breaks = seq(8,16,2))
  
