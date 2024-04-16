rm(list=ls())# Removes all variables stored previously
library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read_excel("Desktop/Portfolio/COVID_19_Dashboard/COVID19_line_list_data.xlsx") #reads the excel value to a short and resuable variable name
summary(data) #To get a summary of the data in the COVID 19 datasheet
glimpse(data)
unique(data) #helps to find the unique values in the table

#Cleaned up death column
data$death_dummy <- as.integer(data$death != 0) #created a new column named death_dummy which shows the number of people died
data$recovered_dummy <- as.integer(data$recovered != 0)
Recovery_count <- data %>%
  count(recovered_dummy)
View(Recovery_count)
#Grouping the Data for Representation
data <- data %>% mutate(club_age = case_when(
  age <= 10 ~ "0-10",
  age > 10 & age <= 20 ~ "10-20",
  age > 20 & age <= 30 ~ "20-30",
  age > 30 & age <= 40 ~ "30-40",
  age > 40 & age <= 50 ~ "40-50",
  age > 50 & age <= 60 ~ "50-60",
  age > 60 & age <= 70 ~ "60-70",
  age > 70 & age <= 80 ~ "70-80",
  age > 80 & age <= 90 ~ "80-90",
  age > 90 ~ "Above 90",
  TRUE ~ "Other"
))

#death rate = 0.05806452 = ~0.058
sum(data$death_dummy) / nrow(data) #no. of people died/ total no. of people infected


#AGE
#Claim: Older infected people die.
dead = subset(data, death_dummy == 1) #63
alive = subset(data, death_dummy == 0) #1022
install.packages("na.tools")
class(data$age) #Helps determine the data type of a column/object
a <- as.numeric(dead$age) #converts a data type into numeric data type
b <- as.numeric(alive$age)
dataframe <- data.frame(dead)
mean(a, na.rm=TRUE) #68.58621
mean(b, na.rm = TRUE)#48.07229

#statistical significance
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)

#If p-value <0.05, we reject the NULL hypothesis.
#Hence proved that people who died were much older than the rest of the population in the sample.

#Gender
#Claim: Gender has no effect.
women = subset(data, gender == "female") #
men = subset(data, gender == "male") #
class(dead$gender)
gender <- as.character(dead$gender)
class(gndr)
mean(women$death_dummy, na.rm = TRUE) #3.66%
mean(men$death_dummy, na.rm = TRUE)#8.46%

#statistically significant
t.test(women$death_dummy, men$death_dummy, alternative="two.sided", conf.level = 0.99)
#With 99% confidence, we can conclude that men have about 0.8% - 8.8% higher chances of dying.

install.packages("ggplot2")
#Plot for Deaths based on Gender
dt <- filter(dead, gender != "NA") #Filters out a particular value
ggplot(dt, aes(x = gender, y = death_dummy))+
  geom_bar(stat = "identity") +
  labs(title = "Death Rate based on Gender",
       x = "Gender",
       y = "Deaths")

#Plot for Deaths based on Age
age_count <- data %>% 
  count(club_age)
View(age_count)

dd <- filter(data, club_age != "NA")
age_count %>%
ggplot(aes(x = club_age, y = n))+
  geom_bar(stat = "identity") +
  labs(title = "Infected based on Age",
       x = "Age Group",
       y = "No. of Deaths") +
  geom_text(aes(label = n ), vjust = 0 )

#Recovery based on Age
recovery = subset(data, recovered_dummy == 1)
recovery_rate <- recovery %>%
  count(club_age)

recovery_rate %>%
  ggplot(aes(x = club_age, y = n))+
  geom_bar(stat = "identity") +
  labs(title = "Recovery based on Age",
       x = "Age Group",
       y = "No. of Recoveries") +
  geom_text(aes(label = n ), vjust = 0 )
