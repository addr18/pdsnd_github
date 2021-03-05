#Read In BikeShare Data
getwd()
list.files()
ny <- read.csv('new-york-city.csv')
wash <- read.csv('washington.csv')
chi <- read.csv('chicago.csv')
names(chi)
names(ny)

#call ggplot2
library(ggplot2)
library(ggthemes)
library(dplyr)

names(chi)

#Question #1 - On average for trips under
#120 minutes in New York, who had the longer trip
#duration, men or women?

ggplot(aes(x = Gender, y = Trip.Duration/60),
       data = subset(ny, Gender = 'Male',
                     Gender = 'Female',
                     Gender != '')) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, 10))+
  ggtitle('Trip Duration By Gender')+
  labs( y = 'Trip Duration (Mins.)')+
  labs( x = 'Gender')+
  theme_minimal(10)

table(ny$Gender)
by(ny$Trip.Duration/60, ny$Gender, summary)

# Question 2- What is the most common end station in
# Chicago by Gender?

chi.customer <- filter(chi, User.Type == 'Customer')
table(chi.customer$End.Station)

ggplot(aes(x = User.Type),
       data = subset(chi, User.Type != 'Dependent')) +
  geom_bar()+
  ggtitle('User Type Count By Gender - Chicago')+
  labs( y = 'Count')+
  labs( x = 'User Type')

#Question 3 - Five most common end stations by count?

top.five <- chi %>%
  count(End.Station, sort = TRUE) %>%
  top_n(5)

ggplot(aes(x = End.Station, y = n),
       data = top.five) +
  geom_col()
