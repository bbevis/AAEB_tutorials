
# Data types

my_var <- 30 # my_var is type of numeric
my_var <- "Sally" # my_var is now of type character (aka string)

# numeric - (10.5, 55, 787)
# integer - (1L, 55L, 100L, where the letter "L" declares this as an integer)
# complex - (9 + 3i, where "i" is the imaginary part)
# character (a.k.a. string) - ("k", "R is exciting", "FALSE", "11.5")
# logical (a.k.a. boolean) - (TRUE or FALSE)

# numeric
x <- 10.5
class(x)

# integer
x <- 1000L
class(x)

# complex
x <- 9i + 3
class(x)

# character/string
x <- "R is exciting"
class(x)

# logical/boolean
x <- TRUE
class(x)

# Data structures

# Vector of strings
fruits <- c("banana", "apple", "orange")

# Print fruits
fruits

# Vector with numerical values in a sequence
numbers <- 1:10

numbers

# List of strings
thislist <- list("apple", "banana", 1)

# Print the list
thislist

# Create a matrix
thismatrix <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)

# Print the matrix
thismatrix

# Create a data frame
Data_Frame <- data.frame(
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)

# Print the data frame
Data_Frame

#########################
# Tutorial week 2
#########################
library(dplyr)
# Read in data
auto = read.csv('auto.csv', header = TRUE)

# we have a random column we want to drop....
auto = read.csv('auto.csv', header = TRUE) %>%
  select(-X)

# Check current working directory
getwd()

# change working directory
setwd('filepath')

# Read data in from url
auto <- read.csv("https://www.dropbox.com/s/gm22o5efboc3q0w/auto.csv?dl=1", header=TRUE) %>%
  select(-X)

# Get some summary statistics
summary(auto)
colnames(auto)

# Some data manipulations (requires dplyr)
auto %>%
  select(make, price, mpg)

auto_small = auto %>%
  select(make, price, mpg)

auto %>%
  select(make, price, mpg) %>%
  filter(price > mean(price))

auto %>%
  select(make, price, mpg) %>%
  filter(price > mean(price)) %>%
  group_by(make) %>%
  summarise(mean(price), max(mpg))

library(tidyr)
# wide to long use gather()
#.long to wide use spread()

# Exercise 2
covid=read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# transform and create new variables
covid_july=covid %>% filter(as.Date.character(date)=="2020-07-31") %>%
  mutate(deathsOcases=deaths/cases,
         deaths_sh=deaths/sum(deaths),
         cases_sh=cases/sum(cases))

head(covid_july,10)

# filtering
covid_july %>%
  filter(deathsOcases==max(deathsOcases))

# plotting
library(ggplot2)

covid %>%
  group_by(date) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(date=as.Date(date),
         deathsOcases=deaths/cases) %>%
  ggplot(aes(x=date, y=deathsOcases)) +
  geom_point(color = 'blue') + 
  scale_x_date(date_breaks="1 month",date_labels = "%b") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=18,face="bold"),
        legend.position = 'None',
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Month",y="Death per cases")

ggsave("./plots/deathrate1.png",dpi=200,height=5.8,width=12)
  


# Adding more states
pop=read.csv("https://mondpanther.github.io/datastorieshub/data/populationdata.csv")

covid_states = covid %>%
  left_join(pop, by="state")

# facet plots are awesome
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(date, state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(date=as.Date(date),
         deathsOcases=deaths/cases) %>%
  ggplot(aes(x=date, y=deathsOcases)) +
  geom_point(color = 'blue') + 
  scale_x_date(date_breaks="1 month",date_labels = "%b") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=18,face="bold"),
        legend.position = 'None',
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Month",y="Death per cases") +
  facet_wrap(~state)

ggsave("./plots/deathrate_facet.png",dpi=200,height=5.8,width=12)

##########################
# Regressions
##########################

covid_states %>%
  with(summary(lm(deaths ~ cases + pop + growth)))

# for loops
fruits <- list("apple", "banana", "cherry")

for (x in fruits) {
  print(x)
}
         