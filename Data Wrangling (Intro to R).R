# Introduction to R ####
install.packages("learnr")
library(learnr)
library(datasauRus)
library(gganimate)
library(ggplot2)
library(purrr)
library(rmarkdown)
library(tidyverse)
# Part of the tidyverse:
library(dplyr)
library(magrittr)

# Change this code so it computes two plus two (learnr)
1 + 1

# Data Wrangling ####

# Load the data
participants_data <- read.csv("participants_data.csv")

# Use 'readr' and 'url' function to load data
library(readr)
urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
participants_data <- read_csv(url(urlfile))

# View the full data in console
participants_data

# Use 'head' functions to look at the top rows. Default is 6 rows
head(participants_data, n = 4)

# Use 'names' function to check names of the variables
names(participants_data)

# Use 'str' function to look at the structure of the data
str(participants_data)

# Use '$' to call a particular variable in your data
participants_data$age
participants_data$gender

## Subset ####

### Select ####
# Use 'select' from dplyr to return a subset of a column
select(participants_data,
       academic_parents,
       working_hours_per_day)
# Change the selection to batch and age
select(participants_data,
       batch, age)

# Create a subset of data using 'select' function 
select(participants_data,
       -academic_parents,
       -working_hours_per_day)

### Filter ####

# To extract a subset of rows

# Create a subset of data with 'filter' function.
filter(participants_data,
       working_hours_per_day > 10)
# Change the selection to 
# those who work more than 5 hours a day
filter(participants_data,
       working_hours_per_day >5)

# Create a subset of the data with multiple options 
#in the filter function
filter(participants_data,
       working_hours_per_day >10 &
         letters_in_first_name >6)
# Change the filter to those who 
# work more than 5 hours a day and 
# names are longer than three letters
filter(participants_data,
       working_hours_per_day >5 &
         letters_in_first_name >3)

## Rename ####

# Use 'renmame' function to change the names of the variables
rename(participants_data,
       name_length = letters_in_first_name)
# Rename the variable km_home_to_office as commute
rename(participants_data,
       commute = km_home_to_zef)

## Mutate ####

# To add new variables and columns or transform

mutate(participants_data,
       labor_mean = working_hours_per_day*
         mean(working_hours_per_day))
# Mutate a new column named age_mean that is a function of 
#the age multiplied by the mean of all ages in the group
mutate(participants_data,
       age_mean = age* mean(age))

# Create a commute category with the mutate function
mutate(participants_data,
       commute = 
         ifelse(km_home_to_zef > 10,
                "commuter" , "local"))
# Mutate new column named response_speed 
# populated by 'slow' if it took you 
# more than a day to answer my email and 
# 'fast' for others
mutate(participants_data,
       response_speed =
         ifelse(days_to_email_response > 1,
                "slow" , "fast"))

## Summarize ####

# To generate tables of summary statistics

# Get a summary of selected variables with summarize
summarise(participants_data,
          mean(years_of_study),
          median(letters_in_first_name))
# Create a summary of the participants_mutate data 
# with the mean number of siblings 
# and median years of study
participants_mutate <- summarise(participants_data,
          mean(number_of_siblings),
          median(years_of_study))

participants_mutate

## magrittr use ####

# Do all the previous with a magrittr pipeline %>%. 
#Use the group_by function to get these results for 
#comparison between groups.
participants_data %>%
  group_by(research_continent) %>% #split data into groups
  summarise(mean(days_to_email_response),
            median(letters_in_first_name),
            max(years_of_study))
# Use the magrittr pipe to summarize 
# the mean days to email response, 
# median letters in first name, 
# and maximum years of study by gender
participants_data %>%
  group_by(gender) %>%
  summarise(mean(days_to_email_response),
            median(letters_in_first_name),
            max(years_of_study))

# Now use the mutate function to subset the data and 
# use the group_by function to get these results for 
# comparisons between groups.
participants_data %>%
  mutate(response_speed = ifelse(
    days_to_email_response >1,
    "slow" , "fast")) %>%
  group_by(response_speed) %>%
  summarise(mean(number_of_siblings),
            median(years_of_study),
            max(letters_in_first_name))
# Use the magrittr pipe to create a new column 
# called commute, where those who travel 
# more than 10km to get to the office 
# are called "commuter" and others are "local". 
# Summarize the mean days to email response, 
# median letters in first name, 
# and maximum years of study.
participants_data %>%
  mutate(commute =
           ifelse(km_home_to_zef > 10,
                  "commuter" , "local")) %>%
  group_by(commute) %>%
  summarise(mean(days_to_email_response),
            median(letters_in_first_name),
            max(years_of_study))

## purrr: Apply a function to each element of a vector####

# Use purr to run a regression

# when using base R functions with the magrittr pipeline 
# we use ‘.’to refer to the data.
# The functions split and lm are from base R and stats

# Use purrr to solve: split a data frame into pieces, 
# fit a model to each piece, compute the summary, then extract the R^2.

# library (purrr)
participants_data %>%
  split(.$gender) %>% 
  map(~
        lm(number_of_publications ~
             number_of_siblings,
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# Homework ####

## Long format ####

# Select carat and price from diamonds dataset
select(diamonds,
       carat,
       price)

# Filter only where carat is > 0.5
filter(diamonds,
       carat > 0.5)

# Rename price as cost
rename(diamonds,
       cost = price)

# Mutate: create a variable with ‘expensive’ 
# if greater than mean of cost and ‘cheap’ otherwise
mutate(diamonds,
       cost =
         ifelse( price > mean(price),
                 "expensive" , "cheap"))

#summarize: give some summary statistics of your choice
summarise(diamonds,
          mean(carat),
          median(depth),
          max(table))

## Short format ####

# group_by: split into cheap and expensive
#summarize: give some summary statistics of your choice
diamonds %>% 
  mutate(cost =
        ifelse(price > mean(price),
         "expensive" , "cheap")) %>%
  group_by(cost) %>%
    summarise(mean(carat),
              median(depth),
              max(table))



  
    
  
  

       