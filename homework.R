#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 
# Yushan Guo

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER

ds %>% glimpse() %>% select(Year)

# Two values have quotation marks that would result in NAs when using as.numeric()
# Remove the values that contain quotation marks
library(stringr)
ds$Year <- str_replace_all(ds$Year, '["\']', '')

# Then transform
ds$Year <- as.numeric(ds$Year)
typeof(ds$Year)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER

ds <- ds %>% rename_with(tolower)


### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER

ds <- ds %>% mutate(decade = floor(year / 10) * 10)


### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER

ds <- ds %>% arrange(rank)


### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER

#typeof(ds$rank)
top10 <- ds %>% filter(rank <= 10) %>% select(song, artist)
top10


### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER

ds_sum <- ds %>% summarize(earliest = min(year),
                           most_recent = max(year),
                           avg_release = floor(mean(year)))
ds_sum

#Mcomment: Looks good, since you dealt with the NAs you don't need it, but the key include na.rm = T in the summarize functions
ds_sum <- ds %>% summarize(min_yr = min(year, na.rm = T),
                 max_yr = max(year, na.rm = T),
                 mean_yr = mean(year, na.rm = T))

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER

ds %>% filter(year %in% ds_sum) %>% arrange(year) %>% select(song, artist, year)

#Mcomment: Looks good - the key uses OR commands but this is the more efficient way

ds %>% filter(year == round(ds_sum$min_yr) | 
                year == round(ds_sum$mean_yr) | 
                year == round(ds_sum$max_yr) ) %>% arrange(year)


### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER

ds <- ds %>% mutate(year = ifelse(year < 1900, year + 100, year))

#Mcomment: See an alternative ifelse below
ds  <- ds %>% mutate(year = ifelse(song == "Brass in Pocket", 1979, year)

ds <- ds %>% mutate(decade = floor(year / 10) * 10)

ds_sum_correct <- ds %>% summarize(earliest = min(year),
                           most_recent = max(year),
                           avg_release = floor(mean(year)))
ds_sum_correct

ds %>% filter(year %in% ds_sum_correct) %>% arrange(year) %>% select(song, artist, year)


### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER

ds %>% group_by(decade) %>% summarize(avg_rank = floor(mean(rank)), num_song = n())

#Mcomment: Again you already dealt with the NAs, but if you hadn't you could do the below
ds %>% filter(!is.na(decade)) %>% 
  group_by(decade) %>% 
  summarize(mean_rank = mean(rank), n_songs = n())
                     
### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

# ANSWER

ds %>% group_by(decade) %>% count() %>% ungroup() %>% slice_max(order_by = n, n = 1)
                     
#Mcomment: Count allows you to group without the groupby function, see below 
ds %>% count(decade) %>% slice_max(n)
