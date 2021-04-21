#Mpumalanga_Mnyekemfu_Assignment_1

#Section_1

#Built-in dataset BOD
library(tidyverse)
data("BOD")
C is true


#Section_2
library(dplyr)
library(dslabs)
data("murders")

#exploring 
glimpse(state.region)
glimpse(state.name)

head(murders,8)

tail(murders, 5)

select(murders, region)

#murder dataset description

The murder dataset is straightforward and easy to read and understand. This is also evident in how easily the datasets can be changed and adapted to fit instructions, and even just to explore.

#select 
select(murders, population = 4,state = 1)

#Removing
murders[murders$state != "Florida",]

#no_south data frame
no_south <- murders[murders$region != "South",]
17 states from the South region were removed, 34 states remained in the new data frame 

#South & West population size
murders %>% 
  filter(region == "South") %>% 
  summarise(south_pop = sum(population))

murders %>% 
  filter(region == "West") %>%
  summarise(west_pop = sum(population))

#Northeast population size
northeast <- murders %>%
 filter(region == "Northeast") %>%
  summarise(northeast_pop = sum(population))

#plots
library(tidyverse)
data("murders")
ggplot(data = murders, aes(x = abb, y = total)) +
  geom_point() +
  geom_line(aes(group = region))


plot 2

#South vs West population size
When comparing population size the population size of the South is higher than that of the West

#new data frame
Total <- murders %>% 
  filter(total > 20) %>% 
  filter(total < 100)

#object
create_object <- murders
slice(murders, c(10:24, 26))

murders_tibble <- as_tibble(murders)

tibble_region <- as_tibble(murders) %>% 
  group_by(region)


#Section_3

library(dplyr)
library(dslabs)
data("heights")

#heights dataset description
The heights data set consists of height values for 1050 individuals, both male and female. 

#Exploring
glimpse(heights)

head(heights, 5)

tail(heights, 8)

group_by(heights, sex)


D.heights <- heights %>% 
  heights %>% 
  filter(sex == "Male") %>% 
  summarise(min_mal = min(height),
            median_mal = median(height),
            max_mal = max(height),
            ave_mal = ave(height)
            sd_mal = sd(height))


#Section_4
#vectors
X <-c(1,6,21,19,NA,73,NA)
Y <-c(NA,NA,3,NA,13,24,NA)

summary(X)
2 elements are missing in X
summary(Y)
4 elements are missing in Y

mean(X, na.rm = TRUE)
mean(Y, na.rm = TRUE)


#Section_5

Seasonal_data <-data.frame(year=c(2015,2016,2017,2018),
        winter=c(41,39,47,40),
        spring=c(41,46,57,45),
        summer=c(75,52,85,66),
        Autumn=c(57,66,52,56))
            

#hypothesis
The temperature is highest during summer over the 4 year period of 2015 to 2018

#plots
ggplot() +
    geom_line(data = Seasonal_data, aes(x = year, y = summer), colour = "blue") +
    labs(x = "Year", y = "Temperature (F)") +
    ggtitle("Average summer temperatures over 4 years (2015 to 2018)")


ggplot(data = Seasonal_data, aes(x = year, y = Autumn)) +
  geom_bar(stat = "identity", colour = "green", fill = "red") +
  labs(x = "Year", y = "Temperature (F)") +
  ggtitle("Average Autumn temperatures over 4 years (2015 to 2018)")

#discussion

cats_data<-tibble(cats=c("A","B","C"),
                  position=c("1-2-3","3-1-2","2-3-1"),
                  minutes=c(3,3,3),
                  seconds=c(12,44,15))
cats_data

#separate function to split
cats_data%>% 
  separate(col = position, into = c("first_place", "second_place", "third_place"))
  
#unite
cats_data %>% 
  unite(minutes, seconds, col = "total_time",)


#Section_6

library(tidyverse)
Orange <- datasets::Orange

#applying the data

gather(Orange,Tree) 
The above function collects a set of column and place them in a single column and collects cells of the columns and place them in a single value column

spread(Orange, Tree, circumference)
The above function creates a set of tidy columns from from a pair of key:value columns

separate(Orange, age, into = c("Tree", "circumference"))
The above function makes a single character column into multiple columns

Orange %>%  
  arrange(desc(age))
The above function sorts or reorders row by one or more variable

select(Orange, Tree = 1, circumference = 3)
The above function chooses which variables are of interest

group_by(age)
