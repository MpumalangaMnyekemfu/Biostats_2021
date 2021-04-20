question 1
numerical data, data containing numerical values. an example would be chick weight
descriptive data, data that describes a particular feature of a subject. an example would be the colours of roses found at a certain field

to view data, data(), 

skewness refers to the distribution of data, whether it has normal distribution, skewed to the left or skewed to the right. kurtosis is used to help us determine skewness of a data set and to group the data into quartiles


question 2
ls(Orange)
library(tidyverse)
library(plotly)
ls(Orange:dataset)
the orange dataset is numerical data because it shows the age of the trees and their circumference
ggplot(data = Orange, aes(x = age, fill = circumference)) + geom_histogram(position = "stack", binwidth = 1, alpha = 0.8)

question 3
mutate(), this is used to transform the data
select(), this chooses the set of data that we are working with
group_by(), this is a way to group together data of the same kind
filter(), removes unwanted data from the data we want to preserve
separate(), converts a single column into mutiple columns 