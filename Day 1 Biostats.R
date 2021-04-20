# Calculate sample size
library(tidyverse)
chicks <- as_tibble(ChickWeight)
chicks <- datasets::ChickWeight
#
library(tidyverse)
chicks %>% 
filter(Time=="20")
summarise(mean)

# library(e1071)
skewness(faithful$eruptions)

library(tidyvers)
chicks %>% 
 filter(Time == 20) %>% 
  group_by(Diet)
chicks %>% 
  summarise(mean_wt = mean(weight))
mean(chicks$weight
     summarise(mean_wt = round(mean(weight) , 1))
     chicks %>% 
 # library(e1071)
skewness(chicks$weight)     

     chicks %>% 
       summarise(min_wt = min(weight),
                 qrt1_wt = quantile(weight, p = 0.25),
                 med_wt = median(weight),
                 qrt3_wt = quantile(weight, p = 0.75),
                 max_wt = max(weight)) 
     chicks %>% 
       summarise(min_wt = min(weight),
                 qrt1_wt = quantile(weight, p = 0.25),
                 med_wt = median(weight),
                 qrt3_wt = quantile(weight, p = 0.75),
                 max_wt = max(weight))
     