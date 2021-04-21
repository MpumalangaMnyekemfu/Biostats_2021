#Day_3

head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)

p.val = 0.001

r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

# Loading libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Reading in the ecklonia dataset
ecklonia <- read_csv("data/ecklonia.csv")

# Using select function to exclude/prevent some columns from being read
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

# Performing correlation analysis
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")


# Kendall rank 
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

# Correlation test
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Multiple panel visual
corrplot(ecklonia_pearson, method = "circle")

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Producing a heat map
# Load libraries
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)
library(corrplot)
library(reshape2)


# dlply
library(plyr)


function (.data, .variables, .fun = NULL, ..., .progress = "none", 
          .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL) 
{
  .variables <- as.quoted(.variables)
  pieces <- splitter_d(.data, .variables, drop = .drop)
  llply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
}
<bytecode: 0x000000e3f49d1310>
  <environment: namespace:plyr>
  > 
  
  # Producing a heat map
  # Load libraries
  library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)
library(corrplot)
library(reshape2)
library(hrbrthemes)

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#melt the data
melted <- melt(ecklonia_pearson)

ggplot(data = melted, mapping = aes(x = X1, y = X2, fill = value)) +
  geom_tile() 
