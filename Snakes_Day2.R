snakes <- read_csv("data/snakes.csv")
snakes$day = as.factor(snakes$day)

snakes.summary <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
 
ggplot(data = snakes, aes(x = snake, y = openings)) +
  geom_point() +
  geom_line(aes(group = day))

library(ggplot2)
#Basic barplot
p<-ggplot(data = snakes, aes(x = snake, y = openings)) +
  geom_bar(stat="identity")
p

ggplot(data = snakes, aes(x = openings)) +
  geom_histogram(aes(fill = day), position = "dodge", binwidth = 100) +
  labs(x = "snake", y = "count")

snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)

par(mfrow = c(2, 2))

snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "purple")

plot(fitted(snakes.aov), residuals(snakes.aov), col = "green")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "brown")
