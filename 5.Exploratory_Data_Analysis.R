#Cleanup environment
rm(list = ls())
# free memory
gc()
setwd("C:/Users/gmanish/Downloads/openminds/code/R/")

library(tidyverse)

#Visualizing Distributions
#barplot
ggplot(data = diamonds) +
geom_bar(mapping = aes(x = cut))

diamonds %>%
count(cut)

#histogram
ggplot(data = diamonds) +
geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>%
count(cut_width(carat, 0.5))

#working with a smaller dataset
smaller <- diamonds %>%
filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
geom_histogram(binwidth = 0.1)

#freqpoly
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
geom_freqpoly(binwidth = 0.1)

#typical values
ggplot(data = smaller, mapping = aes(x = carat)) +
geom_histogram(binwidth = 0.01)


ggplot(data = faithful, mapping = aes(x = eruptions)) +
geom_histogram(binwidth = 0.25)

#Unusual Values

ggplot(diamonds) +
geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>%
filter(y < 3 | y > 20) %>%
arrange(y)
unusual

#Missing Values
#drop rows
diamonds2 <- diamonds %>%
filter(between(y, 3, 20))

#replacing the unusual values with missing values.
diamonds2 <- diamonds %>%
mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
geom_point(na.rm = TRUE)

nycflights13::flights %>%
mutate(
cancelled = is.na(dep_time),
sched_hour = sched_dep_time %/% 100,
sched_min = sched_dep_time %% 100,
sched_dep_time = sched_hour + sched_min / 60
) %>%
ggplot(mapping = aes(sched_dep_time)) +
geom_freqpoly(
mapping = aes(color = cancelled),
binwidth = 1/4
)

#covariation

#A Categorical and Continuous Variable
#freqpoly
ggplot(data = diamonds, mapping = aes(x = price)) +
geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

#barplot
ggplot(diamonds) +
geom_bar(mapping = aes(x = cut))

#freqpoly with density
ggplot(
data = diamonds,
mapping = aes(x = price, y = ..density..)
) +
geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

#boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
geom_boxplot()

#reorder
ggplot(data = mpg) +
geom_boxplot(
mapping = aes(
x = reorder(class, hwy, FUN = median),
y = hwy
)
)

#Two Categorical Variables

#geom_count
ggplot(data = diamonds) +
geom_count(mapping = aes(x = cut, y = color))

#using dplyr
diamonds %>%
count(color, cut)

#geom_tile
diamonds %>%
count(color, cut) %>%
ggplot(mapping = aes(x = color, y = cut)) +
geom_tile(mapping = aes(fill = n))

#Two Continuous Variables

#geom_point
ggplot(data = diamonds) +
geom_point(mapping = aes(x = carat, y = price))

#geom_bin2d and geom_hex
ggplot(data = smaller) +
geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
geom_hex(mapping = aes(x = carat, y = price))

#cut_width
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

#cut_number
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

