#Cleanup environment
rm(list = ls())
# free memory
gc()
setwd("C:/Users/gmanish/Downloads/openminds/code/R/")

#load packages
library(nycflights13)
library(tidyverse)

?flights
str(flights)
flights
View(flights)

#filter
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))

#This will print an error.
filter(flights, month = 1)

#Comparisons
sqrt(2) ^ 2 == 2
1/49 * 49 == 1
near(sqrt(2) ^ 2, 2)
near(1 / 49 * 49, 1)

#Logical Operators
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#Missing values
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

x=5
is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)


#arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))


#select
select(flights, Y=year, month, day)
select(flights, year:day)
select(flights, -(year:day))
rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())

#mutate
flights_sml <- select(flights,
year:day, ends_with("delay"),
distance, air_time)

mutate(flights_sml,
gain = arr_delay - dep_delay,
speed = distance / air_time * 60)

mutate(flights_sml,
gain = arr_delay - dep_delay,
hours = air_time / 60,
gain_per_hour = gain / hours)

transmute(flights,
gain = arr_delay - dep_delay,
hours = air_time / 60,
gain_per_hour = gain / hours)

#Modular arithmetic
transmute(flights, dep_time,
hour = dep_time %/% 100,
minute = dep_time %% 100)

(x <- 1:10)
lag(x)
lead(x)

x
cumsum(x)
cummean(x)
cumprod(x)
cummin(x)
cummax(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
dense_rank(desc(y))
percent_rank(y)
cume_dist(y)
ntile(y,2)


#summarize
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
#summarize with groupby
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
count = n(),
dist = mean(distance, na.rm = TRUE),
delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20)

# It looks like delays increase with distance up to ~750 miles
# and then decrease. Maybe as flights get longer there's more
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
geom_point(aes(size = count), alpha = 1/3) +
geom_smooth(se = FALSE)

#combining with pipe
delays <- flights %>%
group_by(dest) %>%
summarize(count = n(),
dist = mean(distance, na.rm = TRUE),
delay = mean(arr_delay, na.rm = TRUE)) %>%
filter(count > 20, dest != "HNL")


#missing values
flights %>%
group_by(year, month, day) %>%
summarize(mean = mean(dep_delay))

flights %>%
group_by(year, month, day) %>%
summarize(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>%
filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
group_by(year, month, day) %>%
summarize(mean = mean(dep_delay))

#counts

delays <- not_cancelled %>%
group_by(tailnum) %>%
summarize(
delay = mean(arr_delay)
)
ggplot(data = delays, mapping = aes(x = delay)) +
geom_freqpoly(binwidth = 10)


delays <- not_cancelled %>%
group_by(tailnum) %>%
summarize(
delay = mean(arr_delay, na.rm = TRUE),
n = n()
)
ggplot(data = delays, mapping = aes(y = n, x = delay)) +
geom_point(alpha = 1/10)

delays %>%
filter(n > 25) %>%
ggplot(mapping = aes(x = n, y = delay)) +
geom_point(alpha = 1/10)

# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
group_by(playerID) %>%
summarize(
ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
ab = sum(AB, na.rm = TRUE))

batters %>%
filter(ab > 100) %>%
ggplot(mapping = aes(x = ab, y = ba)) +
geom_point() +
geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam'

#If you naively sort on desc(ba), the people with the best batting averages are clearly lucky, not skilled
batters %>%
arrange(desc(ba))

#useful summary functions

#mean() and subsetting
not_cancelled %>%
group_by(year, month, day) %>%
summarize(
# average delay:
avg_delay1 = mean(arr_delay),
# average positive delay:
avg_delay2 = mean(arr_delay[arr_delay > 0])
)

#sd()
# Why is distance to some destinations more variable than to others?
not_cancelled %>%
group_by(dest) %>%
summarize(distance_sd = sd(distance)) %>%
arrange(desc(distance_sd))

# When do the first and last flights leave each day?
not_cancelled %>%
group_by(year, month, day) %>%
summarize(
first = min(dep_time),
last = max(dep_time)
)

#Measures of position first(x), nth(x, 2), last(x)
not_cancelled %>%
group_by(year, month, day) %>%
summarize(
first_dep = first(dep_time),
last_dep = last(dep_time)
)

not_cancelled %>%
group_by(year, month, day) %>%
mutate(r = min_rank(desc(dep_time))) %>%
filter(r %in% range(r))

# Which destinations have the most carriers?
not_cancelled %>%
group_by(dest) %>%
summarize(carriers = n_distinct(carrier)) %>%
arrange(desc(carriers))

#shorter code
not_cancelled %>%
count(dest)

#weighted count.
not_cancelled %>%
count(tailnum, wt = distance)

# How many flights left before 5am? (these usually
# indicate delayed flights from the previous day)
not_cancelled %>%
group_by(year, month, day) %>%
summarize(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more
# than an hour?
not_cancelled %>%
group_by(year, month, day) %>%
summarize(hour_perc = mean(arr_delay > 60))

#grouping by multiple variables

daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))

(per_month <- summarize(per_day, flights = sum(flights)))

(per_year <- summarize(per_month, flights = sum(flights)))

#Ungrouping

daily %>%
ungroup() %>% # no longer grouped by date
summarize(flights = n()) # all flights


#grouped mutates and filters

#Find the worst members of each group
flights_sml %>%
group_by(year, month, day) %>%
filter(rank(desc(arr_delay)) < 10)


#Find all groups bigger than a threshold:
popular_dests <- flights %>%
group_by(dest) %>%
filter(n() > 365)
popular_dests

#Standardize to compute per group metrics:
popular_dests %>%
filter(arr_delay > 0) %>%
mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
select(year:day, dest, arr_delay, prop_delay)

