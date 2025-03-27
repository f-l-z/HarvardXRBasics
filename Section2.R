### 2.1
library(dplyr)
library(dslabs)
data(murders)

# mutate(df, new column), looks for var from data instead of workspace
murders <- mutate(murders, rate=total/population*100000)
head(murders)

# filter(df, conditional)
filter(murders, rate <= 0.71)

# select(df, columns)
new_table <- select(murders,state,region,rate)
filter(new_table, rate <= 0.71)

# pipe: what is being piped is operated on, don't need to specify df murders
murders %>% select(state,region,rate) %>% filter(rate <= 0.71) # first arg is result
16 |> sqrt() |> log2() # same as log2(sqrt(16))

# creating dataframes
grades <- data.frame(names= c("John", "Sally", "Fe", "Max"),
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 85, 85, 90))
class(grades$names) # df default characters to factors

grades <- data.frame(names= c("John", "Sally", "Fe", "Max"),
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
class(grades$names) # stringsAsFactors = FALSE forces it to be character

# Exercise
murders <- mutate(murders, population_in_millions = population/10^6)
murders <- mutate(murders, rank = rank(-rate)) # rank(x) low to high, rank(-x) high to low

select(murders, state, population) |> head()
select(murders, state, abb) |> head()

filter(murders, state == "New York")
filter(murders, rank <=5)

no_florida <- filter(murders, state != "Florida")
no_south <- filter(murders, region != "South")
nrow(no_south)

murders_nw <- filter(murders, region %in% c("Northeast", "West"))
nrow(murders_nw)

my_states <- filter(murders, region %in% c("Northeast","West") & rate < 1)
select(my_states, state, rate, rank)

data(murders)
mutate(murders, rate = total / population * 10^5, rank = rank(-rate)) |>
  filter(region %in% c("Northeast", "West") & rate < 1) |>
  select(state, rate, rank)

### 2.2
library(tidyverse)
data(murders)

# Summarize: creates new df with summarized stats
murders <- mutate(murders, rate = total / population * 10^5)
s <- murders %>% filter(region == "West") %>%
  summarize(minimum = min(rate), # creates a summary table
            median = median(rate),
            maximum = max(rate))
s

us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# Summarize with more values: need to create a function that returns a df
data(heights)
heights |> filter(sex == "Female") |> 
  summarize(median_min_max = quantile(height, c(0,0.5,1))) # returns 3 rows

median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

heights |>
  filter(sex == "Female") |>
  summarize(median_min_max(height))

# Pull to access columns
class(us_murder_rate) # summarize always gives a df
us_murder_rate %>% pull(rate) # get a numeric

us_murder_rate <- murders |> 
  summarize(rate = sum(total) / sum(population) * 100000) |>
  pull(rate)
class(us_murder_rate) # now a numeric bc piped into pull function

# Dot Placeholder: placeholder for the df
us_murder_rate <- murders |> 
  summarize(rate = sum(total) / sum(population) * 100000) |>
  _$rate # _ is for |> syntax

us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate # . is for %>% syntax

# Group then Summarize (group_by)
murders %>% group_by(region) # grouped data frame

murders %>%
  group_by(region) %>%
  summarize(median = median(rate))

# Sorting (arrange)
murders %>% arrange(population) %>% head() #ascending
murders %>% arrange(desc(rate)) %>% head() #descending
murders %>% arrange(region, rate) %>% top_n(10)

# Exercise
library(NHANES)
data(NHANES)

data(na_example)
mean(na_example)
sd(na_example)

mean(na_example, na.rm = TRUE) # removes NA
sd(na_example, na.rm = TRUE) # removes NA

# std bp for ages 20-29
ref <- NHANES |> filter(AgeDecade == " 20-29") |>
  summarize(SD = sd(BPSysAve, na.rm = TRUE))

ref_avg <- NHANES |> filter(AgeDecade == " 20-29") |>
  summarize(SD = sd(BPSysAve, na.rm = TRUE)) |>
  pull(SD)

# min and max bp for ages 20-29
NHANES |> filter(AgeDecade == " 20-29") |>
  summarize(min = min(BPSysAve, na.rm = TRUE), max = max(BPSysAve, na.rm = TRUE))

# Female average and std BP by age groups
NHANES |> filter(Gender == "female") |>
  group_by(AgeDecade) |>
  summarize(average = mean(BPSysAve, na.rm = TRUE), std = sd(BPSysAve, na.rm = TRUE))

# Male average and std BP by age groups
NHANES |> filter(Gender == "male") |>
  group_by(AgeDecade) |>
  summarize(average = mean(BPSysAve, na.rm = TRUE), std = sd(BPSysAve, na.rm = TRUE))

# Avg and std BP by age group and gender
NHANES |> group_by(AgeDecade, Gender) |>
  summarize(average = mean(BPSysAve, na.rm = TRUE), std = sd(BPSysAve, na.rm = TRUE))

# BP for 40-49 males by race
NHANES |> filter(AgeDecade == " 40-49" & Gender == "male") |>
  group_by(Race1) |>
  summarize(average = mean(BPSysAve, na.rm = TRUE)) |>
  arrange(desc(average))

### 2.3 Data.table has slightly different syntax designed to waste memory
library(data.table)
data(murders)

murders <- setDT(murders) # convert to a data.table object

# select
select(murders, state, region) # dplyr syntax
murders[, .(state, region)] |> head() # data.table syntax

# mutate
murders[, rate := total / population * 10^5] # := define new column
murders[, ":="(rate = total/population * 10^5, rank = rank(population))]
head(murders)

# saving memory
x <- data.table(a=1)
y <- x # y is NOT a new table, just a reference to x
# If you change x, y will also change. Vice versa change to y > change to x

# copy to avoid above
x <- data.table(a=1)
y <- copy(x) # new data.table

# filter
murders[rate <= 0.7]

# filter + select
murders[rate <= 0.7, .(state,rate)]

# summarize
data(heights)
heights <- setDT(heights)

s <- heights[sex == "Female", # filter
             .(average = mean(height), standard_dev = sd(height))] # summarize

# multiple summaries
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

heights[, .(median_min_max(height))]

# group and summarize using by
heights[, .(avg = mean(height), std = sd(height)), by = sex]

# sorting
murders[order(population, decreasing = TRUE)]
murders[order(region, rate)]

# tibbles tbl is type of df
# select, filter, mutate, arrange preserve class of input

as_tibble(murders) # Difference 1: prints cleaner than df

data(murders)
class(murders[,2]) # Difference 2: df subset might not be a df
class(as_tibble(murders)[,1]) # tibbles subset always df > useful for df functions
class(as_tibble(murders)$state) # using accessor gives vector class

murders$State
as_tibble(murders)$State # Difference 3: tibbles gives you a more specific warning

# Difference 4: can have complex entries (like functions)
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

### Quiz 2.1
data(heights)
options(digits = 3)

mean <- mean(heights$height)
ind <- which(heights$height > mean)
length(ind)

length(which(heights$height > mean & heights$sex == "Female"))

length(which(heights$sex == "Female"))/length(heights$sex)

min <- min(heights$height)
match(min, heights$height)

select(heights, sex)

heights_DT <- setDT(heights)
heights_DT[1032, 1]

max(heights$height)
x <- min(heights$height): max(heights$height)

sum(!x %in% heights$height)


heights <- mutate(heights, ht_cm = height * 2.54)
heights[18, 3]
mean(heights$ht_cm)

heights2 <- filter(heights, sex == "Female")

mean(heights2$ht_cm)
