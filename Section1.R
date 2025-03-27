### 1.1
library(tidyverse)
library(dslabs)
data("murders")

murders%>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

### 1.2
a <- 1
b <- 1
c <- -1

solution_1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
solution_2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)

# existing functions
2^3
log(8,2)
pi
Inf
sqrt(1000)
signif(sqrt(1000), digits=4)
round(sqrt(1000), digits=3)

# help: shows arguments and description of functions
?log
help("log")

## Exercises
john <- 7
paul <- 12^2
ringo <- paul*john
ringo <- 2*ringo
ringo <- 7+ringo # should be 2023


### 1.3
class(a)
data(murders)
class(murders)

str(murders) # gives us structure of df
names(murders) # gives column names of df
head(murders) # gives us first 6 lines of df
table(murders$region) # gives frequency of each category

murders$population # $ is an accessor. This gives us a numeric vector
pop <- murders$population
length(pop) # length of the vector
class(pop) # numeric

murders[25, 1] # 25th row, 1st column
murders[2:3,] # rows 2 to 3

# quotes show character
class(murders$state)

# logical vector: True or False
z <- 3 == 2
class(z)

# factor object type: for categorical data
class(murders$region) # factor
levels(murders$region) # NE, S, NC, W

# list
record <- list(name = "John Doe",
               student_id = 1234,
               grades = c(95, 82, 91, 97, 93),
               final_grade = "A")

record$student_id # 1234
record[["student_id"]] #1234

  # if a list doesn't have variable names, you can only use [[]] to extract data
record2 <- list("John Doe", 1234)
record2[[1]] # can't use $ as an accessor

# matrices: entries all the same type, can perform matrix operations
mat <- matrix(1:12, 4, 3)
mat[2, 3] # mat[row,column]
mat[2,]
mat[,2:3]
mat[1:2, 2:3]

mat_df <- as.data.frame(mat) # convert to dataframe

# Exercise
str(murders) # c. shows state name, abb, region, pop, and total murders
names(murders) # column names state, abb, region, population, total
a <- murders$abb
class(a) # character object
b <- murders[,2]
a==b # these are the same
length(levels(murders$region)) # there are 4 categories
table(murders$region)

### 1.4 Vectors
country <- c("a", "b", "c") # c means concatenate
codes <- c(Italy = 1, Canada = 30) # you can name the entries of a vector

num <- seq(1,10) # seq gives us a sequence
jump_num <- seq(1,10,2) #seq odd numbers

# subsetting
codes[1:2]
codes["Canada"]
codes[c("Italy", "Canada")]

# coercion
x <- c(1, "canada", 3) # R coerced 1 and 3 to character strings
x

x <- 1:5
y <- as.character(x)
x <- as.numeric(y)

x <- c("1", "b", "3")
as.numeric(x) # NA is missing data, here because can't coerce

# Exercise
temp <- c(Beijing = 35, Lagos = 88, Paris = 42, RiodeJaneiro = 84,
          SanJuan = 81, Toronto = 30)
city <- names(temp)
temp[1:3]
temp[c("Paris", "SanJuan")]
seq(12,73)
seq(1,100,2)
length(seq(6,55,4/7))
class(seq(1,10,0.5))
class(seq(1,10))
class(1L) # L forces an integer class instead of default numeric
class(1)
x <- c("1","3","5")
as.integer(x)

### 1.5 Sorting
sort(murders$total)

x <- c(31, 4, 15, 92, 65)
sort(x) # sorts the entries
order(x) # the index that would put things in order

index <- order(murders$total)
murders$abb[index] # the indexes of total in order

max(murders$total)
i_max <- which.max(murders$total)
murders$abb[i_max]

rank(x) # gives you the rank of which smallest each entry is

x <- c(1,2,3)
y <- c(10, 20, 30, 40, 50)
x + y # R recycles the values in x because it is shorter in length

# Exercise
pop <- murders$population
sort_pop <- sort(pop)
sort_pop[1]

ord_pop <- order(pop)
ord_pop[1]

which.min(pop)

states <- murders$state
states[which.min(pop)]

ranks <- rank(murders$population)
my_df <- data.frame(name = murders$state, rank = ranks)

index <- order(murders$population)
my_df <- data.frame(name = murders$state[index], rank = ranks[index])

data("na_example")
str(na_example)
mean(na_example)

ind <- is.na(na_example)
table(ind)

x <- na_example[is.na(na_example) != TRUE]
mean(x)

### 1.6 VECTOR ARITHMETIC
murders$state[which.max(murders$population)]
max(murders$population)

# vector math element wise/entry by entry
inch <- c(23, 24, 12, 64, 23)
cm <- inch * 2.54
diff_from_25 <- inch - 25

murder_rate <- murders$total/murders$population*100000
murders$state[order(murder_rate,decreasing=TRUE)]

# Exercise
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", 
          "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

temp_C = 5/9 * (temp-32)

sum(1/seq(1:100)^2)
pi^2/6

murder_rate <- murders$total/murders$population*100000
mean(murder_rate)

### 1.7 Indexing
# logical operators
index <- murder_rate <= 0.71
murders$state[index]
sum(index) # TRUE coerced to 1, FALSE coerced to 0

west <- murders$region == "West"
safe <- murder_rate <= 1

index <- safe & west
murders$state[index]

# which
ind <- which(murders$state == "California")
murder_rate[ind]

# match: which indexes of second vector match first vetor
ind <- match (c("New York", "Florida", "Texas"), murders$state)
murder_rate[ind]

# %in%: whether or not first vector element is in second vector
c("Boston", "Dakota", "Washington") %in% murders$state # FALSE FALSE TRUE

# Exercise
murder_rate <- murders$total/murders$population*100000
low <- murder_rate < 1

ind <- which(low == TRUE)

murders$state[ind]

NE <- murders$region == "Northeast"
ind <- which(low & NE)
murders$state[ind]

lower <- murder_rate < mean(murder_rate)
sum(lower)
murders$state[which(lower)]

ind_abb <- match(c("AK", "MI", "IA"), murders$abb)
murders$state[ind_abb]

test_abb <- c("MA", "ME", "MI", "MO", "MU")
real_abb <- test_abb %in% murders$abb
test_abb[which(!real_abb)]

### 1.8 Basic Plots
# Scatter Plot
x <- murders$population/10^6
y <- murders$total
plot(x,y)

with(murders, plot(population,total))

# Histogram
x <- with(murders, total/population*100000)
hist(x)

# Boxplot
murders$rate <- with(murders, total/population*100000)
boxplot(rate~region, data = murders)

# Image
x <- matrix(1:120, 12, 10)
image(x)

# Exercise
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

plot(log(population_in_millions), log(total_gun_murders))

hist(with(murders, population))

boxplot(population~region, data=murders)

### Quiz 1
# Part 1
a <- 2
b <- -1
c <- -4

(-b + sqrt(b^2-4*a*c))/(2*a)
(-b - sqrt(b^2-4*a*c))/(2*a)

log(1024, 4)

data(movielens)
str(movielens)

class(movielens$title)
class(movielens$genres)

nlevels(movielens$genres)

# Part 2
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_hour <- time/60
speed <- distance/time_hour

time_hour[which(name=="Olivia")]
speed[which(name=="Mandi")]
name[which.max(speed)]

data(olive)
head(olive)

with(olive, plot(palmitic,palmitoleic))
hist(olive$eicosenoic)
boxplot(palmitic~region, data=olive)

table(olive$region)
