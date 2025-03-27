### 3.1
# Conditional
if(a!=0){
  print(1/a)
} else {
  print("No reciprocal for 0.")
}

a <- 0
ifelse(a > 0, 1/a, "NA")

a <- c(0, 1, 2, -4, 5)
result <- ifelse(a > 0, 1/a, NA)

z <- c(TRUE, TRUE, FALSE)
any(z) # if any are true
all(z) # if all are true

library(dslabs)
data(murders)
data(na_example)

murder_rate <- murders$total/murders$population * 10^5
ind <- which.min(murder_rate)
if(murder_rate[ind]<0.7){
  print(murders$state[ind])  
} else {
  print("No state has murder rate that low.")
}

no_nas <- ifelse(is.na(na_example), 0, na_example) # ifelse replace NA's with 0

# Functions
avg <- function(x) {
  s <- sum(x) # local to the function
  n <- length(x) # local to the function
  s/n
}

avg <- function(x, arithmetic = TRUE) { # functions can have a default value
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

# Namespaces
# multiple libraries might have same function name
search() # shows what packages
stats::filter # forces specific namespace (stats filter function)
dplyr::filter # forces use of dplyr filter function
# can also use :: if we want a function without loading whole package

### 3.2 For Loops
compute_s_n <- function(n) {
  x <- 1:n
  sum(x)
}

compute_s_n(4)

for (i in 1:5){
  print(i)
}

m <- 25
s_n <- vector(length = m)
for (n in 1:m){
  s_n[n] <- compute_s_n(n)
}

n <- 1:25

plot(n, s_n)
lines(n, n*(n+1)/2)

# Functionals
# Apply family: apply, sapply, tapply, mapply
# Split, Cut, Quantile, Reduce, Identical, Unique
x <- 1:10
sapply(x, sqrt) # element wise operations

# Exercise
data(murders)

new_names <- ifelse(nchar(murders$state) > 8, murders$abb, murders$state)

sum_n <- function(n) {
  sum(1:n)
}

altman_plot <- function(x, y) {
  plot(x-y, x+y)
}

compute_s_n <- function(n) {
  x <- sapply(1:n, function(x) x^2)
  sum(x)
}

s_n <- vector("numeric", length = 25)
for (i in 1:25) {
  s_n[i] <- compute_s_n(i)
}
s_n_sapply <- sapply(1:25, compute_s_n)
s_n_map <- map_dbl(1:25, compute_s_n)

plot(1:25, s_n)
lines(n, n*(n+1)*(2*n+1)/6)

### Quiz
library(dslabs)
data(heights)

sum(ifelse(heights$sex == "Female", 1, 2))

mean(ifelse(heights$height > 72, heights$height, 0))

inches_to_ft <- function(x) {
  x/12
}
sum(ifelse(inches_to_ft(heights$height) < 5, 1, 0))

# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n