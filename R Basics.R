# R Basics, Text book: Introduction to Data Science
# 2.11 Exercises
# The formula for the sum of integers 1 through n is n(n+1)/2
n = 1000
n*(n+1)/2
n <- 1000
x <- seq(1, n)
sum(x)

## Code to compute solution to quadratic equation

## Define the variables
coef_a <- 3 
coef_b <- 2
coef_c <- -1

## Now compute the solution
(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
(-coef_b - sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)

# Based on the result, what do you think the functions seq and
# sum do? You can use help.

# a. sum creates a list of numbers and seq adds them up.
# b. seq creates a list of numbers and sum adds them up.
# c. seq creates a random list and sum computes the sum of
#     1 through 1,000.
# d. sum always returns the same number.
# Answer: b

# Use one line of code to compute the log, in base 10, of the
# square root of 100.
log(100, 10)
log10(100)

# Which of the following will always return the numeric
# value stored in x?

x = 5
log(10^x)
log10(x^10)
log(exp(x)) # correct answer
exp(log(x, base = 2))

library(dslabs)
str(murders)
# The 51 states.
# The murder rates for all 50 states and DC.
# The state name, the abbreviation of the state name,
# the state’s region, and the state’s population and total number of murders for 2010.
# str shows no relevant information.

library(tidyverse)
library(dslabs)
data(murders)

murders %>%
  ggplot(aes(population, total, label=abb, color=region)) +
  geom_label()

# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)

# determining that the murders dataset is of the "data frame" class
class(murders)
# finding out more about the structure of the object
str(murders)
# showing the first 6 lines of the dataset
head(murders)

# using the accessor operator to obtain the population column
murders$population
# displaying the variable names in the murders dataset
names(murders)
# determining how many entries are in a vector
pop <- murders$population
length(pop)
# vectors can be of class numeric and character
class(pop)
class(murders$state)

# logical vectors are either TRUE or FALSE
z <- 3 == 2
z
class(z)

# factors are another type of class
class(murders$region)
# obtaining the levels of a factor
levels(murders$region)

# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]

library(dslabs)
data(murders)
sort(murders$total)

x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in order

index <- order(x)    # returns index that will put x in order
x[index]    # rearranging by this index puts elements in order
order(x)

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]    # order abbreviations by total murders

max(murders$total)    # highest number of total murders
i_max <- which.max(murders$total)    # index with highest number of murders
murders$state[i_max]    # state name with highest number of total murders

x <- c(31, 4, 15, 92, 65)
x
rank(x)    # returns ranks (smallest to largest)

# The name of the state with the maximum population is found by doing the following
murders$state[which.max(murders$population)]

# how to obtain the murder rate
murder_rate <- murders$total / murders$population * 100000

# ordering the states by murder rate, in decreasing order
murders$state[order(murder_rate, decreasing=TRUE)]

# Indexing
# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

# Indexing Functions
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state

# Basic Plots
library(dplyr)
library(dslabs)
data("murders")

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

# Question 1
coef_a <- 2
coef_b <- -1
coef_c <- -4


## Now compute the solution
(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
(-coef_b - sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)

# Question 2
# Use R to compute log base 4 of 1024
log(1024, 4)

# Question 3
# How many rows are in the dataset?
library(dslabs)
data(movielens)
str(movielens)

#  how many levels are in the factor genres in 
#  the movielens data frame.
nlevels(movielens$genre)

# Part 2 Question 1 & 2
x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)

min(x)
which.min(x)
max(x)
which.max(x)

# Question 3
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

# Write a line of code to convert time to hours
hours <- time/60
speed <- distance/hours


x <- c(4, "seven", 9)

# Question 6
library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic, olive$palmitoleic)

# Question 7
hist(olive$eicosenoic)

# Question 8
boxplot(palmitic~region, data = olive)

# Basic Data Wrangling
# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# Create Data Frames
# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

# Summarize Function
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))
s

# accessing the components with the accessor $
s$median
s$maximum

# average rate unadjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# Summarize with more than one value
# The quantile() function can be used to return 
# the min, median, and max in a single line of code.
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region using quantile
# note that this returns a vector
murders %>% 
  filter(region == "West") %>%
  reframe(range = quantile(rate, c(0, 0.5, 1)))

# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

# Pull to access columns
# The pull() function can be used to access values 
# stored in data when using pipes: when a data object
# is piped %<% that object and its columns can be 
# accessed using the pull() function.
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# us_murder_rate is stored as a data frame
class(us_murder_rate)

# the pull function can return it as a numeric value
us_murder_rate %>% pull(rate)

# using pull to save the number directly
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate

# us_murder_rate is now stored as a number
class(us_murder_rate)

# The Dot placeholder
# The dot (.) can be thought of as a placeholder for 
# the data being passed through the pipe.
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# using the dot to access the rate
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate
class(us_murder_rate)

# Group then Summarize
# Splitting data into groups and then computing 
# summaries for each group is a common operation in 
# data exploration.
# We can use the dplyr group_by() function to create a
# special grouped data frame to facilitate such 
# summaries.
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# group by region
murders %>% group_by(region)

# summarize after grouping
murders %>% 
  group_by(region) %>%
  summarize(median = median(rate))

# Sorting Data Tables
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# order the states by population size
murders %>% arrange(population) %>% head()

# order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states by murder rate
murders %>% top_n(10, rate)

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)

# introduction to data.table
# install the data.table package before you use it!
install.packages("data.table")

# load data.table package
library(data.table)

# load other packages and datasets
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

# convert the data frame into a data.table object
murders <- setDT(murders)

# selecting in dplyr
select(murders, state, region)

# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))]

# y is referring to x and := changes by reference
x <- data.table(a = 1)
y <- x

x[,a := 2]
y

y[,a := 1]
x

# use copy to make an actual copy
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y

# Subsetting with data.table
# Subsetting in data.table uses notation similar 
# to that used with matrices.
# load packages and prepare the data
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
library(data.table)
murders <- setDT(murders)
murders <- mutate(murders, rate = total / population * 10^5)
murders[, rate := total / population * 100000]

# subsetting in dplyr
filter(murders, rate <= 0.7)

# subsetting in data.table
murders[rate <= 0.7]

# combining filter and select in data.table
murders[rate <= 0.7, .(state, rate)]

# combining filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state, rate)

# Summarizing with data.table
# In data.table we can call functions inside .() and
# they will be applied to rows.
# The group_by followed by summarize in dplyr is 
# performed in one line in data.table using the by 
# argument.
# load packages and prepare the data - heights dataset
library(tidyverse)
library(dplyr)
library(dslabs)
data(heights)
heights <- setDT(heights)

# summarizing in dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))

# summarizing in data.table
s <- heights[, .(average = mean(height), standard_deviation = sd(height))]

# subsetting and then summarizing in dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# subsetting and then summarizing in data.table
s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]

# previously defined function
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]

# Sorting data frames
# load packages and datasets and prepare the data
library(tidyverse)
library(dplyr)
library(data.table)
library(dslabs)
data(murders)
murders <- setDT(murders)
murders[, rate := total / population * 100000]

# order by population
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] 

# order by region and then murder rate
murders[order(region, rate)]

# Tibbles
# view the dataset
data(murders)
murders %>% group_by(region)

# see the class
murders %>% group_by(region) %>% class()

# compare the print output of a regular data frame to a tibble
gapminder
as_tibble(gapminder)

# compare subsetting a regular data frame and a tibble
class(murders[,1])
class(as_tibble(murders)[,1])

# access a column vector not as a tibble using $
class(as_tibble(murders)$state)

# compare what happens when accessing a column that doesn't exist in a regular data frame to in a tibble
murders$State
as_tibble(murders)$State

# create a tibble
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

# Section 2 Quiz, Part 1
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

h <- heights %>% 
  summarize(avg = mean(height))
ind <- heights %>%
  filter(height > 68.3)
ind_f <- ind %>% 
  filter(sex == "Female")
31/532
h_f <- heights %>% 
  filter(sex == "Female")
238/1050

# 4a
h_min <- heights %>% 
  summarize(minimum = min(height))

# 4 b
index <- match(50, heights$height)

# 5A
h_max <- heights %>% 
  summarize(maximum = max(height))

# 5B
x <- h

# 6
heights2 <- heights %>%
  mutate(ht_cm = height*2.54)
heights2[18, ]
mean(heights2$ht_cm)

heightsF <- heights2 %>%
  filter(sex == "Female")
mean(heightsF$ht_cm)

data(murders)
which(murders$state == "Massachusetts")
match(c("Massachusetts"), murders$state)
c("Massachusetts") %in% murders$state
which(murders$state = "Massachusetts")

filter(murders, region == "Northeast")
murders %>% filter(region == "Northeast")
murders %>% select(region == "Northeast")
murders %>% filter(murders, region == "Northeast")
select(murders, region == "Northeast")

ind %in% murders$state
!ind %in% murders$state # correct
# ind !%in% murders$state
!match(ind, murders$state)
match(!ind, murders$state)

# Conditionals
# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}

# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}

# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

# Functions
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

# For Loops
# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a very simple for-loop
for(i in 1:5){
  print(i)
}

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)

# Section 3 Quiz
# Question 1
library(dslabs)
data(heights)
sex <- ifelse(heights$sex == "Female", 1, 2)
sum(sex)

# Question 2
tall <- ifelse(heights$height > 72, heights$height, 0)
mean(tall)

# Question 3
x <- (heights$height)
inches_to_feet <- function(x){
  feet <- (x * 12)
  short <- (x < 5)
}
sum(x)
print(short)

# Question 4
m <- 10
f_n <- vector(length = m)
for(n in 1:m){
  f_n[n] <- factorial(n)
}
f_n

# Question 6 
n <- 13
ifelse(n > 10, n, "None")

# Question 7
x <- 25
s <- 5
test <- function(x){
  s <- 1/x
}

# Question 8
test <- vector(length = 5)
for (i in 1:5){
  test[i] <- i^2
}
test

# Copy the spreadsheet containing the US murders data (included as part of the dslabs package) 

filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")

# Once the file is copied, import the data with a line of code. Use the read_csv function from the readr package (included in the tidyverse)
library(tidyverse)
dat <- read_csv(filename)

# See an example of a full path on your system 
system.file(package = "dslabs")

# Use the function list.files to see examples of relative paths
dir <- system.file(package = "dslabs")
list.files(path = dir)

# Get the full path of your working directory by using the getwd function
wd <- getwd()

# Obtain a full path without writing out explicitly 
filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)

# By exploring the directories in dir we find that the extdata contains the file we want
dir <- system.file(package = "dslabs") 
filename %in% list.files(file.path(dir, "extdata"))

# Copy the file to our working directory 
filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")

# Load it directly
library(readr)

# Open the file to take a look or use the function read_lines to look at a few lines
read_lines("murders.csv", n_max = 3)

# Read-in the data into R from the .csv suffix 
dat <- read_csv(filename)

# Confirm that the data has in fact been read-in 
View(dat)

# Use the full path for the file
dat <- read_csv(fullpath)

# Load the readxl package using
library(readxl)

# Our dslabs package is on GitHub and the file we downloaded with the package has a url
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

# Use read_csv file to read these files directly
dat <- read_csv(url)

# Use the download.file function in order to have a local copy of the file
download.file(url, "murders.csv")

# Run a command like this which erases the temporary file once it imports the data
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

# Use scan to read-in each cell of a file
path <- system.file("extdata", package = "dslabs")
filename <- "murders.csv"
x <- scan(file.path(path, filename), sep = ",", what = "c")
x[1:10]
