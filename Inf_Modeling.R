# Sampling Model Parameters and Estimates
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length.out = 100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p * (1-p)/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p, se)

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` 
# for every value of `p` for each of the three samples sizes
# `N` in the vector `sample_sizes`. Plot the three graphs,
# using the `ylim` argument to standardize the y-axis across 
# all three plots.
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
}

# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. 
# Print this value to the console.
se <- 2 * sqrt(p*(1-p)/N)
se

# The Central Limit Theorem in Practice
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

# Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
# Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)
  
  # Code: Plotting margin of error in an extremely large poll over a 
  # range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()
  
# Write a function called `take_sample` that takes `p` and `N` as arguements
# and returns the average value of a randomly sampled population.
take_sample <- function(N, p){
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
}
 



# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` 
# randomly selected people from a population containing a proportion of 
# Democrats equal to `p`. Print this value to the console.
take_sample(N, p)
print(take_sample)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to
# be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the 
# expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the
# result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, p - take_sample(p, N))

print(mean(errors))

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion
# of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.

mean(abs(errors))

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Calculate the standard deviation of `errors`
errors <- errors^2
mean(errors)
sqrt(mean(errors))

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p * (1 - p)/N)

# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
x <- sample(c(1,0), size = N, replace = TRUE, prob = c(p, 1 - p))



# Define `X_bar` as the average sampled proportion
X_bar <- mean(x)

# Calculate the standard error of the estimate. Print the result to the console.
print(sqrt(X_bar * (1 - X_bar)/N))

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)

plot(se, N)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors) 
qqline(errors, distribution = qnorm, probs = c(p, 1- p))

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats
# in the population is greater than 0.5. Print this value to the console.
1 - pnorm(0.5, p, sqrt(p*(1-p)/N))

# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1 - (pnorm(0.01/se_hat) - pnorm(-0.01/se_hat))

# Load the data
library(dslabs)
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that
# ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.")


# How many rows does `polls` contain? Print this value to the console.
print(nrow(polls))

# Assign the sample size of the first poll in `polls` to a variable called `N`.
N <- polls[1, 6]
print(N)

# For the first poll in `polls`, assign the estimated percentage of Clinton
# voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls[1, 8]/100
print(X_hat)

# Calculate the standard error of `X_hat` and save it to a variable called
# `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat * (1 - X_hat)/N)
print(se_hat)


# Use `qnorm` to calculate the 95% confidence interval for the proportion 
# of Clinton voters. Save the lower and then the upper confidence interval 
# to a variable called `ci`.
ci <- c(X_hat - (qnorm(0.975) * se_hat), X_hat + (qnorm(0.975) * se_hat))

# Confidence Intervals
# Code: geom_smooth confidence interval example
# The shaded area around the curve is related to the concept of confidence
# intervals.

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

# Code: Monte Carlo simulation of confidence intervals
# Note that to compute the exact 95% confidence interval, we would use
# qnorm(.975)*SE_hat instead of 2*SE_hat.

p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

# Code: Solving for  with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct pr

q <- 0.95
z <-1 - ((1 - q)/2)
qnorm(q)

# The upper limit of this 95% confidence interval will be 
# X_bar + qnorm(.975)*SE_hat, which removes the 2.5% highest observations.

# The lower limit of this 95% confidence interval will be
# X_bar - qnorm(.975)*SE_hat, which removes the 2.5% lowest observations.

# Code: Monte Carlo simulation
# Note that to compute the exact 95% confidence interval, we would use
# qnorm(.975)*SE_hat instead of 2*SE_hat.

B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - (qnorm(0.975)*SE_hat), X_hat + (qnorm(0.975)*SE_hat))    # TRUE if p in confidence interval
})
mean(inside)

# The 95% confidence intervals are random, but p is not random.
# 95% refers to the probability that the random interval falls on top of p.
# It is technically incorrect to state that  has a 95% chance of being 
# in between two values because that implies p is random.

# Power
# If we are trying to predict the result of an election, then a confidence
# interval that includes a spread of 0 (a tie) is not helpful.
# A confidence interval that includes a spread of 0 does not imply a close
# election, it means the sample size is too small.
# Power is the probability of detecting an effect when there is a true 
# effect to find. Power increases as sample size increases, because larger
# sample size means smaller standard error.

# Code: Confidence interval for the spread with sample size of 25
# Note that to compute the exact 95% confidence interval, we would use
# c(-qnorm(.975), qnorm(.975)) instead of 1.96.

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

# p values
# The null hypothesis is the hypothesis that there is no effect. 
# In this case, the null hypothesis is that the spread is 0, or p = 0.5.
# 
# The p-value is the probability of detecting an effect of a certain size
# or larger when the null hypothesis is true.
# 
# We can convert the probability of seeing an observed value under the null
# hypothesis into a standard normal random variable. We compute the value of  
# z that corresponds to the observed result, and then use that z to compute 
# the p-value.
# 
# If a 95% confidence interval does not include our observed value, then the
# p-value must be smaller than 0.05.
# 
# It is preferable to report confidence intervals instead of p-values, 
# as confidence intervals give information about the size of the estimate and
# p-values do not.

# Code: Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

(2 * 0.52) - 1

# The `polls` object that filtered all the data by date and nation has 
# already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for 
# pollster name, end date, X_hat, se_hat, lower confidence interval, 
# and upper confidence interval for each poll.
pollster_results <- polls %>% 
  mutate(X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
         lower = (X_hat - (qnorm(0.975) * se_hat)), 
         upper = X_hat + (qnorm(0.975) * se_hat)) %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper)

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the 
# confidence interval of each poll. Summarize the average `hit` result to determine the proportion 
# of polls with confidence intervals include the actual value. Save the result as an object called
# `avg_hit`.

avg_hit <- pollster_results %>% 
mutate(hit = lower <= 0.482 & upper >= 0.482) %>% 
  summarize(mean(hit))

pnorm(0.314)
# Sample code
df %>%
  mutate(value = case_when(points <= 102 & rebounds <=45 ~ 2,
                           points <=215 & rebounds > 55 ~ 4,
                           points < 225 & rebounds < 28 ~ 6,
                           points < 325 & rebounds > 29 ~ 7,
                           points >=25 ~ 9))
# player position points rebounds value
# 1     P1        A    102       22     2
# 2     P2        B    215       12     6
# 3     P3        A    319       19     9
# 4     P4        B    125       23     6
# 5     P5        B    112       36     7

library(dslabs)
library(tidyverse)
data(polls_us_election_2016)

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.") %>% 
  mutate(d_hat = (rawpoll_clinton - rawpoll_trump)/100)


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1, 6]
print(N)

# Assign the difference `d_hat` of the first poll in `polls` to a variable
# called `d_hat`. Print this value to the console.
d_hat <- polls[1, 16]
print(d_hat)


# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (1 + d_hat)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. 
# Print this value to the console.
se_hat <- 2 * (sqrt(X_hat * (1 - X_hat)/N))
print(se_hat)


# Use `qnorm` to calculate the 95% confidence interval for the difference in
# the proportions of voters. Save the lower and then the upper confidence 
# interval to a variable called `ci`.
ci <- c(d_hat - (qnorm(0.975) * se_hat), d_hat + (qnorm(0.975) * se_hat))
qnorm(0.975)

# The subset `polls` data with 'd_hat' already calculated has been loaded. 
# Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for 
# pollster name, end date, d_hat, lower confidence interval of d_hat, 
# and upper confidence interval of d_hat for each poll.
polls <- polls %>% 
  mutate(X_hat = (1 + d_hat)/2, se_hat = 2 * (sqrt(X_hat * (1 - X_hat)/samplesize)),
         lower = d_hat - (qnorm(0.975) * se_hat), upper =  d_hat + (qnorm(0.975) * se_hat))
pollster_results <- polls %>% 
  select(pollster, enddate, d_hat, lower, upper)

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value
# (0.021) exists within the confidence interval of each poll. Summarize the
# average `hit` result to determine the proportion of polls with confidence 
# intervals include the actual value. Save the result as an object called 
# `avg_hit`.
avg_hit <- pollster_results %>% 
  mutate(hit = lower <= 0.021 & upper >= 0.021) %>% 
  summarize(mean(hit))

# The `polls` object has already been loaded. Examine it using the `head` 
# function.
head(polls)

# Add variable called `error` to the object `polls` that contains the 
# difference between d_hat and the actual difference on election day. 
# Then make a plot of the error stratified by pollster.
polls <- polls %>% 
  mutate(errors = d_hat - 0.021)

polls %>% 
  ggplot(aes(pollster, errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the 
# difference between d_hat and the actual difference on election day. 
# Then make a plot of the error stratified by pollster, but only for pollsters
# who took 5 or more polls.
polls %>% 
  group_by(pollster) %>% 
  filter(n() >= 5)
polls %>% 
  ggplot(aes(pollster, error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Poll Aggregators
# Code: Simulating polls
# Note that to compute the exact 95% confidence interval, we would use 
# qnorm(.975)*SE_hat instead of 2*SE_hat.

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls
# Code: Calculating the spread of combined polls
# Note that to compute the exact 95% confidence interval, we would use
# qnorm(.975) instead of 1.96.

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

# Poll data and pollster bias
# Code: Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered
# reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)
# Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# Data drivemodels
# Note that to compute the exact 95% confidence interval, we would use 
# qnorm(.975) instead of 1.96.

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

# Statistical Models
# Exercise 1 - Heights Revisited
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
print(mean(x))

# Calculate the population standard deviation. Print this value to the console.
print(sd(x))

# Exercise 2 - Sample the population of heights
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Calculate the sample average. Print this value to the console.
print(mean(X))

# Calculate the sample standard deviation. Print this value to the console.
print(sd(X))

# Exercise 4 - Confidence Interval Calculation
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value
# to the console.
print (se <- sd(X)/sqrt(N))



# Construct a 95% confidence interval for the population average based
# on our sample. Save the lower and then the upper confidence interval to
# a variable called `ci`.
ci <- c(mean(X) - qnorm(0.975) * se, mean(X) + qnorm(0.975) * se)
B <- 10000
inside <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  between(p, X_hat - qnorm(0.975)*se, X_hat + qnorm(0.975)*se)    # TRUE if p in confidence interval
})
# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated 
# intervals that contain mu

res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- between(mu, mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)    # TRUE if p in confidence interval
})
mean(res)

# Exercise 6 - Visualizing Polling Bias
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% 
  ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point()

# Exercise 13 - Compute the Estimates
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` 
# and a column for `s`, the standard deviation of the spreads
sigma <- polls %>% 
  group_by(pollster) %>% 
  summarize(s = sd(spread))

# Print the contents of sigma to the console
print(sigma)

# Exercise 15 - Calculate the 95% Confidence Interval of the Spreads
# The `polls` data have already been loaded for you. Use the `head` 
# function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, 
# standard deviation, and number of polls for the two pollsters.
res <- polls %>% 
  group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n())
  
# Store the difference between the larger average and the smaller in a
# variable called `estimate`. Print this value to the console.
estimate <- res$avg[2] - res$avg[1]
print(estimate)

# Store the standard error of the estimates as a variable called `se_hat`. 
# Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
print(se_hat)

  
# Calculate the 95% confidence interval of the spreads. Save the
# lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)

# Exercise 16 - Calculate the P-value
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
(1 - pnorm(estimate/se_hat)) * 2

# Exercise 17 - Comparing Within-Poll and Between-Poll Variability
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster,
# mean spread, and standard deviation. Print the contents of this object to
# the console.
var <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread)) 

# Bayesian Statistics
# Code: Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this 
# value to the console.
print(Pr_1 * Pr_2)

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence
# of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given 
# that her two children died with no evidence of physical harm.
# Print this value to the console.
Pr_AB <- (Pr_BA * Pr_A)/Pr_B
print(Pr_AB)

# Exercise 6 - Back to Election Polls
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing 
# the average spread (`avg`) and the standard error (`se`). 
# Print the results to the console.
results <- polls %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(n()))
print(results)

# Estimate the Posterior Distribution
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the 
# object `results`
sigma <- results$se


# Define a variable called `Y` that contains the average in the object `
# results`
Y <-  results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value
# to the console.
B <- sigma^2/(sigma^2 + tau^2)

# Calculate the expected value of the posterior distribution
(B * mu) + (1 - B) * Y

# Standard error of the posterior distribution
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. 
# Print this value to the console.
print(sqrt(1/(1/sigma^2 + 1/tau^2)))

# Exercise 10- Constructing a Credible Interval
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then 
# the upper confidence interval to a variable called `ci`.
ci <- c((1 - B) * Y - qnorm(0.975) * se, (1 - B) * Y + qnorm(0.975) * se)

# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual 
# spread was less than 0 (in Trump's favor). Print this value to the console.
print(pnorm(0, exp_value, se))

# Change the Priors
# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the
# probability of the spread being less than 0
p_calc <- function(tau) {
  B <- sigma^2 / (sigma^2 + tau^2)
  exp_value <- B*mu + (1-B)*Y 
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  prob <- pnorm(0, exp_value, se)
  
}


B <- sigma^2 / (sigma^2 + tau^2)

# Create a vector called `ps` by applying the function `p_calc` across
# values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)

# Election Forecasting
# Code: Definition of results object
# This code from previous videos defines the results object used for empirical
# Bayes election forecasting.

library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

# Code: Computing the posterior mean, standard error, credible interval and 
# probability
# Note that to compute an exact 95% credible interval, we would use qnorm(.975)
# instead of 1.96.

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

# Mathematical Representations of Models
# Simulated data with Xj = d + Ej
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

# Simulated data with Xi,j = d + Ei,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

# Simulated data with Xi,j = d + Hi + Ei,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

# Code: Calculating probability of  with general bias
# Note that sigma now includes an estimate of the variability due
# to general bias sigmab = 0.025.
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

# Code: Top 5 states ranked by electoral votes
# The results_us_election_2016 object is defined in the dslabs package:
  
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% 
  arrange(desc(electoral_votes)) %>% 
  top_n(5, electoral_votes)

# Code: Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

# Code: Calculating the posterior mean and posterior standard error
# Note there is a small error in the video code: B should be defined
# as sigma^2/(sigma^2 + tau^2).

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
# Code: Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Code: Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# Forecasting
# Code: Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

# Code: Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

# Code: Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

# Exercise 1 - Confidence Intervals of Polling Data
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports
# the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the 
# instructions
cis <-  polls %>% 
  mutate(X_hat = (spread + 1)/2, 
         se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = spread - qnorm(0.975) * se,
         upper = spread + qnorm(0.975) * se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Exercise 2 - Compare to Actual Results
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of 
# confidence intervals that contain the actual value. 
# Print this object to the console.
p_hits <- ci_data %>% 
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>% 
  summarize(proportion_hits = mean(hit))
print(p_hits)

interval <- between(mu, mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)

# Exercise 3 - Stratify by Pollster and Grade
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of 
# hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% 
  mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>% 
  filter(n() >= 5) %>% 
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>% 
  arrange(desc(proportion_hits))
  
# Exercise 4 - Stratify by State  
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits
# for each state that has more than 5 polls.  
p_hits <- ci_data %>% 
  mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(state) %>% 
  filter(n() > 5) %>% 
  summarize(proportion_hits = mean(hit), n = n()) %>% 
  arrange(desc(proportion_hits))

# Exercise 5- Plotting Prediction Results
# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% 
  arrange(desc(proportion_hits))

 
ggplot(aes(p_hits$state, p_hits$proportion_hits)) +
geom_bar(stat = "identity") +
coord_flip()

# Exercise 6 - Predicting the Winner
# The `cis` data have already been loaded. Examine it using the `head` function.
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
cis <-  cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")
head(cis)

# Create an object called `errors` that calculates the difference between
# the predicted and actual spread and indicates if the correct winner was 
# predicted
errors  <- cis %>% mutate(error = actual_spread - spread, hit = sign(spread) == sign(actual_spread))



# Examine the last 6 rows of `errors`
tail(errors)

# Exercise 7 - Plotting Prediction Results
library(tidyverse)

p_hits <- errors %>% 
  group_by(state) %>% 
  filter(n() >= 5) %>% 
  summarize(proportion_hits = mean(hit), n = n())

p_hits %>% 
  mutate(state = reorder(state, proportion_hits)) %>% 
  ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Exercise 8 - Plotting the Errors
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)


# Calculate the median of the errors. Print this value to the console.
print(median(errors$error))
  

# The `errors` data have already been loaded. Examine them using the
# `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades
# B+ or higher
errors %>% 
  filter(grade %in% c("A+", "A", "A-", "B+") | is.na(grade)) %>% 
  mutate(state = reorder(state, error)) %>% 
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()

# Exercise 10 - Filter Error Plot
# The `errors` data have already been loaded. Examine them using the
# `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least
# 5 polls with grades B+ or higher
errors %>% 
  filter(grade %in% c("A+", "A", "A-", "B+") | is.na(grade)) %>%
  group_by(state) %>% 
  filter(n() >= 5) %>% 
  ungroup() %>% 
  mutate(state = reorder(state, error)) %>% 
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()

# The t-Distribution
# Code: Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

# Exercise 1 - Using the t-Distribution
# Calculate the probability of seeing t-distributed random variables
# Calculate the probability of seeing t-distributed random variables 
# being more than 2 in absolute value when 'df = 3'.being more than 2 in 
# absolute value when 'df = 3'.
(1 - abs(pt(2 , 3))) + abs(pt(-2, 3))
 
# Exercise 2 - Plotting the t-distribution
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3, 50, 1)

# Make a function called 'pt_func' that calculates the probability that
# a value is more than |2| for any degrees of freedom
pt_func <- function(df){
  (1 - abs(pt(2 , df))) + abs(pt(-2, df))
}


# Generate a vector 'probs' that uses the `pt_func` function to calculate
# the probabilities
probs <- sapply(df, pt_func)


# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

# Exercise 3 - Sampling From the Normal Distribution
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and
# the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected
# result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the
# simulations
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- between(mu, mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)    # TRUE if p in confidence interval
})

# Calculate the proportion of times the simulation produced values
# within the 95% confidence interval. Print this value to the console.
mean(res)

# Exercise 4 - Sampling from the t-Distribution
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the 
# simulations using the t-distribution
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  df <- N - 1
  interval <- mean(X) + c(-1,1)*qt(0.975, df)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within 
# the 95% confidence interval. Print this value to the console.
print(mean(res))

# Code: Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

# Code: Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# Code: Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

# Code: Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# Code: p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing
# odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")




# The 'errors' data have already been loaded. Examine them using the 
# `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and
# bad predictions for polls rated A- and C-
totals <- errors %>% 
  filter(grade %in% c("A-", "C-")) %>%  
  group_by(grade, hit) %>% 
  summarize(num = n()) %>% 
  spread(grade, num)



# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])

# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[[2]])

# Exercise 2 - Chi-squared Test
# The 'totals' data have already been loaded. Examine them using the `head`
# function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an 
# object called 'chisq_test'.
chisq_test <- totals %>% 
  select(-hit) %>% 
  chisq.test()

# Print the p-value of the chi-squared test to the console
print(chisq_test$p.value)

# Exercise 3 - Odds Ratio Calculation
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)
totals <- totals %>% 
  

# Generate a variable called `odds_C` that contains the odds of getting
# the prediction right for grade C- polls
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))

# Generate a variable called `odds_A` that contains the odds of getting
# the prediction right for grade A- polls
odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))

# Calculate the odds ratio to determine how many times larger the odds
# ratio is for grade A- polls than . C- polls
odds_A/odds_C

# Brexit poll analysis - Part 1
# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Question 1: Expected value and standard error of a poll
N <- 1500
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)  
EV <-  N*p #expected value of voters
SE_S <- sqrt(N*p*(1-p)) #standard error on the sum of remain voters
SE_X_bar <- sqrt((X_hat*(1-X_hat))/N) #standard error of the sample.

X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean
EV_d <- (1 + d)/2
SE_spread <- 2*(SE_hat)

# Answers
# Expected value of voters
EV <- N*p

# Sample remain voters
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)  

# Standard Error of the samples
SE_S <- sqrt(N*p*(1-p))

# Expected value of X hat = p

# Standard error of X hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)

# Expected value of d
d_hat <- 2*p-1

# Expected error of the spread
SE_spread <- 2*(SE_hat)

# Question 2: Actual Brexit poll estimates
brexit_polls
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

mu_spread <- mean(brexit_polls$spread)
sd_spread <- sd(brexit_polls$spread)
mu_x_hat <- (mu_spread + 1)/2
sd_x_hat <- sd(brexit_polls$x_hat)

# Question 3: Confidence interval of a Brexit poll

poll <- as.data.frame(brexit_polls[1, ])
se <- poll %>% 
  sqrt(x_hat*(1-x_hat)/samplesize)
se <- sqrt(0.52*(1-0.52)/4772)
ci <- c(0.52 - (qnorm(0.975) * se), 0.52 + (qnorm(0.975) * se))

# Brexit poll analysis - Part 2
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
d <- -0.038
# Question 4: Confidence intervals for polls in June
june_polls <- brexit_polls %>% 
  filter(enddate >= "2016-06-01")
nrow(june_polls)

june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         lower = spread - qnorm(0.975)*se_spread,
         upper = spread + qnorm(0.975)*se_spread)
  
ci_zero <- june_polls %>% 
  summarize(mean(lower < 0 & upper > 0))

ci_plus <- june_polls %>% 
  summarize(mean(lower > 0))

june_polls <- june_polls %>%
  mutate(hit = (2*p-1 > lower) & (2*p-1 < upper))
mean(june_polls$hit)

june_polls <- june_polls %>% 
  group_by(pollster) %>% 
  mutate(N = n()) %>% 
  arrange(desc(hit)) %>% 
  summarize(N = N, results = mean(hit))

box_plot %>% 
  ggplot(aes(poll_type, spread)) +
  geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            spread_lower = spread - qnorm(.975)*se_spread,
            spread_upper = spread + qnorm(.975)*se_spread)
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_lower)

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_chisq <- as.tibble(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value

# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05



# Print the proportion of hits for grade A- polls to the console
chi_test[[2,3]]/sum(totals[[3]])

# Print the proportion of hits for grade C- polls to the console
ch[[2,2]]/sum(totals[[2]])

x <- matrix(c(48, 85, 10, 42))
chisq.test(48, 85) 
chisq.test(10, 42)
pnorm(48)
1 - pnorm(10)

#create matrix
poll_type <- c('Online', 'Phone')
outcome <- c('Hit', 'Miss')
data <- matrix(c(48, 37, 10, 32), nrow=2, ncol=2, byrow=TRUE)
dimnames(data) <- list('Poll'= poll_type, 'Outcome'=outcome)

#view matrix
data