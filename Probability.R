# Probability
# Monte Carlo Simulations
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

# 3 cyan balls, 5 magenta balls, and 7 yellow balls.
3+5+7
cyan <- 3
magenta <- 5
yellow <- 7

cyan + magenta + yellow
# What is the probability that the ball will be cyan?
3/15
cyan/(cyan + magenta + yellow)
# What is the probability that the ball will not be cyan?
12/15

(magenta + yellow)/(cyan + magenta + yellow)
# What is the probability that the first draw is cyan and 
# that the second draw is not cyan?
(3/15)*(12/14)

# sampling with replacement.
# What is the probability that the first draw is cyan and that
# the second draw is not cyan?
(3/15)*(12/15)

# Combinations and Permutations
# paste() joins two strings and inserts a space in between.
# expand.grid() gives the combinations of 2 vectors or lists.
# permutations(n,r) from the gtools package lists the 
# different ways that r items can be selected from a set of 
# n options when order matters.
# combinations(n,r) from the gtools package lists the 
# different ways that r items can be selected from a set of
# n options when order does not matter.

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), 
            shirt = c("white", "grey", "plaid"))

# Code: Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six",
             "Seven", "Eight", "Nine", "Ten", "Jack",
             "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

# Code: Permutations and combinations
library(gtools)
# ways to choose 2 numbers in order from 1:5
permutations(5,2)    
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

# Code: Probability of drawing a second king given that 
# one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings)/ sum(first_card %in% kings)

# Code: Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed
# first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first 
# and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# Code: Monte Carlo simulation of natural 21 in blackjack
# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)})
mean(results)

# Code: The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50
# generate n random birthdays
bdays <- sample(1:365, n, replace = TRUE) 
# check if any birthdays are duplicated
any(duplicated(bdays))    

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

# Code: Function for birthday problem Monte Carlo simulations
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

# Code: Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

# Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

# How many Monte Carlo experiments are enough?
# Code: Estimating a practical value of B
# This code runs Monte Carlo simulations to estimate the 
# probability of shared birthdays using several B values 
# and plots the results. When B is large enough that the 
# estimated probability stays stable, then we have selected 
# a useful value of B.
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

# Celtics Win
# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two 
# steps for B iterations: (1) generating a random four-game 
# series `simulated_games` using the example code, then (2)
# determining whether the simulated series contains at 
# least one win for the Celtics.
any(simulated_games == "win")


# Monty Hall
# Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

# Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

# The Cavs and the Warriors
n <- 6
outcomes <-c(0, 1)
l <- rep(list(outcomes), 6)
possibilities <- as.data.frame(expand.grid(l))
results <- rowSums(possibilities)
print(mean(results >= 4))

# The Cavs and the Warriors - Monte Carlo
B <- 10000
set.seed(1)
results <- replicate(B, {
  outcomes <- sample(c(0,1), 6, replace = TRUE)
  })
any(colSums(results) >= 4)
series <- colSums(results)
print(mean(colSums(results) >= 4))

# Play a series

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the 
# underdog team B can be computed with the following function based on a 
# Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
Pr <- sapply(p, prob_win)
plot(p, Pr)

# Exercise 4. A and B play a series - part 2
N <- seq(1, 25, by = 2)
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}
Pr <- sapply(N, prob_win)
plot(N, Pr)

# 1.4 Assessment: Discrete Probability
library(gtools)
library(tidyverse)

# How many different ways can the 3 medals be distributed 
# across 8 runners?
runners <- permutations(8, 3)

# How many different ways can the three medals be 
# distributed among the 3 runners from Jamaica?
jamaicans <- permutations(3, 3)

# What is the probability that all 3 medals are
# won by Jamaica?
6/336

# Run a Monte Carlo simulation on this vector representing
# the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", 
             "Ecuador", "Netherlands", "France", 
             "South Africa")
set.seed(1)
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica"
    & winners[3] %in% "Jamaica")
})
mean(results)

# meal combos
sides <- combinations(6,2)
mealcombos <- expand.grid(entree = c(1:6), 
                          sides = c(1:15),
                          drink = c(1,2))

# Add drink option
mealcombos_2 <- expand.grid(entree = c(1:6), 
                          sides = c(1:15),
                          drink = c(1:3))

# Add 3rd side
sides <- combinations(6, 3)
mealcombos_3 <- expand.grid(entree = c(1:6), 
                            sides = c(1:20),
                            drink = c(1:3))
# of entrees 
meals <- function(entree){
  (3*15*entree)
}
entree <- seq(1, 12)
Pr_meals <- as.data.frame(sapply(entree, meals))

# of sides
meals <- function(sidechoice){
  (3*6*sidechoice)
}
n <- 6
sidechoice <- combinations(n, 2)
Pr_meals <- as.data.frame(sapply(sidechoice, meals))
n <- seq(1, 12)
replicate(combinations(n, 2))
3*6*21

# Questions 3 and 4: Esophageal cancer and 
# alcohol/tobacco use, part 1
head(esoph)
esoph <- as.data.frame(esoph)

# How many cases are there?
all_cases <- sum(esoph$ncases)
# How many controls are there?
all_controls <- sum(esoph$ncontrols)

# What is the probability that a subject in the highest 
# alcohol consumption group is a cancer case?
highalc <- filter(esoph, alcgp == "120+")
Pr_highalc <- sum(highalc$ncases)/ (sum(highalc$ncases) 
                                    + sum(highalc$ncontrols))

# What is the probability that a subject in the lowest
# alcohol consumption group is a cancer case?
lowalc <- filter(esoph, alcgp == "0-39g/day")
Pr_lowalc <- sum(lowalc$ncases)/ (sum(lowalc$ncases) + sum(lowalc$ncontrols))

# Given that a person is a case, what is the probability
# that they smoke 10g or more a day?
case_smoker <- filter(esoph, tobgp > "0-9g/day")
smoke_10g <- sum(case_smoker$ncases)/all_cases

# Given that a person is a control, what is the probability
# that they smoke 10g or more a day?
cont_10g <- sum(case_smoker$ncontrols)/all_controls

# For cases, what is the probability of being in the 
# highest alcohol group?
Pr_case_highalc <- sum(highalc$ncases)/all_cases

# For cases, what is the probability of being in the
# highest tobacco group?
high_tob <- filter(esoph, tobgp == "30+")
Pr_high_tob <- sum(high_tob$ncases)/all_cases

# For cases, what is the probability of being in the 
# highest alcohol group and the highest tobacco group?
highall <- filter(esoph, alcgp == "120+" & tobgp == "30+")
Pr_highall <- sum(highall$ncases)/all_cases

# For cases, what is the probability of being in the highest
# alcohol group or the highest tobacco group?
highall_or <- filter(esoph, alcgp == "120+" | tobgp == "30+")
Pr_highall_or <- sum(highall_or$ncases)/all_cases

# For controls, what is the probability of being in the
# highest alcohol group?
Pr_highalc_cont <- sum(highalc$ncontrols)/all_controls

# For controls, what is the probability of being in
# the highest tobacco group?
tob_cont <- filter(esoph, tobgp == "30+")
Pr_tob_cont <- sum(tob_cont$ncontrols)/all_controls

# For controls, what is the probability of being in the 
# highest alcohol group and the highest tobacco group?
highall_cont <- filter(esoph, alcgp == "120+" & tobgp == "30+")
Pr_6d <- sum(highall_cont$ncontrols)/all_controls

# For controls, what is the probability of being in the 
# highest alcohol group or the highest tobacco group?
dat_6e <- filter(esoph, alcgp == "120+" | tobgp == "30+")
Pr_dat_6e <- sum(dat_6e$ncontrols)/all_controls

# How many times more likely are cases than controls to be
# in the highest alcohol group or the highest tobacco group?
cases6f <- sum(dat_6e$ncases)
cont6f <- sum(dat_6e$ncontrols)
66/70

# Continuous Probability

# Code: Cumulative distribution function
# Define x as male heights from the dslabs heights dataset:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# 
# Given a vector x, we can define a function for 
# computing the CDF of x using:
  
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

# Code: Using pnorm() to calculate probabilities
# Given male heights x:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# We can estimate the probability that a male is taller than 70.5 inches using:
  
  1 - pnorm(70.5, mean(x), sd(x))
# Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges 
# containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't
# match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
# dnorm()
# In R, the probability density function for the normal 
# distribution is given by dnorm(). We will see uses of 
# dnorm() in the future.
# Note that dnorm() gives the density function and pnorm() 
# gives the distribution function, which is the integral 
# of the density function.
# Plotting the probability density for the normal 
# distribution
# We can use dnorm() to plot the density curve for the normal
# distribution. dnorm(z) gives the probability density f(z) 
# of a certain z-score, so we can draw a curve by calculating 
# the density over a range of possible values of z.
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

dnorm(x)
dnorm(x, mu, sigma)

# Monte Carlo Simulations
# rnorm(n, avg, s) generates n random numbers from the normal
# distribution with average avg and standard deviation s.
# By generating random numbers from the normal distribution, 
# we can simulate height data with similar properties to 
# our dataset. Here we generate simulated height data using
# the normal distribution.

# Code: Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution
# - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)
# Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 fe

# Exercise 1. Distribution of female heights - 1
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation
# for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate
# the probability that a randomly selected female is shorter
# than 5 feet. Print this value to the console.
print(pnorm (60, female_avg, female_sd))

# Exercise 2. Distribution of female heights - 2
# Assign a variable 'female_avg' as the average female 
# height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation
# for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate 
# the probability that a randomly selected female is 6 feet
# or taller. Print this value to the console.the probability 
# that a randomly selected female is 6 feet or taller. 
# Print this value to the console.
print (1 - (pnorm (6*12, female_avg, female_sd)))

# If we pick a female at random, what is the probability
# that she is between 61 and 67 inches?
print (pnorm(67, female_avg, female_sd) - 
  pnorm(61, female_avg, female_sd))

# Convert to cm
female_avg <- 64*2.54
female_sd <- 3*2.54
print (pnorm(67*2.54, female_avg, female_sd) - 
         pnorm(61*2.54, female_avg, female_sd))

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# To a variable named 'taller', assign the value of a height that is one SD taller than average.
taller <- 67

# To a variable named 'shorter', assign the value of a height that is one SD shorter than average.
shorter <- 61

# Calculate the probability that a randomly selected 
# female is between the desired height range.
# Print this value to the console.
print (pnorm(taller, female_avg, female_sd) - 
         pnorm(shorter, female_avg, female_sd))

# Assign a variable 'male_avg' as the average male height.
male_avg <- 69

# Assign a variable 'male_sd' as the standard deviation
# for male heights.
male_sd <- 3

# Determine the height of a man in the 99th percentile
# of the distribution.
qnorm(0.99, male_avg, male_sd)

# Exercise 7. Distribution of IQ scores
B <- 10000
highestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the highest IQ
})
mean(highestIQ)
hist(highestIQ)

# Questions 1 and 2: ACT scores, part 1
B <- 10000
set.seed(16, sample.kind = "Rounding")
act_scores <- replicate(B, {
  simulated_data <- rnorm(1, 20.9, 5.7)   
  mean(simulated_data)
})
m <- mean(act_scores)
sd <- sd(act_scores)
df <- as.data.frame(act_scores)
df <- filter(df, act_scores <= 10)
282/10000

x <- seq(1, 36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)



convert_scores <- df %>%
  mutate(act_scores = (act_scores - m)/sd)
z_score <- filter(convert_scores, act_scores > 2)
act_2 <- filter(convert_scores, act_scores == 2)
df[6542,]
233/10000

qnorm(0.975, m, sd)

x <- seq(1, 36)
F <- function(a) pnorm(x, 0, 1)
1 - F(0.95)   
df[5838,]

qnorm(0.95, 20.9, 5.7)
p <- seq(0.01, 0.99, 0.01)
sample_q <- quantile(act_scores, p)
print(sample_q)

theory_q <- qnorm(p, 20.9, 5.7)
qqplot(theory_q, sample_q)

# Random Variables
# Random variables are numeric outcomes resulting from
# random processes.
# Statistical inference offers a framework for quantifying
# uncertainty due to randomness.
# Code: Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

# Sampling Models
# A sampling model models the random behavior of a process
# as the sampling of draws from an urn.
# The probability distribution of a random variable is the 
# probability of the observed value falling in any given 
# interval.
# We can define a CDF F(a) = PR(S <= a)  to answer questions
# related to the probability of S being in any interval.
# The average of many draws of a random variable is called 
# its expected value.
# The standard deviation of many draws of a random variable
# is called its standard error.
# Monte Carlo simulation: Chance of casino losing money on 
# roulette
# We build a sampling model for the random variable S that
# represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S
# We use the sampling model to run a Monte Carlo simulation
# and use the results to estimate the probability of the 
# casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money
# We can plot a histogram of the observed values of S as well
# as the normal density curve based on the mean and standard
# deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

# Exercise 1. American Roulette probabilities
# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket

p_green <- green/(green + black + red)
# Print the variable `p_green` to the console
print(p_green)

# Exercise 2. American Roulette payout
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of 
# the ball not landing in a green pocket

p_not_green <- (black + red)/(green+black+red)
# Create a model to predict the random variable `X`, 
# your winnings from betting on green. Sample one time.
x <- sample(c(-1, 17), 1, replace = TRUE, prob = 
              c(p_not_green, p_green)) 

# Print the value of `X` to the console

print (sum(x))    #

# Exercise 3. American Roulette expected value
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# # Calculate the expected outcome if you win $17 if the 
# ball lands on green and you lose $1 if the ball doesn't 
# land on green
# Expected value of a random variable
# ap + b(1-p)
sum(-1 * p_not_green) + (17 * p_green)

# Exercise 4. American Roulette standard error
# SE = n sqrt * [b-a] * p(1-p)sqrt
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Compute the standard error of the random variable
SE <- abs(sum (17 - -1)) * (sqrt (p_green * p_not_green))

# Exercise 5. American Roulette sum of winnings
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(-1,17), n, replace = TRUE, prob = c(p_not_green, p_green)) 

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)  

# Print the value of 'S' to the console
print(S)

# Exercise 6. American Roulette winnings expected value
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Calculate the expected outcome of 1,000 spins if you win
# $17 when the ball lands on green and you lose $1 when 
# the ball doesn't land on green
# Expected value of n draws =
# n * (ap + b(1-p))

EX <- sum(n * ((-1 * p_not_green)+ (17 * p_green)))
    
# Exercise 7. American Roulette winnings expected value
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Compute the standard error of the sum of 1,000 outcomes
abs(sum(17 - -1)) * sqrt(p_green * p_not_green) / sqrt(1000)

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1 - pnorm(0, avg, se)

# Exercise 2. American Roulette Monte Carlo simulation
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the
# sample code for `B` iterations and sums the outcomes.
S <- replicate(B, {
  X <- sample(c(17,-1), n, replace = TRUE, 
              prob = c(p_green, p_not_green))
  sum(X)    # determine total profit
})



# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

# Exercise 3. American Roulette Monte Carlo vs CLT
# Calculate the proportion of outcomes in the vector
# `S` that exceed $0
mean(S > 0)

# Exercise 5. American Roulette average winnings per bet
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(17,-1), n, replace = TRUE, 
            prob = c(p_green, p_not_green))

# Define a variable `Y` that contains the mean outcome
# per bet. Print this mean to the console.
print(Y <- mean(X))

# Exercise 6. American Roulette per bet expected value
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
print(sum(-1*p_not_green) + (17*p_green))

# Exercise 7. American Roulette per bet standard error
# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs(sum(17 - -1)) * sqrt(p_not_green*p_green)/sqrt(10000)

# Exercise 8. American Roulette winnings per game
# are positive
# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green

# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Given this average and standard error, determine
# the probability of winning more than $0. 
# Print the result to the console.
print(1-pnorm(0, -0.0526, 0.0419))

# Exercise 9. American Roulette Monte Carlo again
## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.

# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S <- replicate(B, {
  X <- sample(c(17,-1), n, replace = TRUE, 
              prob = c(p_green, p_not_green))
  mean(X)    # determine total profit
})




# Compute the average of `S`
print(mean(S))

# Compute the standard deviation of `S`
print(sd(S))

mean(S > 0)

# probability of quessing right on one question
1/5

# What is the expected value of points for 
# guessing on one question?
correct <- 1/5
not_correct <- 4/5
E_X <- sum(-0.25 * not_correct) + (1 * correct)
(-0.25 * 0.8) + (1 * 0.2)

# What is the expected score of guessing on all
# 44 questions?
Expect <- 44*((-0.25 * not_correct) + (1 * correct))
Expect

# What is the standard error of guessing on all
# 44 questions?
sqrt(44) * abs(sum(-0.25 - 1)) * sqrt(correct*not_correct)

# Use the Central Limit Theorem to determine the
# probability that a guessing student scores 8 points
# or higher on the test.
1 - pnorm(8, 0, 3.317)

# Set the seed to 21, then run a Monte Carlo simulation
# of 10,000 students guessing on the test.
set.seed(21, sample.kind = "Rounding")
n <- 44
B <- 10000
S <- replicate(B, {
  X <- sample(c(-0.25,1), n, replace = TRUE, 
              prob = c(not_correct, correct))
  sum(X)    # determine total profit
})
mean(S >= 8)

# multiple choice options is 4 and that there is
# no penalty for guessing - that is, an incorrect
# question gives a score of 0.
correct <- 1/4
not_correct <- 3/4
new_Expect <- 44*((1 * correct))

# What is the lowest p such that the probability of 
# scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
prob_35 <- function(p){
B <- 10000
  result <- replicate(B, {
 score_35 <- sample(c(0,1), 44, replace = TRUE, prob = c(correct, not_correct))
  sum(score_35)>= 35
}
)}
 
mean(prob_35)

Pr <- sapply(p, score_35)
plot(p, Pr)

# What is the expected value of the payout 
# for one bet?
r_win <- 5/38
r_lose <- 1 - r_win
(6*r_win) + (-1*r_lose) 

# What is the standard error of the payout for one bet?
abs(6- -1)  * (sqrt(r_win*r_lose))

# What is the expected value of the average payout
# over 500 bets?
# The expected value of the average of multiple draws
# from an urn is the expected value of the urn (mu)

# The standard deviation of the average of multiple
# draws from an urn is the standard deviation of 
# the urn divided by the square root of the number of
# draws (sigma/sqrt(n)).
2.37/sqrt(500)

# What is the expected value of the sum of 500 bets?
500*-0.079

# What is the standard error of the sum of 500 bets?
sqrt(500) * abs(6 - -1) * (sqrt(r_win*r_lose))
pnorm(-1, -39.5, 52.91)

# Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

# Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

# Code: Expected value and standard error of the sum
# of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# Code: Calculating interest rates for expected value
# of 0
x = - loss_per_foreclosure*p/(1-p)
x

# On a $180,000 loan, this equals an interest rate of:
  
x/180000

# Code: Calculating interest rate for 1% probability 
# of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))/x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Code: Monte Carlo simulation for 1% probability
# of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

# Code: Expected value with higher default rate and
# interest rate
p <- .04 #default  rate
loss_per_foreclosure <- -200000
r <- 0.05 # higher interest rate
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# Code: Calculating number of loans for desired 
# probability of losing money
# The number of loans required is:
  
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# Code: Monte Carlo simulation with known default
# probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

# Code: Monte Carlo simulation with unknown default
# probability
# This Monte Carlo simulation estimates the expected
# profit given an unknown probability of default
# 0.03 < p > 5, modeling the situation where an event
# changes the probability of default for all 
# borrowers simultaneously. 
# Note that your results will differ from the video 
# because the seed is not set.
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

# Exercise 1. Bank earnings
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample( c(0,1), n, 
                 prob=c(1-p_default, p_default), replace = TRUE) 

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
print(S <- sum(defaults) * loss_per_foreclosure)

# Exercise 2. Bank earnings Monte Carlo
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate
# the code from the previous exercise over 'B' 
# iterations to generate a list of summed losses 
# for 'n' loans.  Ignore any warnings for now.
S <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE)
  sum(defaults)*loss_per_foreclosure
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

# Exercise 3. Bank earnings expected value
# What is the expected value of S, the sum of losses 
# over 10,000 loans? 
# For now, assume a bank makes no money if the loan 
# is paid.
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Calculate the expected loss due to default out of
# 10,000 loans
n*(p_default*loss_per_foreclosure)

# Exercise 4. Bank earnings standard error
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Compute the standard error of the sum of 10,000 
# loans
sqrt(n)* abs(sum(0- -1))* (sqrt(p_default * 1-p_default))
sqrt(n)*abs(loss_per_foreclosure)*
  sqrt(p_default*(1-p_default)) 

# Exercise 5. Bank earnings interest rate - 1
# How much money do we need to make when people 
# pay their loans so that our net loss is $0?
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x = - loss_per_foreclosure*p_default/(1-p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
print(x/180000)

# Exercise 6. Bank earnings interest rate - 2
# What should the interest rate be so that the 
# chance of losing money is 1 in 20?
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1-p_default)))/ 
  ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000.
# Print this value to the console.
print(x/180000)
1/20
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))/x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p))

#The Big Short
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

# What is the expected value of the company's 
# net profit on one policy for a 50 year old female?
a <- -150000
b <- 1150
p_a <- 0.003193
p_b <- 1-p_a

X <- (a*p_a) + (b*p_b)

# Calculate the standard error of the profit on 
# one policy for a 50 year old female.

SE <- abs(b-a)*sqrt(p_a*p_b)
SE_1000 <- sqrt(1000)*abs(b-a)*sqrt(p_a*p_b)
1000*X

pnorm(-1, 667378.1, 269657.9)

# Male age 50 premium to charge to make 700,000
p_a <- 0.005013
a <- -150000
p_b <- 1 - p_a
mu <- 700000
n <- 1000

b <- (mu - (n*((a*p_a)/p_b)))/n

# Using the new 50 year old male premium rate, 
# calculate the standard error of the sum of 1,000 
# premiums.
sqrt(n)*abs(b-a)*sqrt(p_a*p_b)

# What is the probability of losing money on a series
# of 1,000 policies to 50 year old males?
# Use the Central Limit Theorem.
pnorm(-1, 700000, 338254.3)

# In this 6-part question, we'll look at a scenario in
# which a lethal pandemic disease increases the 
# probability of death within 1 year for a 50 year old
# to .015. Unable to predict the outbreak, the 
# company has sold 1,000 $150,000 life insurance 
# policies for $1,150.
a <- -150000
b <- 1150
p_a <- 0.015
p_b <- 1-p_a
n <- 1000

EX <- n*((a*p_a)+(b*p_b))
SE <- sqrt(n)*abs(b-a)*sqrt(p_a*p_b)

# What is the probability of the company losing money?
pnorm(-1, EX, SE)

# What is the probability of losing more than $1 
# million?
pnorm(-10^6, EX, SE)

# What is the lowest death probability for which the
# chance of losing money exceeds 90%?

p <- seq(.01, .03, .001)
B <- 1
a <- -150000
b <- 3268.06
n <- 1000
set.seed(30, sample.kind = "Rounding")
profit <- replicate(B, {
  new_p <- 0.015 + sample(seq(.01, .03, .001), 1)
  draws <- sample( c(a, b), n, prob=c(new_p, 1-new_p), replace = TRUE)
  mean(draws)
})

Pr <- sapply(new_p, draws)
mean(draws)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10^6)    # pro
min(profit < -10^6)
  
min(p[which(d_prob > 0.9)])

mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10^6) 

# Define a sampling model for simulating the total
# profit over 1,000 loans
a <- -150000
b <- 1150
p_a <- 0.015
p_b <- 1-p_a
n <- 1000
set.seed(25, sample.kind = "Rounding")
loans <- sample( c(a, b), n, prob=c(p_a, p_b), replace = TRUE) 

sum(loans)/10^6

# Set the seed to 27, then run a Monte Carlo simulation
# of your sampling model with 10,000 replicates to 
# simulate the range of profits/losses
# over 1,000 loans.
B <- 10000
a <- -150000
b <- 1150
p_a <- 0.015
p_b <- 1-p_a
n <- 1000
set.seed(27, sample.kind = "Rounding")
sim_loans <- replicate(B, {
  loans <- sample( c(a, b), n, prob=c(p_a, p_b), replace = TRUE)
  
  sum(loans) <= -10^6
  
})

mean(sim_loans)

# Calculate the premium required for a 5% chance of 
# losing money given  loans, probability of death , 
# and loss per claim . Save this premium as x for 
# use in further questions.  
l <- -150000
z <- qnorm(0.05)
n <- 1000
p <- 0.015
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

# What is the expected profit per policy at this rate?
b <- 3268.06
EX <- (l*p) + b*(1-p)

# What is the expected profit over 1,000 policies?
1000*EX

# Use the new rate
# What is the probability of losing money here?
#   
B <- 10000
a <- -150000
b <- 3268.06
p_a <- 0.015
p_b <- 1-p_a
n <- 1000
set.seed(28, sample.kind = "Rounding")
sim_loans <- replicate(B, {
  loans <- sample( c(a, b), n, prob=c(p_a, p_b), replace = TRUE)
  
  sum(loans) < 0
  
})

mean(sim_loans)

# Monte Carlo on range of p
B <- 10000
a <- -150000
b <- 3268.06
p_a <- 0.015
p_b <- 1-p_a
n <- 1000
set.seed(29, sample.kind = "Rounding")
profit <- replicate(B, {
  new_p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(a, b), n, prob=c(new_p, 1-new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10^6)    # probability of losing over $10 million


