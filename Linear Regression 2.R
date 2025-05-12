#Linear Regression 2
#Required libraries
library(tidyverse)
library(dplyr)
library(Lahman)
library(ggplot2)
library(dslabs)
library(HistData)
library(MASS)
ds_theme_set()

##HR and Runs
HR_run <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G, BB_per_game = BB / G) %>%
  
  lm(R_per_game ~ HR_per_game + BB_per_game, .)
  summary(lm) %>%
  
  
  
  
summarise(lm)
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
  
data("Teams")
rundata <- Teams|>
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, 
         R_per_game = R / G, 
          BB_per_game = BB / G) %>%
  Runmodel <- lm(HR_per_game ~ BB_per_game)
  Runmodel
    
   
  
  # fit regression line to predict son's height from father's height
  fit <- lm(son ~ father, data = galton_heights)
  fit
  
#Define female_heights, a set of mother and daughter heights sampled 
  #from GaltonFamilies, as follows:
  
  set.seed(1989) #if you are using R 3.5 or earlier
  set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
set.seed(1989, sample.kind="Rounding") 
options(digits = 3)    # report 3 significant digits
  
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#Detach MASS to solve unused arguments error
library(HistData)
data("GaltonFamilies")
set.seed(1989, sample.kind="Rounding") 
options(digits = 3)    # report 3 significant digits
galton_heights <- GaltonFamilies |>
  filter(gender == "female") |>
  group_by(family) |>
  sample_n(1) |>
  ungroup() |>
  select(mother, childHeight) |>
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data = galton_heights)
fit
summary(fit)

#Predict mothers' heights using the model from Question 7 and the predict() function.
model <- lm(mother ~ daughter, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
mom <- as_tibble(predictions) %>% bind_cols(daughter = galton_heights$daughter)



#compare the stability of singles and BBs.
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

library(Lahman)
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

sum(bat_99_01$mean_singles > 0.2)
sum(bat_99_01$mean_bb > 0.2)

#combine the bat_02 table with the table of 1999-2001 rate averages you created
#in the previous question.
bat_99_02 <-inner_join(bat_02, bat_99_01)
cor(bat_99_02$singles, bat_99_02$mean_singles)
cor(bat_99_02$bb, bat_99_02$mean_bb)

#Make scatterplots of mean_singles versus singles and mean_bb versus bb.
bat_99_02 %>%
  ggplot(aes(mean_singles, singles)) + 
  geom_point(alpha = 0.5)

bat_99_02 %>%
  ggplot(aes(mean_bb, bb)) + 
  geom_point(alpha = 0.5)

#Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
smodel <- lm(singles ~ mean_singles, data = bat_99_02)
smodel
summary(smodel)
predsingles <- predict(smodel, interval = c("confidence"), level = 0.95)
predsingles

#Fit a linear model to predict 2002 bb given 1999-2001 mean_bb
bb_model <- lm(bb ~ mean_bb, data = bat_99_02)
bb_model
summary(bb_model)
predsingles <- predict(smodel, interval = c("confidence"), level = 0.95)

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# include the lm inside a summarize and it will work
dat %>%  
  group_by(HR) %>%
  summarize(slope = lm(R ~ BB)$coef[2])

# tidy function from broom returns estimates in and information in a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals
tidy(fit, conf.int = TRUE)

# combine with group_by and summarize to get the table we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))

# it's a data frame so we can filter and select the rows and columns we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# visualize the table with ggplot
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#the tibble dat is defined as follows:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>%
  summarise(get_slope())

#correct answer
dat %>%
  group_by(HR) %>%
  summarise(get_slope(across()))

dat %>%
  group_by(HR) %>%
  summarise(slope = get_slope(across()))

#You want to know whether the relationship between home runs and runs per game
#varies by baseball league. You create the following dataset:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>%
  group_by(lgID) %>%
  summarise(tidy(lm(R ~ HR, data = across()), confint = T)) %>%
  filter(term == "HR")

dat %>%
  group_by(lgID) %>%
  summarise(tidy(lm(R ~ HR, data = ,), confint = T)) %>%
  filter(term == "HR")

#Create the galton dataset using the code below:
library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton_pair <- galton %>%
  group_by(pair) %>%
  summarise(n = n ())

#Calculate the correlation coefficients for fathers and daughters, 
#fathers and sons, mothers and daughters and mothers and sons.
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))

#Compute the least squares estimates, standard errors, 
#confidence intervals and p-values for the parentHeight coefficient for each pair.
library(broom)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)
  
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < 0.05) %>%
  PV <- pull(p.value)

# Building a Better Offensive Metric for Baseball
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
# NOTE: In old versions of the Lahman library, the "People" dataset was called "Master"
# The following code may need to be modified if you have not recently updated the Lahman library.
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#A way to actually pick the players for the team can 
#be done using linear programming.
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
#This algorithm chooses these 9 players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#We note that these players all have above average BB and HR rates
#while the same is not true for singles.
my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

# The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(People, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# The code to create a table with only the ROY award winners and add
# their batting statistics
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# The code to keep only the rookie and sophomore seasons 
# and remove players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# The code to use the spread function to have one column for 
# the rookie and sophomore years batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

# The code to calculate the proportion of players who have a lower
# batting average their sophomore year:
mean(ROY$sophomore - ROY$rookie <= 0)

# The code to do the similar analysis on all players that played
# the 2013 and 2014 seasons and batted more than 130 times 
# (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

# The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

#Another use for linear regression is with measurement error models, 
#where it is common to have a non-random covariate (such as time). 
#Randomness is introduced from measurement error rather than sampling 
#or natural variability.

# The code to use dslabs function rfalling_object to generate simulations of 
# dropping balls:
  library(dslabs)
  library(broom)
falling_object <- rfalling_object()

# The code to draw the trajectory of the ball:
  falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
  
# The code to use the lm() function to estimate the coefficients:
  fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

# The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

# The code to see the summary statistic of the regression:
  tidy(fit, conf.int = TRUE)
  
# the equation for the trajectory of a falling object is:
  
# d =h0 + v0t − 0.5 × 9.8t^2
  
# Question 3
TeamA <- (2*0.371) + (4*0.519) + (0.771) + (1.44)
TeamB <- (1*0.371) + (6*0.519) + (2*0.771) + 1.24

# Question 7
# The measurement error is assumed to be random, independent from each other, 
# and having the same distribution for each i. We also assume that there is 
# no bias, which means the expected value  E[ε] = 0.

#  Regression and baseball, part 2
# Use the Teams data frame from the Lahman package. Fit a multivariate linear 
# regression model to obtain the effects of BB and HR on Runs (R) in 1971. 
# Use the tidy() function in the broom package to obtain the results in a data frame.
library(Lahman)
library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)
Teams1971 <- Teams %>% 
  filter(yearID == 1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(Teams1971, conf.int = TRUE)

# find the effects of BB and HR on runs (R) for every year 
# from 1961 to 2018 using reframe() and the broom package.

Teams61_18 <- Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 

Teams61_18 %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

# Fit a linear model on the results from Question 10 to 
# determine the effect of year on the impact of BB. That is, 
# determine how the estimated coefficients of BB from the 
# models in Question 10 can be predicted by the year

Teams61_18 %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID")
  pull(estimate)
  
# Use linear models to answer the following 3-part 
# question about Teams_small.
library(tidyverse)
library(broom)
library(Lahman)

# Affect of R, HR, and W on attendance
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, R = R/G, HR = HR/G)
  tidy(lm(avg_attendance ~ R, data = Teams_small))
  tidy(lm(avg_attendance ~ HR, data = Teams_small))
  tidy(lm(avg_attendance ~ W, data = Teams_small))
  tidy(lm(avg_attendance ~ yearID, data = Teams_small))
  
cor(Teams_small$W, Teams_small$R)
cor(Teams_small$W, Teams_small$HR)

# Stratify Teams_small by wins: divide number of wins by 10
# and then round to the nearest integer. Filter to keep only
# strata 5 through 10.
dataW <- Teams_small %>%
  mutate(W_strata = round(W/10, 0)) %>%
  filter(W_strata >= 5 & W_strata <=10) %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ R, data = .))) %>%
  ungroup() 
sum(dataW$W_strata == 8)
  
# Fit a multivariate regression determining the effects of 
# runs per game, home runs per game, wins, and year on 
# average attendance  
avg_att <- lm(avg_attendance ~ R + HR + W + yearID, data = Teams_small)
model_2002 <- data.frame(R = 5, HR = 1.2, W = 80, yearID = 2002)
model_1960 <- data.frame(R = 5, HR = 1.2, W = 80, yearID = 1960)
predict(avg_att, data.frame(R = 5, HR = 1.2, W = 80, 
                                      yearID = 2002))
predict(avg_att, newdata = model_1960)

# Use your model from Question 4 to predict average attendance for 
# teams in 2002 in the original Teams data frame.

Teams_2002 <- Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(avg_attendance = attendance/G, R = R/G, HR = HR/G)


predict_2002 <- predict(avg_att, newdata = Teams_2002)
att_2002 <- as_tibble(predict_2002) %>% 
  bind_cols(Actual = Teams_2002$attendance)

cor(att_2002$value, att_2002$Actual)
