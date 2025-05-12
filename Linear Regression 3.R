# Spurious Correlation
#Required libraries
library(tidyverse)
library(dplyr)
library(Lahman)
library(ggplot2)
library(dslabs)
library(HistData)
library(MASS)
ds_theme_set()


# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), 
                   x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  reframe(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, 
                                          color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  reframe(tidy(lm(y ~ x)))

# simulate independent X, Y and standardize all except entry 23
# note that you may get different values than those shown in the video depending on R version
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")

# Another way association can be confused with causation is 
# when the cause and effect are reversed.
# cause and effect reversal using son heights to predict
# father heights
# in the Galton data, when father and son were reversed in 
# the regression, the model was technically correct. The 
# estimates and p-values were obtained correctly as well. 
# What was incorrect was the interpretation of the model.
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% reframe(tidy(lm(father ~ son)))

# If X and Y are correlated, we call Z a confounder if 
# changes in Z causes changes in both X and Y.
# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  reframe(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  reframe(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  reframe(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  pivot_wider(names_from = gender, values_from = admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  reframe(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot percent of applicants accepted by gender
admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# plot admissions stratified by major
admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% 
  reframe(average = mean(admitted))

?admissions
# Remember that the p-value is defined as the probability 
# of finding the observed result when the null hypothesis 
# (no correlation) is true. When we have a p-value of 0.05, 
# this means the chance of finding a correlation when none 
# exists is 5% - e.g., 0.05*1,000,000 chances.

# Assessment: Confounding 
library(dslabs)
data("research_funding_rates")
research_funding_rates

m_award <- sum(research_funding_rates$awards_men)
m_noaward <- sum(research_funding_rates$applications_men) -
  m_award
w_award <- sum(research_funding_rates$awards_women)
w_noaward <- sum(research_funding_rates$applications_women) -
  w_award
Funding <- research_funding_rates %>%
  reframe(men = m_award,women = w_award)
Funding <- add_row(Funding,men = m_noaward, women = w_noaward)
m_percent <- 290/sum(Funding$men)
w_percent <- 177/sum(Funding$women)
xsg <- tidy(chisq.test(Funding))

# Test data for Simpson's Paradox
Fund_plot <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")
Fund_plot

Fund_plot %>%
  ggplot(aes(gender, y = success, fill = discipline)) +
  geom_bar(stat = "identity", position = "stack")
