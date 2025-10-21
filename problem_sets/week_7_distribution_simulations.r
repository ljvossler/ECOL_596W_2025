library(dplyr)
library(ggplot)
library(ggthemr)
ggthemr(palette = "dust", layout = "clean", text_size = 24)

# Leaf length: probably normal! Continuous, have a "typical" value
# Biological traits: there's variation, some small, some big, constraints on size
# based on biology, trade-offs between water loss and photosynthesis

# mesquite
mesquite_m <- 12
mesquite_sd <- 4

fake_mesquite <- rnorm(100000, mean = mesquite_m, sd = mesquite_sd)
hist(fake_mesquite)


#huachucas: a sample of 20 leaves
mesquite_huachuca <- 15

# how often would we get a sample where mean = 15 from
# taking 20 leaves from the population
head(fake_mesquite)

sampling_results <- NULL
for (i in 1:10000000) {
sampling_results[i] <- sample(fake_mesquite, 20, replace = TRUE) %>% mean
}
hist(sampling_results)
abline(v = 15)
sampling_results[sampling_results >= 15] %>% length /

# this is a very rare result
4097/10000000


# Example: Is Rocio clairvoyant?
# Did she successfully guess the suite or not?
# 30 trials
# Successful = 10
# Wrong = 20

# what is our null expectation of correct guesses?
# p = 0.25

# simulate a binomial distribution where p = 0.25

average_person <- rbinom(1000, 1, p = 0.25)
head(average_person)
average_person %>% sum

data.frame(average_person = average_person) %>%
  ggplot(aes(x = average_person)) + geom_bar()

# Hypothesis: Rocio is clairvoyant (and her distribution is
# not drawn from the average person distribution)

# how likely is it that a sample where p = 0.3 came from
# an underlying distribution where p = 0.25


# let's just pull binomial data a bunch times with the parameters
# that we're assuming from the "average person" situation

clairvoyance_simulations <- NULL # create a place to save the results

# run a loop that pulls samples from the null distribution 10,000 times
for (i in 1:10000) {
 sample <- rbinom(30, 1, p = 0.25)
clairvoyance_simulations[i] <-  sum(sample)/30 # save the proportion of correct things
}

# Look at the results
hist(clairvoyance_simulations)
abline(v=.3) # observed correct values

# Are Rocios results unusual? Not really
clairvoyance_simulations[clairvoyance_simulations >= 0.3] %>% length/10000

