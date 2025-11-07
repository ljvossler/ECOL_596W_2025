library(dplyr)
library(ggplot2)
library(lme4)
library(lmer)
library(lmerTest)

# Format Data
euc_data <- read.csv('datasets/Euc_data.csv')

euc_data <- euc_data %>%
  mutate(total_seedlings = euc_sdlgs0_50cm + euc_sdlgs50cm.2m + euc_sdlgs.2m) %>%
  mutate(percent_total_grass = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover)

# Remove weird-looking outliers
euc_data <- euc_data %>%
  filter(total_seedlings < 75)

# Plot some data together
euc_data %>%
  ggplot(aes(x=percent_total_grass, y = total_seedlings)) + geom_point()

# Explore Data
var(euc_data$total_seedlings) # 17.1908 Non-poisson data
mean(euc_data$total_seedlings) # 1.606


# Modeling
lm(total_seedlings~percent_total_grass, data = euc_data) %>% summary()
glm(total_seedlings~percent_total_grass, family="poisson", data=euc_data) %>% summary()

glmer(total_seedlings~percent_total_grass + (1|Season) + (1|Landscape.position) + (1|Property/Quadrat.no), family = "poisson", data = euc_data) %>% summary()

glmer(formula=total_seedlings~scale(percent_total_grass) + scale(annual_precipitation) + (1|Landscape.position), family = "poisson", data = euc_data) %>% summary()


#Logan - testing multiple fixed effects
#random effect may be too big, pull them in and out, try one at a time
#scale variables
