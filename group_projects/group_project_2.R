library(dplyr)
library(ggplot2)
library(lme4)
#library(lmer)
library(lmerTest)

# Format Data
euc_data <- read.csv('datasets/Euc_data.csv')

euc_data <- euc_data %>%
  mutate(total_seedlings = euc_sdlgs0_50cm + euc_sdlgs50cm.2m + euc_sdlgs.2m) %>%
  mutate(percent_total_grass = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover) %>%
  mutate(percent_plant_cover = ExoticAnnualHerb_cover + ExoticPerennialHerb_cover +ExoticShrub_cover + NativePerennialFern_cover + NativePerennialHerb_cover + NativeShrub_cover + NativePerennialGraminoid_cover) %>%
  mutate(percent_abiotic_cover = BareGround_cover, Rock_cover) %>%
  mutate(germination_binom = ifelse(total_seedlings > 0,1,0)) %>%
  mutate(percent_total_cover = percent_abiotic_cover + percent_plant_cover)


# Remove weird-looking outliers
euc_data <- euc_data %>%
  filter(total_seedlings < 75) %>%
  filter(total_seedlings != 0)

# Plot some data together
euc_data %>%
  ggplot(aes(x=percent_total_grass, y = total_seedlings)) + geom_point()

# Explore Data
var(euc_data$total_seedlings) # 17.1908 Non-poisson data
mean(euc_data$total_seedlings) # 1.606


# Modeling
lm(total_seedlings~percent_total_grass, data = euc_data) %>% summary()
glm(total_seedlings~percent_total_grass, family="poisson", data=euc_data) %>% plot()

glmer(total_seedlings~percent_total_grass + (1|Season) + (1|Landscape.position) + (1|Property/Quadrat.no), family = "poisson", data = euc_data) %>% summary()

glm(formula=total_seedlings~scale(percent_total_grass) + scale(percent_plant_cover), family = "quasipoisson", data = euc_data) %>% summary()

glmer(formula=total_seedlings~scale(percent_total_grass)  +(1|Property/Quadrat.no) , family = "poisson", data = euc_data) %>% plot()


#Logan - testing multiple fixed effects
#random effect may be too big, pull them in and out, try one at a time
#scale variables
