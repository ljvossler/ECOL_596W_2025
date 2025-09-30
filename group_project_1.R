library(tidyverse)

# Tidying for Site-Data Plotting
#========================================================
finch <- read.csv('datasets/finch.csv')

# Making separate site-grouped dataframe to not mess with our original
site_grouped_finch_df <- finch %>%
  group_by(type_of_site)

per_site_pox_info <- site_grouped_finch_df %>%
  summarise(freq=sum(pox_IUR=='I'),
            prop=sum(pox_IUR=='I')/n())

per_site_spp_info <- site_grouped_finch_df %>%
  count(species) %>%
  pivot_wider(names_from = species, values_from = n)
per_site_spp_info <- replace(per_site_spp_info, is.na(per_site_spp_info), 0) # NAs to Zeros

# For plotting
per_site_spp_info_long <- per_site_spp_info %>%
  pivot_longer(cols = c('cactus', 'large','medium', 'small'), names_to = 'species')

plumage_grouped_finch_df <- finch %>%
  group_by(plumage) %>%
  filter(!is.na(plumage))

plumage_summary$plumage <- as.factor(plumage_summary$plumage)

plumage_summary <- plumage_grouped_finch_df %>%
  summarise(freq_i=sum(pox_IUR=='I'),
            freq_u=sum(pox_IUR=='U'),
            freq_r=sum(pox_IUR=='R'))

plumage_sum_long<- plumage_summary %>%
  pivot_longer(cols = c("freq_i",  "freq_u",  "freq_r"), names_to = 'status')

# Pox Prevalence Across Site Types
#========================================================
# Frequency
per_site_pox_info %>%
  ggplot(aes(x=type_of_site, y=freq)) +
  geom_col()

# Proportion
per_site_pox_info %>%
  ggplot(aes(x=type_of_site, y=prop)) +
  geom_col(aes(fill = 'red')) +
  labs(title = 'Proportion of Pox-Infected Birds Per Site Type', y='Proportion of Birds Caught', x='Site Type') +
  theme_bw() +
  theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 12),
        axis.title = element_text(size=18), plot.title = element_text(size = 20), legend.position='none') +
  scale_x_discrete(labels=c('Agricultural', 'Arid', 'Forest', 'Pasture'))

# Species Prevalence Across Site Types
#========================================================
per_site_spp_info_long %>%
  ggplot(aes(x=type_of_site, y=value, fill = species)) +
  geom_bar(stat = 'identity', width=0.8, position = position_dodge(preserve = 'single')) +
  labs(title = 'Species Catch Frequency Across Site Types', y='Number of Birds Caught', x='Site Type') +
  theme_bw() +
  theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 12),
        axis.title = element_text(size=18), plot.title = element_text(size = 20)) +
  scale_x_discrete(labels=c('Agricultural', 'Arid', 'Forest', 'Pasture')) +
  guides(fill=guide_legend(title = 'Species')) +
  scale_fill_manual(labels=c('Cactus', 'Large', 'Medium', 'Small'),
                    values = c(cactus="#4EAFAF", large='orange', medium="#FF817E", 'small'='#A6C965'))
#========================================================




plumage_sum_long %>%
  ggplot(aes(x=plumage, y=value, fill = status)) +
  geom_bar(stat = 'identity', width=0.8, position = position_dodge(preserve = 'single')) +
  labs(title = 'Plumage Variation and Infection Status', y='Number of Birds', x='Plumage Variation') +
  theme_bw() +
  theme(axis.text.x=element_text(size = 15), axis.text.y=element_text(size = 12),
        axis.title = element_text(size=18), plot.title = element_text(size = 20)) +
  #scale_x_discrete(labels=c('Agricultural', 'Arid', 'Forest', 'Pasture')) +
  guides(fill=guide_legend(title = 'Status')) +
  scale_fill_manual(labels=c('Infected', 'Unknown', 'Recovered'),
                    values = c(freq_i="#4EAFAF", freq_r='orange', freq_u="#FF817E"))
