finch = read.csv("datasets/finch.csv")
library(MASS)
library(dplyr)
str(finch)

#problems
table(finch$sex)
#couple extra numbers
#extra u space??
table(finch$site)
# Ma.Elena Guerra vs. Maria Elena Guerra, Barranco vs. El Barranco, ramon accent
#band 22

table(finch$species)

#sex and plumage vs pox presence
#site vs pox presence
#month vs pox presence
# R vs U and impact on mass within species

#PRETTY CODE

finch = read.csv("datasets/finch.csv")

#cleaning up sex
finch_updated <- finch |> mutate(sex=recode(sex, "U " = "U"))
finch_updated <- finch |> filter(!sex %in% c("0","4")) |> mutate(sex=recode(sex, "U " = "U"))

#cleaning up site
finch_updated <- finch |> mutate(site=recode(site, "El Barranco" = "Barranco", "Ma. Elena Guerra" = "Maria Elena Guerra", "Miguel Ram\xf3n" = "Miguel Ramon"))

#cleaning up band
duplicated(finch$band)
finch_updated <- finch[!duplicated(finch$band),]
finch_updated <- finch_updated[band!="CEMPA SEA LISBOA CO 36956",]
finch_updated <- finch_updated[band!="NO ANILLO",]
finch_updated <- finch_updated[band!="T042955-KLIWAT-AUSTRIA",]
finch_updated <- finch_updated[!grepl("^[0-9]+$", finch_updated$band),]

#graphing

#Impact of R vs U on mass across species
finch_mass <- finch_updated |> select(mass,species,pox_IUR) |> filter(!is.na(pox_IUR)) |> mutate(pox_IUR=recode(pox_IUR, "I" = "Infected", "U" = "Uninfected", "R" = "Recovered"))

head(finch_mass)

library(dplyr)
library(ggplot2)
library(ggpubr)

finch_mass |> ggplot(aes(x=pox_IUR, y=mass, fill = pox_IUR)) + geom_boxplot() + facet_wrap(~species, labeller = as_labeller(c("cactus" = "Cactus", "large" = "Large", "medium" = "Medium", "small" = "Small"))) + stat_compare_means(label = "p.format", label.y=42) + labs(x="Pox Presence", y="Mass", title = "Mass vs Pox Presence Across Species", fill="Pox Presence") + theme(text = element_text(size = 18)) + scale_fill_brewer(palette="Paired")

finch_spp %>%
  ggplot(aes(x = pox_IUR, y= plumage)) +
  geom_point(fill="steelblue") +
  labs(title = "Pox Status Plumage", x = "Pox Status", y = "Plumage") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        plot.title = element_text(size=14,face="bold"))
