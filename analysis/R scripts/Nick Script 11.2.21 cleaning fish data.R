# working to clean up and provide summaries for Deadwater Data
# Nick Porter
# 11/2/20201
rm(list = ls()) # clear the working environment

require(tidyverse)
require(lubridate)
require(boot)
require(bootstrap)
require(lme4)
require(Matrix)
require(numDeriv)
require(FSA)
require(FSAdata)
require(magrittr)
require(car)
require(dplyr)
require(plotrix)
require(nnet)
#require(Hmisc) # does not work
#require(Matching)
require(Rcapture)
require(nlstools)
require(stringi)
require(MASS)
require(readr)

#Read in the Deadwater dataset
deadwater <- read_csv("analysis/data/raw data/deadwater_cmr_effort_20210614.csv") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%y"))

NPM = deadwater %>%
  filter(Species == "Northern Pikeminnow")

Clean_NPM = NPM %>%
  mutate(Fish_or_Parts = case_when(stri_detect_fixed(StomachContents, "Fish") ~ "Fish or Fish Parts",
                                   stri_detect_fixed(StomachContents, "Empty") ~ "Empty", TRUE ~ "Other")) %>%
  drop_na(Length) %>%
  mutate(Non_fish_wt = StomachContentsWeight - FishContentWeight)

Stomach_content_DF = Clean_NPM %>%
  dplyr::select(Date, Length, LavageID, StomachContents, Comments, Fish_or_Parts, StomachContentsWeight, Non_fish_wt, FishContentWeight)

unique(Clean_NPM)

#Read in the Bioenergetics

bioenergetics<- read_csv("analysis/data/raw data/2020 Bioenergetics 1 year.csv") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%y")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons_fish_g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

bioenergetics_10percet <- read_csv("analysis/data/raw data/2020 Bioenergetics 1 year 10% growth.csv") %>%
mutate(Date = as.POSIXct(Date, format = "%m/%d/%y")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons_fish_g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))


#Fall bioenergetics looking at 78 days
Fall_bioenergetics = bioenergetics %>%
  filter(Date > "2020-08-31") %>%
  filter(Date < "2020-11-18") %>%
  mutate(Cumu_fish_eatten = cumsum(Cons_fish_g)) %>%
  mutate(Date = as.Date(Date))

Alternative_bioenergetics <- read_csv("analysis/data/raw data/Bioenergetics 32 fish 68 inverts.csv") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%y")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons_fish_g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))




#Plot of fish eatten
fall_bioenergetics = Fall_bioenergetics %>%
  filter(Date >= "2020-09-01" & Date <= "2020-11-17") %>%
  ggplot() +
  aes(x = Date, y = Cumu_fish_eatten, colour = Temperature_C) +
  geom_line(size = 2.5) +
  scale_color_distiller(palette = "Spectral", direction = -1) +
  #labs(y = "Cumulative fish eatten (g)") +
  scale_x_date(breaks = "2 week") +
  scale_y_continuous(name = "Cumulative fish eatten (g)", breaks = seq(0,200, by = 10),
                     sec.axis = sec_axis(~. /12, name = "Potential number of Chinook smolts", breaks = seq(0,30, by = 1))) +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(color = "Temperature (C)")

fall_bioenergetics
#Plot of fish eatten for 1 year and no growth for NPM
Bioenergetics_plot <- bioenergetics %>%
  ggplot() +
  aes(x = Date, y = Cumu_fish_eatten, colour = Temperature_C) +
  geom_line(size = 2.5) +
  scale_color_distiller(palette = "Spectral", direction = -1) +
  #labs(y = "Cumulative fish eatten (g)") +
  #scale_x_date(breaks = "1 month") +
  scale_y_continuous(name = "Cumulative fish eatten (g)", breaks = seq(0,600, by = 50),
                     sec.axis = sec_axis(~. /12, name = "Potential number of Chinook smolts", breaks = seq(0,30, by = 2))) +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(color = "Temperature (C)", x = "Month")

Bioenergetics_plot

#bioenergetics plot with 10% growth in NPM
Bioenergetics_plot_10percent <- bioenergetics_10percet %>%
  ggplot() +
  aes(x = Date, y = Cumu_fish_eatten, colour = Temperature_C) +
  geom_line(size = 2.5) +
  scale_color_distiller(palette = "Spectral", direction = -1) +
  #labs(y = "Cumulative fish eatten (g)") +
  #scale_x_date(breaks = "1 month") +
  scale_y_continuous(name = "Cumulative fish eatten (g)", breaks = seq(0,600, by = 50),
                     sec.axis = sec_axis(~. /12, name = "Potential number of Chinook smolts", breaks = seq(0,40, by = 2))) +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(color = "Temperature (C)", x = "Month")

Bioenergetics_plot_10percent

Alt_Bioenergetics_plot <- Alternative_bioenergetics %>%
  ggplot() +
  aes(x = Date, y = Cumu_fish_eatten, colour = Temperature_C) +
  geom_line(size = 2.5) +
  scale_color_distiller(palette = "Spectral", direction = -1) +
  #labs(y = "Cumulative fish eatten (g)") +
  #scale_x_date(breaks = "1 month") +
  scale_y_continuous(name = "Cumulative fish eatten (g)", breaks = seq(0,600, by = 50),
                     sec.axis = sec_axis(~. /12, name = "Potential number of Chinook smolts", breaks = seq(0,30, by = 2))) +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(color = "Temperature (C)", x = "Month")

Alt_Bioenergetics_plot

#esquisse::esquisser(Fall_bioenergetics)

# Stomach contents and length frequency ############################################################

Clean_NPM$NPM_50=lencat(Clean_NPM$Length, w = 50)

Stomach_catagories_sum = Stomach_content_DF %>%
  drop_na(StomachContentsWeight) %>%
  group_by(Fish_or_Parts) %>%
  count()

Stomach_wt_sum = Stomach_content_DF %>%
  drop_na(StomachContentsWeight) %>%
  group_by(Fish_or_Parts) %>%
  summarise(sum(StomachContentsWeight))

Non_fishwt_sum = Stomach_content_DF %>%
  drop_na(StomachContentsWeight) %>%
  group_by(Fish_or_Parts) %>%
  summarise(sum(Non_fish_wt))

Fish_weights_sum = Stomach_content_DF %>%
  drop_na(StomachContentsWeight) %>%
  group_by(Fish_or_Parts) %>%
  summarise(sum(FishContentWeight))


#esquisse::esquisser(Clean_NPM)

Stomach_contents_per_sizeclass = Clean_NPM %>%
  filter(Date >= "2020-05-17 23:00:00" & Date <= "2020-11-21 00:00:00") %>%
  filter(Method %in%
           "Angling") %>%
  ggplot() +
  aes(x = Fish_or_Parts) +
  geom_bar(fill = "#DB740E") +
  labs(x = "Stomach Contents", y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(vars(NPM_50), nrow = 2L)

Stomach_contents_per_sizeclass

length_freq=ggplot(data=Clean_NPM, aes(Length)) +
  geom_histogram(breaks=seq(150, 650, by = 50),
                 col="#DB740E",
                 fill="#DB740E",
                 alpha = .2) +
  labs(x="TL (mm)", y="Frequency") +
  xlim(c(100,700)) +
  ylim(c(0,800)) +
  scale_x_continuous(breaks= seq(0,650, by = 50)) +
  scale_y_continuous(breaks = seq(0,550, by = 50)) +
  theme_classic()

length_freq
Stomach_contents_per_sizeclass
Bioenergetics_plot
Bioenergetics_plot_10percent
fall_bioenergetics

# Save fish passing plot
ggsave('analysis/paper/figures/length_freq.jpg',
       length_freq,
       width = 5,
       height = 4)

ggsave('analysis/paper/figures/stomach content per sizeclass.jpg',
       Stomach_contents_per_sizeclass,
       width = 8,
       height = 4)

ggsave('analysis/paper/figures/bioenergetics.jpg',
       Bioenergetics_plot,
       width = 8,
       height = 4)

ggsave('analysis/paper/figures/bioenergetics 10 percent.jpg',
       Bioenergetics_plot_10percent,
       width = 8,
       height = 4)

ggsave('analysis/paper/figures/fall bioenergetics.jpg',
       fall_bioenergetics,
       width = 8,
       height = 4)


