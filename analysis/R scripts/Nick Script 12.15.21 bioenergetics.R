# working to clean up and provide summaries for Deadwater Data
# Nick Porter
# 12/15/20201
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
deadwater <- read_csv("analysis/data/raw_data/deadwater_cmr_effort_20210614.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))


NPM = deadwater %>%
  filter(Species == "Northern Pikeminnow")

Clean_NPM = NPM %>%
  mutate(Fish_or_Parts = case_when(stri_detect_fixed(StomachContents, "Fish") ~ "Fish or Fish Parts",
                                   stri_detect_fixed(StomachContents, "Empty") ~ "Empty",
                                   stri_detect_fixed(Comments, "Whole scuplin") ~ "Fish or Fish Parts",
                                   stri_detect_fixed(StomachContents, "Whitefish") ~ "Fish or Fish Parts",
                                   TRUE ~ "Other")) %>%
  drop_na(Length) %>%
  mutate(Non_fish_wt = StomachContentsWeight - FishContentWeight)

Stomach_content_DF = Clean_NPM %>%
  dplyr::select(Date, Length, LavageID, StomachContents, Comments,
                Fish_or_Parts, StomachContentsWeight, Non_fish_wt, FishContentWeight) %>%
  filter(!StomachContents %in% c("Lost sample CG27-001","Did Not Lavage")) %>%
  drop_na(StomachContents)


Spring_Avg_length = Clean_NPM %>%
  filter(Date > "2021-05-17") %>%
  summarise(avg_TL = mean(Length))

Fall_Avg_length = Clean_NPM %>%
  filter(Date < "2021-05-17") %>%
  summarise(avg_TL = mean(Length))

#Read in the Bioenergetics

Fall_I10_F90<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I10_F90.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I20_F80<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I20_F80.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I30_F70<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I30_F70.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I40_F60<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I40_F60.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I50_F50<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I50_F50.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I60_F40<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I60_F40.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I70_F30<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I70_F30.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I80_F20<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I80_F20.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Fall_I90_F10<- read_csv("analysis/data/raw_data/Bioenergetics_Fall_I90_F10.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

#########################################################################
##    SPRING BIOENERGETICS   ###############3

Spring_I10_F90<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I10_F90.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I20_F80<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I20_F80.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I30_F70<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I30_F70.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I40_F60<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I40_F60.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I50_F50<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I50_F50.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I60_F40<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I60_F40.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I70_F30<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I70_F30.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I80_F20<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I80_F20.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

Spring_I90_F10<- read_csv("analysis/data/raw_data/Bioenergetics_Spring_I90_F10.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d")) %>%
  mutate(Cumu_fish_eatten = cumsum(Cons.fish.g)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(month = month(Date))

fall_weeks = c("9/13", "9/20", "9/27", "10/4", "10/11", "10/18", "10/25", "11/1", "11/8", "11/15", "11/22", "11/29")


spring_week = c("3/1", "3/8", "3/15", "3/22", "3/29", "4/5", "4/19", "4/26", "5/3", "5/10", "5/17", "5/24", "5/31")

#######################################################################
#Spring_bioenergetics %>%
 Spring_Graph = ggplot() +
    geom_line(aes(y = Spring_I10_F90$Cumu_fish_eatten, x =Spring_I10_F90$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I20_F80$Cumu_fish_eatten, x =Spring_I20_F80$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I30_F70$Cumu_fish_eatten, x =Spring_I30_F70$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I40_F60$Cumu_fish_eatten, x =Spring_I40_F60$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I50_F50$Cumu_fish_eatten, x =Spring_I50_F50$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I60_F40$Cumu_fish_eatten, x =Spring_I60_F40$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I70_F30$Cumu_fish_eatten, x =Spring_I70_F30$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I80_F20$Cumu_fish_eatten, x =Spring_I80_F20$Date, colour = "blue")) +
    geom_line(aes(y = Spring_I90_F10$Cumu_fish_eatten, x =Spring_I90_F10$Date, colour = "blue")) +
              geom_line(size = 2.5) +
  #scale_color_distiller(palette = "Spectral", direction = -1) +
  #labs(y = "Cumulative fish eatten (g)") +
  scale_x_date(breaks = "1 weeks") +
  scale_y_continuous(name = "Cumulative fish eatten (g)", breaks = seq(0,600, by = 5),
                     sec.axis = sec_axis(~. /10.9, name = "Potential number of Chinook smolts", breaks = seq(0,30, by = 1))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Week")
Spring_Graph

 #Fall ###################################################################
 Fall_Graph= ggplot() +
    geom_line(aes(y = Fall_I10_F90$Cumu_fish_eatten, x =Fall_I10_F90$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I20_F80$Cumu_fish_eatten, x =Fall_I20_F80$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I30_F70$Cumu_fish_eatten, x =Fall_I30_F70$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I40_F60$Cumu_fish_eatten, x =Fall_I40_F60$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I50_F50$Cumu_fish_eatten, x =Fall_I50_F50$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I60_F40$Cumu_fish_eatten, x =Fall_I60_F40$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I70_F30$Cumu_fish_eatten, x =Fall_I70_F30$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I80_F20$Cumu_fish_eatten, x =Fall_I80_F20$Date, colour = "blue")) +
    geom_line(aes(y = Fall_I90_F10$Cumu_fish_eatten, x =Fall_I90_F10$Date, colour = "blue")) +
    geom_line(size = 2.5) +
    #scale_color_distiller(palette = "Spectral", direction = -1) +
    #labs(y = "Cumulative fish eatten (g)") +
    scale_x_date(breaks = "1 weeks") +
    scale_y_continuous(name = "Cumulative fish eatten (g)", breaks = seq(0,600, by = 5),
                       sec.axis = sec_axis(~. /10.3, name = "Potential number of Chinook smolts", breaks = seq(0,30, by = 1))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = "Week")


Fall_Graph

#esquisse::esquisser(Fall_bioenergetics)

# Stomach contents and length frequency ############################################################

Clean_NPM$NPM_10=lencat(Clean_NPM$Length, w = 10)

PSD_DF = Clean_NPM %>%
  group_by(NPM_10) %>%
  summarise(n = n())

#54.2  S = 250mm P= 380mm

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

#determining fish spp vs unknown

Num_Hungry_fish = Stomach_content_DF %>%
  filter(!Fish_or_Parts == "Empty") %>%
  group_by(Fish_or_Parts) %>%
  count()

Num_Hungry_fish_YR = Stomach_content_DF %>%
  filter(!Fish_or_Parts == "Empty") %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Fish_or_Parts) %>%
  count()

Hungry_fish_DF = Stomach_content_DF %>%
  filter(Fish_or_Parts == "Fish or Fish Parts") %>%
  mutate(Fish = case_when(stri_detect_fixed(StomachContents, "Shiner") ~ "Redside Shiner",
                          stri_detect_fixed(StomachContents, "Whitefish") ~ "Mountin Whitefish",
                          stri_detect_fixed(StomachContents, "scuplin") ~ "Sculpin",
                          stri_detect_fixed(StomachContents, "Chinook") ~ "Chinook",
                          stri_detect_fixed(Comments, "whitefish") ~ "Mountin Whitefish",
                          stri_detect_fixed(Comments, "scuplin") ~ "Sculpin",
                          stri_detect_fixed(Comments, "chinook") ~ "Chinook", TRUE ~ "Other"))


#esquisse::esquisser(Clean_NPM)

Stomach_contents_per_sizeclass = Clean_NPM %>%
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

Alt_Bioenergetics_plot

# Save fish passing plot
ggsave('analysis/paper/figures/length_freq.jpg',
       length_freq,
       width = 5,
       height = 4)

ggsave('analysis/paper/figures/stomach content per sizeclass.jpg',
       Stomach_contents_per_sizeclass,
       width = 8,
       height = 4)

ggsave('analysis/paper/figures/Fall_Bioenergetics.jpg',
       Fall_Graph,
       width = 8,
       height = 4)

ggsave('analysis/paper/figures/Spring_Bioenergetics.jpg',
       Spring_Graph,
       width = 8,
       height = 4)




