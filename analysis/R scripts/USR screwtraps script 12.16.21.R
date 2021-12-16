rm(list = ls())

require(tidyverse)
require(lubridate)
require(esquisse)

tags<- Tagging_Detail_Bioenergetics_2020_avg_weight_ <- read_csv("Merck Sharp & Dohme, Corp/Biomark Field Sampling - Documents/General/Deadwater/Tagging Detail (Bioenergetics).csv")
glimpse(tags)

Clean_tags = tags %>%
  rename(Site_Name =`Release Site Name`) %>%
  rename(Mark_Date = `Mark Date MMDDYYYY`) %>%
  rename(Tag_code = `Tag Code`) %>%
  rename(Weight = `Weight g`) %>%
  rename(Length = `Length mm`) %>%
  rename(Species = `Species Name`) %>%
  rename(Count = `Mark Count`) %>%
  mutate(Mark_Date = as.Date(Mark_Date, format = "%m/%d/%y")) 

Clean_tags_Year_df = Clean_tags %>%
  select(Site_Name, Mark_Date, Tag_code, Weight, Length, Species, Count, Mark_Date) %>%
  mutate(Year = year(Mark_Date)) 

unique(Clean_tags_Year_df$Site_Name)

LLR = Clean_tags_Year_df %>%
  filter(Site_Name == "LLRTP - Lower Lemhi River Rotary Screw Trap")

HYDTRP = Clean_tags_Year_df %>%
  filter(Site_Name == "HYDTRP - Hayden Creek Rotary Screw Trap" )

PAHTRP = Clean_tags_Year_df %>%
  filter(Site_Name ==  "PAHTRP - Pahsimeroi River Trap")

SAWTRP = Clean_tags_Year_df %>%
  filter(Site_Name == "SAWTRP - Sawtooth Trap"  )

NFSTRP = Clean_tags_Year_df %>%
  filter(Site_Name == "NFSTRP - North Fork Salmon River Rotary Screw Trap")

LEMTRP = Clean_tags_Year_df %>%
  filter(Site_Name == "LEMTRP - Upper Lemhi River Rotary Screw Trap")

LEMHIW = Clean_tags_Year_df %>%
  filter(Site_Name == "LEMHIW - Lemhi River Weir")

LEMHIR = Clean_tags_Year_df %>%
  filter(Site_Name == "LEMHIR - Lemhi River")

SALRNF = Clean_tags_Year_df %>%
  filter(Site_Name == "SALRNF - North Fork Salmon River")


Avg_wt_length = Clean_tags_Year_df %>%
  filter(!Length >300) %>%
  na.omit(Weight) %>%
  group_by(Site_Name) %>%
  summarise(avg_length = mean(Length), avg_wt = mean(Weight))

Overall_average = Clean_tags_Year_df %>%
  filter(!Length >300) %>%
  na.omit(Weight) %>%
  summarise(avg_length = mean(Length), avg_wt = mean(Weight))

#esquisse::esquisser(LLR)




Clean_tags_Year_df %>%
  filter(Site_Name %in% "LLRTP - Lower Lemhi River Rotary Screw Trap") %>%
  ggplot() +
  aes(x = Mark_Date) +
  geom_bar(fill = "#112446") +
  labs(x = "Date") +
  theme_classic() +
  scale_x_date(date_breaks = "2 weeks") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(PAHTRP) +
  aes(x = Mark_Date) +
  geom_bar(fill = "#112446") +
  labs(x = "Date") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
