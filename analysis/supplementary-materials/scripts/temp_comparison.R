library(magrittr)
library(tidyverse)
library(here)
library(lubridate)

# shoup gage data
shoup_df = read_tsv(here("analysis/data/raw_data/USGS_13307000_Salmon_River_Nr_Shoup.txt"),
                         comment = "#") %>%
  rename(discharge_cfs = `47768_00060`,
         discharge_cfs_cd = `47768_00060_cd`,
         gage_ht_ft = `47769_00065`,
         gage_ht_ft_cd = `47769_00065_cd`,
         water_temp_c = `47773_00010`,
         water_temp_c_cd = `47773_00010_cd`)

# deadwater boat ramp tidbit data
tidbit_df = read_csv(here("analysis/data/raw_data/deadwater_tidbit_compiled.csv")) %>%
  mutate(datetime = paste(date, time)) %>%
  mutate(across('datetime', ~ as.POSIXct(.x, format = "%m/%d/%Y %H:%M:%S"))) %>%
  select(datetime, temp_c) %>%
  mutate(source = "Tibdit")

temp_df = shoup_df %>%
  select(datetime, water_temp_c) %>%
  mutate(source = "Shoup") %>%
  rename(temp_c = water_temp_c) %>%
  rbind(tidbit_df) %>%
  mutate(year = year(datetime),
         week = week(datetime)) %>%
  group_by(source, year, week) %>%
  summarize(temp_c = mean(temp_c, na.rm = T)) %>%
  drop_na(week) %>%
  mutate(source_yr = paste(source, year))

temp_p = temp_df %>%
  ggplot() +
  geom_line(aes(x = week,
                 y = temp_c,
                 color = source_yr)) +
  labs(x = "Week",
       y = "Temperature (C)",
       color = "Source and Year") +
  scale_color_brewer(palette = "Set1") +
  theme_bw()
temp_p
