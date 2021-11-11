# Author: Kevin See
# Purpose: import data from Deadwater sampling
# Created: 11/9/21
# Last Modified: 11/9/21
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(FSA)
library(here)

theme_set(theme_bw())

#-----------------------------------------------------------------
raw_df = read_csv(here('analysis/data/raw_data/deadwater_cmr_effort_20210614.csv')) %>%
  clean_names() %>%
  # filter(!is.na(count)) %>%
  filter(count > 0) %>%
  mutate(across(date,
                mdy)) %>%
  # split into sampling periods
  mutate(event_name = if_else(month(date) > 8,
                              paste("Fall", year(date), sep = "_"),
                              paste("Spring", year(date), sep = "_"))) %>%
  # fix some data from last day - all 1st time captures
  group_by(event_name) %>%
  mutate(capture_type = if_else(date == max(date, na.rm = T),
                                "Mark",
                                capture_type),
         # some fish were caught and then killed
         capture_type = if_else(fish_status == "Dead",
                                "Mark",
                                capture_type)) %>%
  ungroup()

tabyl(raw_df, species, event_name)
raw_df %>%
  filter(species == "Northern Pikeminnow") %>%
  # filter(is.na(fish_status)) %>% as.data.frame()
  # tabyl(date, capture_type)
  tabyl(fish_status)


# using Schnabel model
N_mods = raw_df %>%
  filter(species == "Northern Pikeminnow") %>%
  # filter out spring 2021 (no recapture)
  filter(event_name != "Spring_2021") %>%
  group_by(event_name, species, date, fish_status, capture_type) %>%
  summarise(n_fish = n(),
            .groups = "drop") %>%
  group_by(event_name, species, date) %>%
  summarise(n = sum(n_fish),
            m = sum(n_fish[capture_type == "Recapture"]),
            R = sum(n_fish[fish_status == "Live"]),
            dead_fish = sum(n_fish[fish_status == "Dead"]),
            .groups = "drop") %>%
  group_by(event_name, species) %>%
  nest() %>%
  mutate(mod = map(data,
                   .f = function(x) {
                     with(x,
                          mrClosed(n = n,
                                   m = m,
                                   R = R,
                                   method = "Schnabel"))
                   }),
         N = map_dbl(mod,
                     .f = summary),
         CI = map(mod,
                  .f = confint))

N_mods %>%
  select(species, N, CI) %>%
  mutate(N.lci = map_dbl(CI,
                         .f = function(x) x[1]),
         N.uci = map_dbl(CI,
                         .f = function(x) x[2])) %>%
  select(-CI)

#-----------------------------------------------------------------
# using Lincoln-Petersen model, combining first 4 and last four days
raw_df %>%
  filter(species == "Northern Pikeminnow") %>%
  mutate(across(date,
                ~ as.factor(as.character(.)))) %>%
  # tabyl(date, capture_type, clip_type)
  tabyl(capture_type, date, clip_type)

# define mark and recapture periods
cap_periods = tibble(event_name = "Fall_2019",
                     cap_period = "Mark",
                     int = interval(start = ymd('20191112'),
                                    end = ymd('20191114'))) %>%
  bind_rows(tibble(event_name = "Fall_2019",
                   cap_period = "Recapture",
                   int = interval(start = ymd('20191119'),
                                  end = ymd('20191121')))) %>%
  bind_rows(tibble(event_name = "Fall_2020",
                   cap_period = "Mark",
                   int = interval(start = ymd('20201020'),
                                  end = ymd('20201023')))) %>%
  bind_rows(tibble(event_name = "Fall_2020",
                   cap_period = "Recapture",
                   int = interval(start = ymd('20201027'),
                                  end = ymd('20201030'))))

raw_df$cap_period = NA_character_
for(i in 1:nrow(cap_periods)) {
  my_int = cap_periods$int[i]
  index = which(raw_df$date %within% my_int)
  raw_df$cap_period[index] = cap_periods$cap_period[i]
}


LP_mod = raw_df %>%
  filter(species == "Northern Pikeminnow") %>%
  # filter out spring 2021 (no recapture)
  filter(event_name != "Spring_2021") %>%
  # mutate(mr_type = if_else(cap_period == "Mark",
  #                          if_else(capture_type == "Mark" & fish_status == "Live",
  #                                  "M",
  #                                  NA_character_),
  #                          if_else(cap_period == "Recapture",
  #                                  if_else(grepl('Caudal', clip_type),
  #                                          "R",
  #                                          "C"),
  #                                  NA_character_),
  #                          NA_character_)) %>%
  mutate(mr_type = if_else(cap_period == "Mark" & fish_status == "Live",
                           "M",
                           if_else(cap_period == "Recapture",
                                   if_else(capture_type == "Mark",
                                           "C",
                                           if_else(capture_type == "Recapture",
                                                   "R",
                                                   NA_character_)),
                                   NA_character_))) %>%
  filter(!is.na(mr_type)) %>%
  group_by(event_name, species) %>%
  summarise(M = sum(mr_type == "M"),
            n = sum(mr_type %in% c("C", "R")),
            m = sum(mr_type == 'R'),
            .groups = "drop") %>%
  nest(data = -c(event_name, species)) %>%
  crossing(model = c("Petersen",
                     "Chapman")) %>%
  mutate(mr_mod = map2(data,
                       model,
                      .f = function(x, y) {
                        with(x,
                             mrClosed(M = M,
                                      n = n,
                                      m = m,
                                      method = y))
                      }),
         N_summ = map(mr_mod,
                      .f = summary,
                      incl.SE = T),
         CI = map(mr_mod,
                  .f = confint))

LP_mod %>%
  select(event_name, species, model, N_summ, CI) %>%
  mutate(N = map_dbl(N_summ,
                     .f = function(x) x[1]),
         SE = map_dbl(N_summ,
                      .f = function(x) x[2]),
         N.lci = map_dbl(CI,
                         .f = function(x) x[1]),
         N.uci = map_dbl(CI,
                         .f = function(x) x[2])) %>%
  select(-N_summ, -CI)

#-----------------------------------------------------------------
# compare all results
LP_mod %>%
  select(event_name, species, model, N_summ, CI) %>%
  mutate(N = map_dbl(N_summ,
                     .f = function(x) x[1]),
         SE = map_dbl(N_summ,
                      .f = function(x) x[2]),
         Lci = map_dbl(CI,
                         .f = function(x) x[1]),
         Uci = map_dbl(CI,
                         .f = function(x) x[2])) %>%
  select(-N_summ, -CI) %>%
  bind_rows(N_mods %>%
              mutate(model = "Schnabel") %>%
              select(event_name, species, model, N, CI) %>%
              mutate(Lci = map_dbl(CI,
                                     .f = function(x) x[1]),
                     Uci = map_dbl(CI,
                                     .f = function(x) x[2])) %>%
              select(-CI)) %>%
  mutate(range = Uci - Lci,
         prop_range = range / N) %>%
  arrange(event_name, model)

#-----------------------------------------------------------------
# CPUE
#-----------------------------------------------------------------

# calculate effort
cpue_df = raw_df %>%
  select(event_name,
         site,
         date,
         crew,
         method,
         equipment,
         start_time:total_effort,
         total_day_effort) %>%
  distinct() %>%
  group_by(event_name,
           date) %>%
  summarise(across(total_effort,
                   sum),
            .groups = "drop") %>%
  # add how many pikeminnow were caught
  left_join(raw_df %>%
              filter(species == "Northern Pikeminnow") %>%
              filter(capture_type != "Recapture") %>%
              group_by(event_name, species, date) %>%
              summarise(n_fish = n(),
                        .groups = "drop")) %>%
  mutate(cpue = n_fish / total_effort)

cpue_df %>%
  ggplot(aes(x = date,
             y = cpue,
             color = event_name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ event_name,
             scales = 'free_x')


cpue_df %>%
  group_by(species, event_name) %>%
  summarise(across(c(total_effort, n_fish),
                   sum),
            .groups = "drop") %>%
  mutate(across(total_effort,
                ~ . / 60)) %>%
  mutate(cpue = n_fish / total_effort) %>%
  ggplot(aes(x = event_name,
             y = cpue,
             fill = event_name)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1",
                    name = "Event") +
  labs(x = "Event",
       y = "CPUE (Pikeminnow caught per hour)") +
  theme(legend.position = "none")
