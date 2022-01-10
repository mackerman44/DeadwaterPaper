# Author: Kevin See
# Purpose: import data from Deadwater sampling
# Created: 11/9/21
# Last Modified: 12/2/21
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
# read in data
#-----------------------------------------------------------------
raw_df = readxl::read_excel(here("analysis/data/raw_data/Deadwater_predation_mark_recapture_11.09.21.xlsx")) %>%
  clean_names() %>%
  mutate(across(date,
                ymd)) %>%
  mutate(across(c(start_time, end_time),
                ~ ymd_hms(paste(year(date), month(date), day(date), hour(.), minute(.), second(.))))) %>%
  # split into sampling periods
  mutate(event_name = if_else(month(date) > 8,
                              paste("Fall", year(date), sep = "_"),
                              paste("Spring", year(date), sep = "_")))

# focus on pikeminnow
cmr_df = raw_df %>%
  filter(count > 0) %>%
  # pull out only Northern Pikeminnow
  filter(species == "Northern Pikeminnow") %>%
  mutate(mark_release = if_else(fish_status == "Live" &
                                  capture_type == "Mark",
                                T, F),
         recap = if_else(capture_type == "Recapture",
                         T, F))


#------------------------------------------------------------
# CPUE
#------------------------------------------------------------
# pull out and summarize effort
# account for time and number of crew members
effort_df = raw_df %>%
  select(event_name,
         site,
         date,
         crew,
         crew_count,
         method,
         equipment,
         start_time:total_effort,
         total_day_effort) %>%
  distinct() %>%
  mutate(across(total_effort,
                ~ . * crew_count)) %>%
  group_by(event_name,
           date) %>%
  summarise(across(c(total_effort,
                     crew_count),
                   sum),
            .groups = "drop")

cpue_df = effort_df %>%
  # add how many pikeminnow were caught
  left_join(cmr_df %>%
              group_by(event_name, species, date) %>%
              summarise(n_fish = n(),
                        n_large_fish = sum(length>=228,
                                           na.rm = T),
                        .groups = "drop")) %>%
  mutate(cpue = n_fish / total_effort,
         cpue_per_day = n_fish / crew_count,
         cpue_large_per_day = n_large_fish / crew_count)



#------------------------------------------------------------
# Abundance
#------------------------------------------------------------
# using Schnabel and Schumacher-Eschmeyer estimators
N_mods = cmr_df %>%
  # filter out spring 2021 (no recapture)
  filter(event_name != "Spring_2021") %>%
  group_by(event_name, species, date) %>%
  summarise(n = n(),
            m = sum(capture_type == "Recapture", na.rm = T),
            u = sum(capture_type == "Mark" | is.na(capture_type)),
            R = sum(fish_status == "Live", na.rm = T),
            .groups = "drop") %>%
  group_by(event_name, species) %>%
  mutate(M = lag(R - m),
         M = replace_na(M, 0),
         M = cumsum(M)) %>%
  nest() %>%
  crossing(model = c("Schnabel",
                     "SchumacherEschmeyer")) %>%
  mutate(mr_model = map2(data,
                         model,
                         .f = function(x, y) {
                           with(x,
                                mrClosed(n = n,
                                         m = m,
                                         R = R,
                                         method = y,
                                         chapman.mod = TRUE))
                         }),
         N = map_dbl(mr_model,
                     .f = summary),
         CI = map(mr_model,
                  .f = confint),
         Lci = map_dbl(CI,
                       .f = function(x) x[1]),
         Uci = map_dbl(CI,
                       .f = function(x) x[2])) %>%
  ungroup() %>%
  select(-CI)

# diagnostic plots
mod = N_mods %>%
  # slice(1) %>%
  slice(3) %>%
  pull(mr_model) %>%
  magrittr::extract2(1)

plot(mod, loess = T)
tibble(m = mod$m,
       n = mod$n,
       M = mod$M) %>%
  ggplot(aes(x = M,
             y = m/n)) +
  geom_point(size = 4) +
  labs(x = "Marked in Population",
       y = "Prop. Recaptures in Sample") +
  geom_smooth(method = 'loess',
              se = F,
              span = 0.9) +
  geom_smooth(method = "lm",
              se = F,
              color = "red",
              linetype = 2)


N_mods %>%
  filter(model == "Schnabel") %>%
  unnest(data) %>%
  group_by(event_name) %>%
  filter(m > 0 |
           date == min(date)) %>%
  ggplot(aes(x = M,
             y = m/n)) +
  geom_point(size = 4) +
  facet_wrap(~ event_name,
             scales = "free") +
  labs(x = "Marked in Population",
       y = "Prop. Recaptures in Sample") +
  geom_smooth(method = 'loess',
              se = F,
              span = 2) +
  geom_smooth(method = "lm",
              se = F,
              color = "red",
              linetype = 2)


# -----------------------------------------------------------------
# using Lincoln-Petersen model, combining first 4 and last four days
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

cmr_df$cap_period = NA_character_
for(i in 1:nrow(cap_periods)) {
  my_int = cap_periods$int[i]
  index = which(cmr_df$date %within% my_int)
  cmr_df$cap_period[index] = cap_periods$cap_period[i]
}


LP_mod = cmr_df %>%
  # filter out spring 2021 (no recapture)
  filter(event_name != "Spring_2021") %>%
  group_by(event_name, species) %>%
  summarise(M = sum(cap_period == "Mark" & fish_status == "Live", na.rm = T),
            n = sum(cap_period == "Recapture"),
            m = sum(cap_period == "Recapture" & capture_type == "Recapture", na.rm = T),
            .groups = "drop") %>%
  nest(data = -c(event_name, species)) %>%
  crossing(model = c("Petersen",
                     "Chapman")) %>%
  mutate(mr_model = map2(data,
                         model,
                         .f = function(x, y) {
                           with(x,
                                mrClosed(M = M,
                                         n = n,
                                         m = m,
                                         method = y))
                         }),
         N_summ = map(mr_model,
                      .f = summary,
                      incl.SE = T),
         CI = map(mr_model,
                  .f = confint)) %>%
  mutate(N = map_dbl(N_summ,
                     .f = function(x) x[1]),
         SE = map_dbl(N_summ,
                      .f = function(x) x[2]),
         Lci = map_dbl(CI,
                       .f = function(x) x[1]),
         Uci = map_dbl(CI,
                       .f = function(x) x[2])) %>%
  select(-N_summ, -CI)

#-----------------------------------------------------------------
# compare all results
all_est = LP_mod %>%
  select(event_name, species, model, N:Uci) %>%
  bind_rows(N_mods %>%
              select(event_name, species, model, N:Uci)) %>%
  mutate(range = Uci - Lci,
         prop_range = range / N) %>%
  mutate(across(event_name,
                ~ str_replace(., "_", " "))) %>%
  mutate(model = fct_recode(model,
                            "Schumacher-Eschmeyer" = "SchumacherEschmeyer")) %>%
  arrange(event_name, model)

all_est
all_est %>%
  ggplot(aes(x = model,
             y = N,
             color = model)) +
  geom_errorbar(aes(ymin = Lci,
                    ymax = Uci),
                width = 0.1) +
  geom_point(size = 4) +
  facet_wrap(~ event_name) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Estimator",
       y = "Pikeminnow Abundance") +
  theme(legend.position = "none")



#-----------------------------------------------------------------
# generate estimate of spring abundance based on CPUE
#-----------------------------------------------------------------
total_cpue = cpue_df %>%
  mutate(across(event_name,
                ~ str_replace(., "_", " "))) %>%
  group_by(species, event_name) %>%
  summarise(across(c(total_effort,
                     n_fish,
                     n_large_fish,
                     angler_days = crew_count),
                   sum),
            .groups = "drop") %>%
  mutate(across(total_effort,
                ~ . / 60)) %>%
  mutate(cpue = n_fish / total_effort,
         cpue_per_day = n_fish / angler_days,
         cpue_large_per_day = n_large_fish / angler_days)


spring_abund = all_est %>%
  select(-range, -prop_range) %>%
  inner_join(total_cpue,
             by = c("event_name", "species")) %>%
  select(species, model,
         model_event = event_name,
         N_F = N,
         Lci_F = Lci,
         Uci_F = Uci,
         cpue_F = cpue) %>%
  full_join(total_cpue %>%
              filter(str_detect(event_name, "Spring")) %>%
              select(species,
                     cpue_S = cpue),
            by = "species") %>%
  mutate(N_S = N_F * (cpue_S / cpue_F),
         Lci_S = Lci_F * (cpue_S / cpue_F),
         Uci_S = Uci_F * (cpue_S / cpue_F))



#-----------------------------------------------------------------
# CPUE
#-----------------------------------------------------------------
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


#-----------------------------------------------------------------
# what if marked fish were not as susceptible to recapture?

N_mods2 = N_mods %>%
  crossing(perc_unavail = c(0, 0.1, 0.3, 0.5, 0.7, 0.9)) %>%
  mutate(data = map2(data,
                     perc_unavail,
                    .f = function(x, y) {
                      x %>%
                        mutate(across(R,
                                      ~ . * (1 - y))) %>%
                        mutate(M = lag(R - m),
                               M = replace_na(M, 0),
                               M = cumsum(M))
                    })) %>%
  mutate(mr_model = map2(data,
                         model,
                         .f = function(x, y) {
                           with(x,
                                mrClosed(n = n,
                                         m = m,
                                         R = R,
                                         method = y,
                                         chapman.mod = TRUE))
                         }),
         N = map_dbl(mr_model,
                     .f = summary),
         CI = map(mr_model,
                  .f = confint),
         Lci = map_dbl(CI,
                       .f = function(x) x[1]),
         Uci = map_dbl(CI,
                       .f = function(x) x[2])) %>%
  ungroup() %>%
  select(-CI) %>%
  group_by(event_name,
           model) %>%
  mutate(perc_N = N / N[perc_unavail == 0],
         perc_avail = 1 - perc_unavail)

all_equal(N_mods %>%
            select(event_name, model,
                   N:Uci),
          N_mods2 %>%
            filter(perc_unavail == 0) %>%
            select(event_name, model,
                   N:Uci))

N_mods2 %>%
  ggplot(aes(x = perc_unavail,
             y = N,
             color = model)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ event_name,
             scales = "free_y") +
  theme(legend.position = "bottom")

N_mods2 %>%
  ggplot(aes(x = perc_avail,
             y = perc_N,
             color = model)) +
  geom_point(aes(shape = event_name)) +
  geom_abline(linetype = 2) +
  geom_smooth()

N_mods2 %>%
  filter(model == "Schnabel")

# I don't know how to do something similar for single census models
