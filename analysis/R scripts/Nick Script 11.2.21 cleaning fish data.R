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
require(Hmisc)
require(Matching)
require(Rcapture)
require(nlstools)
require(stringi)
require(MASS)

deadwater <- read_csv("analysis/data/raw data/deadwater_cmr_effort_20210614.csv") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%y"))

NPM = deadwater %>%
  filter(Species == "Northern Pikeminnow")

Clean_NPM = NPM %>%
  mutate(Fish_or_Parts = case_when(stri_detect_fixed(StomachContents, "Fish") ~ "Fish or Fish Parts",
                                   stri_detect_fixed(StomachContents, "Empty") ~ "Empty", TRUE ~ "Other")) %>%
  drop_na(Length)



#Histogram Ogle

Clean_NPM$NPM_50=lencat(Clean_NPM$Length, w = 50)

esquisse::esquisser(Clean_NPM)


##group_by(Species) %>% 165,640,25
#custbins=seq(150,675,25)
#hist(~Length,data=Clean_NPM,xlab="TL (mm)", ylim=c(0,500))
##xtick<-seq(150,650,20)####creating minor tick marks
#axis(side=1, at=xtick, labels = FALSE)####setting minor tick marks


ggplot(data=Clean_NPM, aes(Length)) +
  geom_histogram(breaks=seq(150, 650, by = 50),
                 col="#DB740E",
                 fill="#DB740E",
                 alpha = .2) +
  labs(x="TL (mm)", y="Frequency") +
  xlim(c(100,700)) +
  ylim(c(0,500)) +
  theme_classic()


Clean_NPM %>%
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
