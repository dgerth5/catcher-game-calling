library(readr)
library(tidyverse)

# pbp
statcast_20 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast20.csv")
statcast_21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast_22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")

statcast <- rbind(statcast_20, statcast_21, statcast_22)

two_strikes <- statcast %>%
  filter(strikes == 2) %>%
  filter(description != "pitchout" & description != "foul_bunt" & description != "missed_bunt" & description != "bunt_foul_tip" & description != "foul_pitchout") %>%
  mutate(cs = if_else(description == "called_strike", 1, 0),
         same_hand = if_else(stand == p_throws, 1, 0),
         hit_score_dif = bat_score - fld_score)
unique(two_strikes$description)

two_strikes$batter <- as.factor(two_strikes$batter)
two_strikes$fielder_2 <- as.factor(two_strikes$fielder_2)

library(lme4)
library(mixedup)

mod1 <- "cs ~ release_speed + same_hand + as.factor(balls) + abs(pfx_x)*abs(pfx_z) + plate_x*plate_z + (1|batter) + (1|fielder_2)"

m1 <- glmer(mod1,
            data = two_strikes,
            family = binomial(),
            nAGQ = 0,
            verbose = TRUE,
            control=glmerControl(optimizer = "nloptwrap"))

ranef_df <- extract_random_effects(m1)
catch_ranef <- ranef_df %>%
  filter(group_var == "fielder_2") %>%
  rename(MLBID = group)

catch_ranef$MLBID <- as.factor(catch_ranef$MLBID)

library(readxl)
statcast_ids <- read_excel("~/statcast-ids.xlsx") 
statcast_ids$MLBID <- as.factor(statcast_ids$MLBID)

catcher_rating <- left_join(catch_ranef, statcast_ids, by = "MLBID")
catcher_rating$odd_ratio <- exp(catcher_rating$value)
