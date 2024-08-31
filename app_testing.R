library(nflfastR)
library(dplyr)


filtered_data_pull <- nflfastR::load_pbp(2023) %>%
  mutate(minutes_remaining = game_seconds_remaining / 60) %>%
  filter(posteam == 'SEA',
         yardline_100 >= 0,
         yardline_100 <= 100,
         ('All' == 'All') | (down %in% c(1,2,3,4)),
         ('Yes' == 'Yes' & ((qtr < 5 & minutes_remaining <= 60 & minutes_remaining >= 15) | qtr == 5))  |
           ('Yes' == 'No' & qtr < 5 & minutes_remaining <= 60 & minutes_remaining >= 15),
         ydstogo >= 10,
         ydstogo <= 10,
         wp*100 >= 30,
         wp*100 <= 75,
         (pass == 1 | rush == 1) & play_type != 'no_play'
  )

seahawks_testing <- nflfastR::load_pbp(2023) %>%
  filter(is.na(epa) == FALSE,
         (pass == 1 | rush == 1) & play_type %in% c('pass', 'run'),
         down == 1,
         ydstogo == 10,
         posteam == 'SEA') %>%
  mutate(count = n_distinct(play_id, old_game_id))

ravens_test <- nflfastR::load_pbp(2023) %>%
  filter(posteam == 'BAL',
         pass == 1,
         is.na(pass_length) == TRUE,
         is.na(pass_location) == TRUE)

ravnes_cannon_issue <- nflfastR::load_pbp(2023) %>%
  filter(down == 3,
         yardline_100 > 80,
         posteam == 'BAL')

team_info <- teams_colors_logos

tb_issue <- nflfastR::load_pbp(2023) %>%
  filter(
         yardline_100 < 10,
         ydstogo < 4,
         posteam == 'TB',
         pass == 1,
         !is.na(two_point_conv_result)) %>%
  select(pass_location, pass_length, air_yards)

run_locations <- nflfastR::load_pbp(2023) %>%
  distinct(run_gap)

pass_lengths <- nflfastR::load_pbp(2023) %>%
  distinct(pass_length)


seahawks_play_type_testing <- nflfastR::load_pbp(2023) %>%
  filter(is.na(epa) == FALSE,
         (pass == 1 | rush == 1) & play_type %in% c('pass', 'run'),
         posteam == 'SEA',
         is.na(two_point_conv_result)) %>%
  mutate(play_outcome =
           case_when(sack == 1 ~ 'dropback resulting in sack',
                     qb_scramble == 1 ~ 'QB scramble',
                     pass == 1 & pass_length == 'deep' ~ 'deep pass',
                     pass == 1 & pass_length == 'short' ~ 'short pass',
                     rush == 1 & (run_location == 'middle' | run_gap == 'guard') ~ 'inside run',
                     rush == 1 & run_gap %in% c('tackle', 'end') ~ 'outside run')) %>%
  filter(is.na(play_outcome))
