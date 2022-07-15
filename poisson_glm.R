

# Setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)


# Read ----------------------------------------------------------------------------------------

rankings <- read_csv(here::here("data/fifa_womens_rankings.csv"))
training_set_wide <- read_csv(here::here("data", "training_data.csv"))
euro_matchups <- read_csv(here::here("data", "euro_matchups.csv"))


# Preparation ---------------------------------------------------------------------------------

win <- training_set_wide %>%
  select(game_id, win_team, win_score, win_points, win_point_diff) %>% 
  mutate(outcome = "win") %>% 
  rename(team = 2, score = 3, points = 4, point_diff = 5)

lose <- training_set_wide %>% 
  select(game_id, lose_team, lose_score, lose_points, lose_point_diff) %>% 
  mutate(outcome = "lose") %>% 
  rename(team = 2, score = 3, points = 4, point_diff = 5)

training_set_long <- bind_rows(win, lose)


# Poisson regression --------------------------------------------------------------------------

poisson_glm <- glm(score ~ points + point_diff, 
                   family = "poisson", 
                   data = training_set_long)

# Prediction ----------------------------------------------------------------------------------

now_points <- rankings %>% 
  filter(ranking_date == max(ranking_date)) %>% 
  select(team = country_name,
         points)

prediction_set <- euro_matchups %>% 
  left_join(now_points, by = c("team")) %>% 
  rename(team_points = points) %>% 
  left_join(now_points, by = c("opponent" = "team")) %>% 
  rename(opponent_points = points) %>% 
  rename(points = team_points) %>% 
  mutate(point_diff = points - opponent_points)

euro_glm_pred <- prediction_set %>% 
  mutate(goals_pred = predict(poisson_glm, newdata = .,
                              type = "response"))

