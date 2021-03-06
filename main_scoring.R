# GoT Death Pool
# Adam Stone (April 2019)
# How to use: Just run the whole thing and it'll calculate/upload results to Google Drive

library(tidyverse)
library(googlesheets)
library(janitor)
library(glue)

# Set Current Episode -----------------------------------------------------

current_episode = 6L


# Scoring Rules -----------------------------------------------------------


  # |            | guessed "lives" | guessed "dies" | guessed "wight" |
  # |------------|-----------------|----------------|-----------------|
  # | is living  | 1               | 0              | -1              |
  # | is dead    | 0               | 1              | -1              |
  # | is a wight | 0               | 0              | 2               |


# Save Player Responses ---------------------------------------------------
# One-time thing to pull player entries off Google Drive and save locally

# orig_player_responses <- gs_title("GoT Death Pool Responses") %>%
#   gs_read() %>%
#   write_csv("player_responses.csv")


# Character Status Tracker ------------------------------------------------

# Updated after each episode. Key: 
# L = alive
# D = dead
# W = turned into a wight/joined the Army of the Dead

# If you need to re-authenticate Google Drive, run these: 
# gs_deauth()
# gs_auth()

# Pull character status tracker off Google Drive
character_tracker <- gs_title("GoT Death Pool Character Tracker") %>%
  gs_read()

# Extract latest episode only 
character_status <- character_tracker %>%
  select(character, glue("episode_{current_episode}")) %>%
  rename(status = glue("episode_{current_episode}"))


# Character Status Scoring ------------------------------------------------

# Pull player responses off Google Drive
player_responses <- read_csv("player_responses.csv") %>%
  clean_names()

# Set up players' character guesses
player_guesses <- player_responses %>%
  select(name, nymeria:daenerys_targaryen) %>%
  gather(character, guess, -name) %>%
  mutate(guess = str_replace_all(guess, " ", "_"),
         guess = str_to_lower(guess))

# Attach and score results
player_accuracy <- player_guesses %>%
  left_join(character_status, by = "character") %>%
  mutate(score = case_when(
    guess == "lives" & status == "L" ~ 1,
    guess == "lives" & status == "D" ~ 0,
    guess == "lives" & status == "W" ~ 0,
    guess == "dies" & status == "L" ~ 0,
    guess == "dies" & status == "D" ~ 1,
    guess == "dies" & status == "W" ~ 0,
    guess == "wights" & status == "L" ~ -1,
    guess == "wights" & status == "D" ~ -1,
    guess == "wights" & status == "W" ~ 2
  )) %>%
  mutate(accuracy = case_when(
    guess == "lives" & status == "L" ~ 1,
    guess == "dies" & status == "D" ~ 1,
    guess == "wights" & status == "W" ~ 1,
    TRUE ~ 0
  ))

# Player scores (based on character status only)
player_scores <- player_accuracy %>% 
  group_by(name) %>% 
  summarise(characters_score = sum(score),
            characters_pct = round(sum(accuracy)/38,2)) %>% 
  arrange(desc(characters_score))


# Extras Scoring ----------------------------------------------------------

# If Daenerys is pregnant, change to "Yes"
is_daenerys_pregnant <- "No"

# Capture all spelling variations for night_king_killer & iron_throne_winner 
extras_accuracy <- player_responses %>%
  select(name, is_daenerys_pregnant_2_points:iron_throne_winner_6_points) %>%
  rename(daenerys_pregnant_guess = is_daenerys_pregnant_2_points,
         night_kingslayer = night_kingslayer_4_points,
         iron_throne_winner = iron_throne_winner_6_points) %>%
  mutate(daenerys_score = case_when(
    daenerys_pregnant_guess == "Yes" & is_daenerys_pregnant == "Yes" ~ 2,
    daenerys_pregnant_guess == "No" & is_daenerys_pregnant == "No" ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(night_kingslayer_score = case_when(
    str_detect(str_to_lower(night_kingslayer), 'arya') ~ 4,
    TRUE ~ 0
  )) %>%
  mutate(iron_throne_score = case_when(
    str_detect(str_to_lower(iron_throne_winner), 'no one') ~ 6,
    TRUE ~ 0
  ))

extras_scores <- extras_accuracy %>% 
  select(name, daenerys_score:iron_throne_score)



# Total Scoring -----------------------------------------------------------

# Calculate total scores 
total_scores <- player_scores %>%
  left_join(extras_scores, by = "name") %>%
  mutate(bonuses_score = daenerys_score + night_kingslayer_score + iron_throne_score) %>%
  mutate(total_score = characters_score + bonuses_score) %>%
  arrange(desc(total_score)) %>%
  select(name, total_score, characters_pct, characters_score, bonuses_score, everything())



# Death Results -----------------------------------------------------------

total_players <- nrow(player_responses)

deaths <- character_status %>%
  filter(status == "D") %>%
  add_column(guess = "dies") %>%
  left_join(player_guesses, by = c("guess", "character")) %>%
  count(character, guess) %>%
  rename(correct_guesses = n) %>%
  mutate(total_guesses = total_players,
         percent_correct = correct_guesses/total_guesses)

wights <- character_status %>%
  filter(status == "W") %>%
  add_column(guess = "wights") %>%
  left_join(player_guesses, by = c("guess", "character")) %>%
  drop_na() %>%
  count(character, guess) %>%
  rename(correct_guesses = n) %>%
  mutate(total_guesses = total_players,
         percent_correct = correct_guesses/total_guesses)

# An ugly hack to handle Lyanna Mormont wighting (which no one guessed).
death_results <- bind_rows(deaths, wights) %>%
  mutate(percent_correct = round(percent_correct, 2)) %>%
  add_row(character = 'lyanna_mormont',
          guess = 'wights',
          correct_guesses = 0,
          total_guesses = 39,
          percent_correct = 0) %>%
  arrange(character)

# Write "no deaths" if none occurred.
if (nrow(death_results) == 0) {
  death_results <- death_results %>%
    add_row(character = "no deaths!")
}

# Uploading Results -------------------------------------------------------

# Upload leaderboard
episode_results <- gs_title("GoT Death Pool Results")
episode_results <- episode_results %>%
  gs_ws_new(ws_title = glue("e{current_episode}_leaderboard"), input = total_scores)

# Upload death results
episode_results <- episode_results %>%
  gs_ws_new(ws_title = glue("e{current_episode}_death_results"), input = death_results)



# Calculating Streaks -----------------------------------------------------

# Pull all scores off Google Drive
episodes <- seq(1, current_episode)

pull_scores <- function(x){
  gs_title("GoT Death Pool Results") %>%
  gs_read(ws = glue("e{x}_leaderboard"))
}

all_scores <- map_df(episodes, pull_scores, .id = 'episode')

# Calculate streaks (per episode and overall)
streaks <- all_scores %>%
    select(name, episode, total_score) %>%
    mutate(episode = glue("e{episode}")) %>%
    spread(episode, total_score) %>%
    mutate(streak_12 = e2 - e1,
         streak_23 = e3 - e2,
         streak_34 = e4 - e3,
         streak_45 = e5 - e4,
         streak_56 = e6 - e5) %>%
  mutate(current_streak = case_when(
    streak_23 >= 0 & streak_34 >= 0 & streak_45 >= 0 & streak_56 >= 0 ~ (streak_23 + streak_34 + streak_45 + streak_56),
    streak_56 < 0 ~ 0,
    streak_45 < 0 ~ streak_56,
    streak_34 < 0 ~ (streak_45 + streak_56),
    streak_23 < 0 ~ (streak_34 + streak_45 + streak_56)
  )) %>%
  arrange(desc(e5))
  
# Upload
episode_results <- episode_results %>%
  gs_ws_new(ws_title = glue("e{current_episode}_streaks"), input = streaks)





# Character and Bonus Guesses ---------------------------------------------


character_guesses <- player_guesses %>% 
  group_by(character) %>% 
  count(guess) %>% 
  spread(guess, n) %>% 
  mutate(expires = dies + wights)

episode_results %>%
  gs_ws_new(ws_title = glue("final_character_guesses"), input = character_guesses)

player_responses %>% count(is_daenerys_pregnant_2_points)
player_responses %>% count(night_kingslayer_4_points)
player_responses %>% count(iron_throne_winner_6_points) %>%
