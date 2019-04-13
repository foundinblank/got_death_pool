# GoT Death Pool

library(tidyverse)
library(googlesheets)
library(janitor)
library(glue)


# Scoring Rules -----------------------------------------------------------


  # |            | guessed "lives" | guessed "dies" | guessed "wight" |
  # |------------|-----------------|----------------|-----------------|
  # | is living  | 1               | 0              | -1              |
  # | is dead    | 0               | 1              | -1              |
  # | is a wight | 0               | 0              | 2               |


# Set Current Episode -----------------------------------------------------

current_episode = 0L

# Character Status --------------------------------------------------------

# Update after each episode. Key: 
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
player_responses <- gs_title("Game of Thrones Death Pool (Responses)") %>%
  gs_read() %>%
  clean_names()

# Set up players' character guesses
player_guesses <- player_responses %>%
  select(email_address, choose_wisely_nymeria:choose_wisely_daenerys_targaryen) %>%
  gather(character, guess, -email_address) %>%
  mutate(character = str_remove(character, "choose_wisely_")) %>%
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
    guess == "becomes_a_wight" & status == "L" ~ -1,
    guess == "becomes_a_wight" & status == "D" ~ -1,
    guess == "becomes_a_wight" & status == "W" ~ 2
  ))

# Player scores (based on character status only)
player_scores <- player_accuracy %>% 
  group_by(email_address) %>% 
  summarise(characters_score = sum(score)) %>% 
  arrange(desc(characters_score))


# Extras Scoring ----------------------------------------------------------

# If Daenerys is pregnant, change to "Yes"
is_daenerys_pregnant <- "No"

# Capture all spelling variations for night_king_killer & iron_throne_winner 
extras_accuracy <- player_responses %>%
  select(email_address, is_daenerys_pregnant_2_point:who_holds_the_iron_throne_at_the_end_6_points) %>%
  rename(daenerys_pregnant_guess = is_daenerys_pregnant_2_point,
         night_king_killer = who_kills_the_night_king_4_points,
         iron_throne_winner = who_holds_the_iron_throne_at_the_end_6_points) %>%
  mutate(daenerys_score = case_when(
    daenerys_pregnant_guess == "Yes" & is_daenerys_pregnant == "Yes" ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(night_king_score = case_when(
    night_king_killer == "whoknows" ~ 4,
    TRUE ~ 0
  )) %>%
  mutate(iron_throne_score = case_when(
    iron_throne_winner == "whoknows" ~ 6,
    TRUE ~ 0
  ))

extras_scores <- extras_accuracy %>% 
  select(email_address, daenerys_score:iron_throne_score)



# Total Scoring -----------------------------------------------------------

# Get first name
first_name <- function(x){
  str_split(x, " ")[[1]][1]
}

# Get names for each email address
player_names <- player_responses %>%
  select(name, email_address) %>%
  mutate(name = case_when(
    email_address == "jilly5ca@me.com" ~ "Jill Stone",
    TRUE ~ name
  )) %>%
  mutate(name = map(name, first_name)) %>%
  mutate(name = as.character(name))

# Calculate total scores and attach player names
total_scores <- player_scores %>%
  left_join(extras_scores, by = "email_address") %>%
  mutate(total_score = characters_score + daenerys_score + night_king_score + iron_throne_score) %>%
  arrange(desc(total_score)) %>%
  left_join(player_names, by = "email_address") %>%
  select(name, email_address, everything()) 



# Death Results -----------------------------------------------------------

total_players <- nrow(player_responses)

deaths <- character_status %>%
  filter(status == "D") %>%
  add_column(guess = "dies") %>%
  left_join(player_guesses, by = c("guess", "character")) %>%
  count(character, guess) %>%
  rename(correct_guesses = n) %>%
  mutate(total_guesses = total_players,
         percent_correct = correct_guesses/total_guesses,
         episode = current_episode)

wights <- character_status %>%
  filter(status == "W") %>%
  add_column(guess = "becomes_a_wight") %>%
  left_join(player_guesses, by = c("guess", "character")) %>%
  count(character, guess) %>%
  rename(correct_guesses = n) %>%
  mutate(total_guesses = total_players,
         percent_correct = correct_guesses/total_guesses,
         episode = current_episode)

death_results <- bind_rows(deaths, wights) %>%
  mutate(percent_correct = round(percent_correct, 2))


# Uploading Results -------------------------------------------------------

# Upload leaderboard
episode_results <- gs_title("GoT Death Pool Results")
episode_results <- episode_results %>%
  gs_ws_new(ws_title = glue("e{current_episode}_leaderboard"), input = total_scores)

# Upload death results
episode_results <- episode_results %>%
  gs_ws_new(ws_title = glue("e{current_episode}_death_results"), input = death_results)
