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

# Load Player Sheet -------------------------------------------------------

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
player_scores <- player_guesses %>%
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


player_scores %>% group_by(email_address) %>% summarise(score = sum(score)) %>% arrange(desc(score))
