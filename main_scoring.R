# GoT Death Pool

library(tidyverse)
library(googlesheets)
library(janitor)


# Character Status --------------------------------------------------------

# Update after each episode. Key: 
# episode: 0 = Alive
# episode: 1-6 = Died in that episode (there are 6 episodes in Season 8)
# wight: 0 = not a wight
# wight: 1 = been wighted

character_status <- tribble(
  ~character, ~episode, ~wight,
  "nymeria", 0, 0,
  "ghost", 0, 0,
  "hot_pie", 0, 0,
  "little_sam_gillys_child", 0, 0,
  "gilly", 0, 0,
  "samwell_tarly", 0, 0,
  "robin_arryn", 0, 0,
  "lyanna_mormont", 0, 0,
  "jorah_mormont", 0, 0,
  "qyburn", 0, 0,
  "lord_varys", 0, 0,
  "melisandre", 0, 0,
  "davos_seaworth", 0, 0,
  "daario_naharis", 0, 0,
  "dolorous_edd", 0, 0,
  "beric_dondarrion", 0, 0,
  "podrick_payne", 0, 0,
  "gendry", 0, 0,
  "bronn", 0, 0,
  "tormund_giantsbane", 0, 0,
  "brienne_of_tarth", 0, 0,
  "grey_worm", 0, 0,
  "missandei", 0, 0,
  "euron_greyjoy", 0, 0,
  "yara_greyjoy", 0, 0,
  "theon_greyjoy", 0, 0,
  "the_mountain", 0, 0,
  "the_hound", 0, 0,
  "arya_stark", 0, 0,
  "bran_stark", 0, 0,
  "sansa_stark", 0, 0,
  "cersei_lannister", 0, 0,
  "jaime_lannister", 0, 0,
  "tyrion_lannister", 0, 0,
  "rhaegal", 0, 0,
  "drogon", 0, 0,
  "jon_snow", 0, 0,
  "daenerys_targaryen", 0, 0)
  

# Load Player Sheet -------------------------------------------------------

# If you need to re-authenticate Google Drive, run these: 
#gs_deauth()
#gs_auth()

# Pull player sheet off Google Drive
player_sheet <- gs_title("Game of Thrones Death Pool (Responses)") %>%
  gs_read() %>%
  clean_names()

# Set up players' character guesses
character_guess <- player_sheet %>%
  select(email_address, choose_wisely_nymeria:choose_wisely_daenerys_targaryen) %>%
  gather(character, guess, -email_address) %>%
  mutate(character = str_remove(character, "choose_wisely_")) %>%
  mutate(guess = str_replace_all(guess, " ", "_"),
         guess = str_to_lower(guess))

# Attach and score actual results
character_scores <- character_guess %>%
  left_join(character_status, by = "character") %>%
  mutate(live_death_score = case_when(
    guess == "lives" & episode == 0 ~ 1, # guessed lives and is alive = 1 point
    guess == "lives" & episode != 0 ~ 0, # guessed lives but died = 0 points
    guess == "dies" & episode != 0 ~ 1,  # guessed dies and died = 1 point
    guess == "dies" & episode == 0 ~ 0,  # guessed dies and is still alive = 0 points
    guess == "becomes_a_wight" & episode != 0 ~ 1, # guessed wight and died = 1 point
    guess == "becomes_a_wight" & episode == 0 ~ 0  # guessed wight and died = 0 points
  )) %>%
  mutate(wight_score = case_when(
    guess == "becomes_a_wight" & episode != 0 & wight == 0 ~  -1, # guessed wight, died, is not a wight = -1
    guess == "becomes_a_wight" & episode != 0 & wight == 1 ~ 1,   # guessed wight, died, is a wight = 1
    TRUE ~ 0 # if guessed lives or dies = 0 points
  ))


character_scores %>% group_by(email_address) %>% summarise(sum(live_death_score), sum(wight_score))