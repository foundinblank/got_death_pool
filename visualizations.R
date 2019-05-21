library(tidyverse)
library(googlesheets)
library(janitor)
library(glue)


all_scores <- gs_title("GoT Death Pool Results") %>%
  gs_read(ws = "e6_streaks")

score_changes <- all_scores %>%
  select(name:e6) %>%
  gather(episode, score, e1:e6) %>%
  mutate(episode = str_sub(episode, 2,2),
         episode = as.integer(episode))

score_changes %>%
  ggplot(aes(x = episode, y = score, group = name)) +
  geom_line()
