library(googlesheets)
library(tidyverse)

responses <- gs_title("GOT") %>% 
  gs_read(ws = 1) %>% 
  rename(Team = `How do you know me?`) %>% 
  select(-Timestamp, -`Email Address`)

answers <- gs_title("GOT") %>% 
  gs_read(ws = 2) 

# points: 
# >0 - correct answer
# =0 - already ruled out (but question still undecided)
# <0 - incorrect answer
# NA - undecided

points <- answers %>% 
  filter(!is.na(points)) %>% 
  select(-category) %>% 
  arrange(week, -points)

category <- answers %>% 
  distinct(question, category)

got <- responses %>% 
  gather(question, answer,
         -Name, -Team) %>% 
  mutate(answer = str_replace_all(answer, "\\),", "\\);")) %>% 
  separate_rows(answer, sep = ";") %>% 
  mutate(answer = str_trim(answer)) %>% 
  left_join(category, by = "question") %>% 
  left_join(points, by = c("question", "answer")) %>%
  mutate(
    possible_points = case_when(
      answer %in% c("Die", "Live",
                    "Reanimated/Turned", 
                    "Not Seen/Unknown") ~ 10,
      !str_detect(answer, "\\d") ~ 50,
      TRUE ~ str_extract(answer, "\\(\\d.*\\)$") %>% 
        str_extract("(?<=\\().*(?=\\))") %>% 
        str_extract("\\d*") %>% 
        as.numeric()),
    answer_clean = str_remove(answer, " \\(\\d.*\\)$")
  )

points_earned <- got %>% 
  filter(!is.na(points)) %>% 
  count(Name, Team, 
        wt = points, name = "Points")

points_remaining <- got %>% 
  filter(is.na(points)) %>% 
  count(Name, Team, 
        wt = possible_points, name = "PPR")

got_points <- full_join(points_earned, 
                        points_remaining,
                        by = c("Name", "Team")) %>% 
  replace_na(list(Points = 0,
                  PPR = 0)) %>% 
  arrange(-Points, -PPR)

save(got, got_points, 
     file = here::here("got", "got-data.RData"))
