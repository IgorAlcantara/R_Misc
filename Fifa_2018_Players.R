

# Pacotes Necessários:
# tidyverse
# stringr
# lubridate
# cowplot
# tabulizer
# rvest

suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(cowplot))
suppressMessages(library(rvest))

library(tabulizer)
library(stringr)

url <- "https://github.com/IgorAlcantara/R_Misc/raw/master/Docs/fifa_player_list_1.pdf"
fifa_players <- extract_tables(url, output = "data.frame")

pdf_data <- bind_rows(fifa_players) %>% 
  as_tibble() %>% 
  rename(team = Team,
         number = X.,
         position = Pos.,
         name = FIFA.Popular.Name,
         birth_date = Birth.Date,
         shirt_name = Shirt.Name,
         club = Club,
         height = Height,
         weight = Weight) %>%
  mutate(team = case_when(
    team == "Korea Republic" ~ "South Korea",
    team == "IR Iran" ~ "Iran",
    TRUE ~ team)) %>% 
  mutate(birth_date = dmy(birth_date),
         league = str_sub(club, -4, -2),
         club = str_sub(club, end = -7),
         age = interval(birth_date, "2018-06-14") / years(1))


html <- read_html("https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads")

country <- html_nodes(html, ".mw-headline") %>% 
  html_text() %>%
  as_tibble() %>% 
  filter(! str_detect(value, "Group")) %>% 
  slice(1:32)

number <- html_nodes(html, ".plainrowheaders td:nth-child(1)") %>% 
  html_text()

name <- html_nodes(html, "th a") %>% 
  html_text() %>% 
  as_tibble() %>% 
  filter(! str_detect(value, "^captain$")) %>% 
  slice(1:736)

caps <- html_nodes(html, ".plainrowheaders td:nth-child(5)") %>% 
  html_text()

wiki_data <- tibble(
  number = as.numeric(number),
  name = name$value,
  team = rep(country$value, each = 23),
  caps = as.numeric(caps))

dados_completos <- left_join(select(pdf_data, -name), wiki_data, by = c("team", "number"))

hist(dados_completos$age, col="green")


dados_completos %>% 
  group_by(team) %>% 
  summarise(elite = mean(league %in% 
                           c("ENG", "ESP", "GER", "ITA", "FRA"))) %>%
  arrange(desc(elite)) %>% 
  slice(c(1:8, 29:32))



