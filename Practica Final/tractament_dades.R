library(readr)
library(dplyr)
library(tidyr)
library(stringr)

setwd("~/UOC/2024 Tardor/Visualització de Dades/Practica/Dataset")

# Importem el dataset de les característiques de les cançons guanyadores
song_data <- read_csv("song_data.csv")

# Dataframe per la clau de la cançó
key_data <- song_data %>%
  group_by(key) %>%
  reframe(
    frequency = n(),
    key_mode = ifelse(str_detect(key, "Minor"), "Menor", "Major"),
    is_winner = sum(final_place == 1, na.rm = TRUE)
  ) %>%
  distinct()

write.csv(key_data, "key_data.csv", row.names = FALSE)

# Seleccio de les cançons guanyadores
winner_songs <- song_data %>%
  filter(final_place == 1) %>%
  select(c(1, 5:9, 15:24, 32:33)) %>%
  mutate(
    key_mode = ifelse(str_detect(key, "Minor"), "Menor", "Major")
  ) %>%
  mutate(
    loudness = as.integer(str_replace(loudness, " dB", "")),
    across(
      c(BPM, energy, danceability, happiness, loudness, acousticness, instrumentalness, liveness, speechiness),
      as.integer
    )
  ) %>%
  mutate(
    across(
      c(BPM, energy, danceability, happiness, loudness, acousticness, instrumentalness, liveness, speechiness),
      .fns = list(norm = ~ round((.x / max(.x, na.rm = TRUE)) * 100)),
      .names = "{.col}_norm"
    )
  ) %>%
  mutate(
    winner = paste0(artist_name, " - ", "\"", song_name, "\"", " (", country, ")", sep = " ")
  )

write.csv(winner_songs, "winner_songs_music.csv", row.names = FALSE)

gender_stats <- song_data %>%
  group_by(year) %>%
  summarize(
    male_count = sum(gender == "Male", na.rm = TRUE),
    mix_count = sum(gender == "Mix", na.rm = TRUE),
    mix_left = -mix_count/2,
    mix_right = mix_count/2,
    female_count = sum(gender == "Female", na.rm = TRUE),
    .groups = "drop"
  )

write.csv(gender_stats, "gender_stats.csv", row.names = FALSE)

# Ranking del jurat i televot
ranked_data <- song_data %>%
  filter(!is.na(final_place)) %>%
  group_by(year) %>%
  mutate(
    ranking_televote = dense_rank(desc(final_televote_points) + desc(final_jury_points) * 1e-6),
    ranking_jury = dense_rank(desc(final_jury_points) + desc(final_televote_points) * 1e-6),
    is_winner = ifelse(final_place == 1, "Si", "No")
  ) %>%
  ungroup() %>%
  select(year, country, final_televote_points, final_jury_points, final_place, ranking_televote, ranking_jury, is_winner)


# Importem les votacions del jurat
X2016_jury_results <- read_csv("Final Results/Jury/2016_jury_results.csv")
X2017_jury_results <- read_csv("Final Results/Jury/2017_jury_results.csv")
X2018_jury_results <- read_csv("Final Results/Jury/2018_jury_results.csv")
X2019_jury_results <- read_csv("Final Results/Jury/2019_jury_results.csv")
X2021_jury_results <- read_csv("Final Results/Jury/2021_jury_results.csv")
X2022_jury_results <- read_csv("Final Results/Jury/2022_jury_results.csv")
X2023_jury_results <- read_csv("Final Results/Jury/2023_jury_results.csv")

# Importem les votacions del públic
X2016_televote_results <- read_csv("Final Results/Televote/2016_televote_results.csv")
X2017_televote_results <- read_csv("Final Results/Televote/2017_televote_results.csv")
X2018_televote_results <- read_csv("Final Results/Televote/2018_televote_results.csv")
X2019_televote_results <- read_csv("Final Results/Televote/2019_televote_results.csv")
X2021_televote_results <- read_csv("Final Results/Televote/2021_televote_results.csv")
X2022_televote_results <- read_csv("Final Results/Televote/2022_televote_results.csv")
X2023_televote_results <- read_csv("Final Results/Televote/2023_televote_results.csv")


jury_dataframes <- list(X2016_jury_results, X2017_jury_results, X2018_jury_results,
                        X2019_jury_results, X2021_jury_results, X2022_jury_results, X2023_jury_results)
jury_years <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023)

televote_dataframes <- list(X2016_televote_results, X2017_televote_results, X2018_televote_results,
                            X2019_televote_results, X2021_televote_results, X2022_televote_results, X2023_televote_results)
televote_years <- c(2016, 2017, 2018, 2019, 2021, 2022, 2023)

# Funció per processar els fitxera
process_dataframe <- function(df, year) {
  df %>%
    mutate(Year = year) %>%
    pivot_longer(
      cols = 5:ncol(df),
      names_to = "Source_country",
      values_to = "Points"
    ) %>%
    filter(!is.na(Points))
}

# Processem els jurats
jury_results <- Map(process_dataframe, jury_dataframes, jury_years) %>%
  bind_rows()

# Processem el televot
televote_results <- Map(process_dataframe, televote_dataframes, televote_years) %>%
  bind_rows()

# Combinem resultats
all_results <- bind_rows(
  jury_results %>% mutate(Vote_type = "Jury"),
  televote_results %>% mutate(Vote_type = "Televote")
)

all_results <- all_results %>%
  mutate(Points_type = ifelse(Points < 8, "Low", "High")) %>%
  filter(Source_country != "Rest of the World") %>%
  select(-c(2:4))

# Diccionari de paisos i geolocalitzacions
country_dict <- data.frame(
  Source_country = c("Austria", "Iceland", "Czech Republic", "Ireland", "Georgia", 
                     "Switzerland", "Denmark", "Armenia", "Bulgaria", "Netherlands", 
                     "Israel", "Belarus", "Germany", "Norway", "Australia", "Lithuania", 
                     "Macedonia", "Ukraine", "Slovenia", "Montenegro", "Bosnia and Herzegovina", 
                     "Finland", "Belgium", "Croatia", "Poland", "Hungary", "Sweden", "San Marino", 
                     "Spain", "France", "Latvia", "Serbia", "Estonia", "Italy", "Cyprus", "Russia", 
                     "Greece", "Azerbaijan", "Albania", "Malta", "United Kingdom", "Moldova", 
                     "Romania", "Portugal", "North Macedonia", "Czechia", "Rest of the World"),
  country_code = c("AUT", "ISL", "CZE", "IRL", "GEO", 
                   "CHE", "DNK", "ARM", "BGR", "NLD", 
                   "ISR", "BLR", "DEU", "NOR", "AUS", "LTU", 
                   "MKD", "UKR", "SVN", "MNE", "BIH", 
                   "FIN", "BEL", "HRV", "POL", "HUN", "SWE", "SMR", 
                   "ESP", "FRA", "LVA", "SRB", "EST", "ITA", "CYP", "RUS", 
                   "GRC", "AZE", "ALB", "MLT", "GBR", "MDA", 
                   "ROU", "PRT", "MKD", "CZE", "ROW"),
  latitude = c(47.5162, 64.9631, 49.8175, 53.4129, 42.3154,
               46.8182, 56.2639, 40.0691, 42.7339, 52.1326,
               31.0461, 53.9006, 51.1657, 60.4720, -25.2744, 55.1694,
               41.6086, 48.3794, 46.1512, 42.7087, 43.9159,
               61.9241, 50.5039, 45.1, 51.9194, 47.1625, 60.1282, 43.9334,
               40.4637, 46.6034, 56.8796, 44.0165, 58.5953, 41.8719, 35.1264, 61.5240,
               39.0742, 40.1431, 41.1533, 35.9375, 55.3781, 47.4116,
               45.9432, 39.3999, 41.6086, 49.8175, NA),
  longitude = c(14.5501, -19.0208, 15.4730, -8.2439, 43.3569,
                8.2275, 9.5018, 45.0382, 25.4858, 5.2913,
                34.8516, 27.5766, 10.4515, 8.4689, 133.7751, 23.8813,
                21.7453, 31.1656, 14.9955, 19.3744, 17.6791,
                25.7482, 4.4699, 15.2, 19.1451, 19.5033, 18.6435, 12.4578,
                -3.7492, 2.2137, 24.6032, 20.9070, 25.0136, 12.5674, 33.4299, 105.3188,
                21.8243, 47.5769, 20.1683, 14.3754, -3.4360, 28.3699,
                24.9668, -8.2245, 21.7453, 15.4730, NA)
)


country_dict <- country_dict %>%
  mutate(country_code_2 = case_when(
    country_code == "AUT" ~ "at",
    country_code == "ISL" ~ "is",
    country_code == "CZE" ~ "cz",
    country_code == "IRL" ~ "ie",
    country_code == "GEO" ~ "ge",
    country_code == "CHE" ~ "ch",
    country_code == "DNK" ~ "dk",
    country_code == "ARM" ~ "am",
    country_code == "BGR" ~ "bg",
    country_code == "NLD" ~ "nl",
    country_code == "ISR" ~ "il",
    country_code == "BLR" ~ "by",
    country_code == "DEU" ~ "de",
    country_code == "NOR" ~ "no",
    country_code == "AUS" ~ "au",
    country_code == "LTU" ~ "lt",
    country_code == "MKD" ~ "mk",
    country_code == "UKR" ~ "ua",
    country_code == "SVN" ~ "si",
    country_code == "MNE" ~ "me",
    country_code == "BIH" ~ "ba",
    country_code == "FIN" ~ "fi",
    country_code == "BEL" ~ "be",
    country_code == "HRV" ~ "hr",
    country_code == "POL" ~ "pl",
    country_code == "HUN" ~ "hu",
    country_code == "SWE" ~ "se",
    country_code == "SMR" ~ "sm",
    country_code == "ESP" ~ "es",
    country_code == "FRA" ~ "fr",
    country_code == "LVA" ~ "lv",
    country_code == "SRB" ~ "rs",
    country_code == "EST" ~ "ee",
    country_code == "ITA" ~ "it",
    country_code == "CYP" ~ "cy",
    country_code == "RUS" ~ "ru",
    country_code == "GRC" ~ "gr",
    country_code == "AZE" ~ "az",
    country_code == "ALB" ~ "al",
    country_code == "MLT" ~ "mt",
    country_code == "GBR" ~ "gb",
    country_code == "MDA" ~ "md",
    country_code == "ROU" ~ "ro",
    country_code == "PRT" ~ "pt",
    country_code == "MKD" ~ "mk",
    country_code == "CZE" ~ "cz",
    country_code == "ROW" ~ "rest_of_world",  # Keep for non-specific cases
    TRUE ~ NA_character_
  ))

country_dict <- country_dict %>%
  mutate(Flag = paste0('https://public.flourish.studio/country-flags/svg/', 
                       country_code_2, '.svg'))

write.csv(country_dict, "country_dict.csv", row.names = FALSE)

# Join dels resultats amb el dataset
all_results <- all_results %>%
  left_join(country_dict, by = "Source_country")

write.csv(all_results, "all_voting_flows_flags.csv", row.names = FALSE)

# Vots totals en tots els anys
total_votes <- all_results %>%
  group_by(Source_country, Contestant) %>%
  summarise(Total_points = sum(Points, na.rm = TRUE), .groups = "drop")

yearly_votes <- all_results %>%
  group_by(Source_country, Contestant, Year) %>%
  summarise(Yearly_points = sum(Points, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

yearly_votes <- yearly_votes %>%
  pivot_wider(names_from = Year, values_from = Yearly_points, values_fill = 0)

yearly_votes <- yearly_votes %>%
  mutate(Total_points = rowSums(select(., -Source_country, -Contestant)))
  # mutate(Frequency = rowSums(select(., -Source_country, -Contestant, -Total_points) != 0))

voting_rate <- yearly_votes %>%
  group_by(Contestant) %>%
  summarise(Sum_points = sum(Total_points, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

yearly_votes <- yearly_votes %>%
  left_join(voting_rate, by = "Contestant")

yearly_votes <- yearly_votes %>%
  mutate(rate = (Total_points / Sum_points)*100)

write.csv(yearly_votes, "years_voting_flows.csv", row.names = FALSE)

yearly_votes_top1 <- yearly_votes %>%
  arrange(Contestant, desc(rate)) %>%
  group_by(Contestant) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(rate = round(rate, 1))

write.csv(yearly_votes_top1, "yearly_votes_top1.csv", row.names = FALSE)



