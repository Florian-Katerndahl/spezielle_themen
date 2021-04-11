library(tidyverse)
library(vctrs)
library(lubridate)
library(hms)
library(sf)
library(ggtext)
library(patchwork)
library(latex2exp)
library(forcats)
library(RColorBrewer)

source("./custom_functions.R")
source("./Neuauflage_MK_TS.R")

#---------------------------------------------------------------------------------------------------------------------------------------------
# OpenAq einlesen
mess_london <- st_read("./Geo/Messstationen_London.gpkg") %>%
  st_transform(3035)

mess_paris <- st_read("./Geo/Messstationen_Paris.gpkg") %>%
  st_transform(3035)

mess_lima <- st_read("./Geo/Messstationen_Lima.gpkg") %>%
  st_transform(5389)

mess_shenzhen <- st_read("./Geo/Messstationen_Shenzhen.gpkg") %>%
  st_transform(2326)

ground_files <- list.files("./OpenAQ", "*.csv", full.names = TRUE) %>%
  tibble(file_path = .,
         station_id = str_extract(file_path, "(?<=_)[0-9]{3,}(?=\\.csv)"))

complete_ground_stations_london <- ground_files %>%
  filter(station_id %in% mess_london$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(!is.na(average))

complete_ground_stations_lima <- ground_files %>%
  filter(station_id %in% mess_lima$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(!is.na(average))

complete_ground_stations_paris <- ground_files %>%
  filter(station_id %in% mess_paris$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(!is.na(average))

complete_ground_stations_shenzhen <- ground_files %>%
  filter(station_id %in% mess_shenzhen$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(!is.na(average))

#---------------------------------------------------------------------------------------------------------------------------------------------
# Sentinel-5 P Data
london_gee <- read_csv("./GEE/London.csv", col_types = "ccdT") %>%
  filter(!is.na(mean))

paris_gee <- read_csv("./GEE/Paris.csv", col_types = "ccdT") %>%
  filter(!is.na(mean))

shenzhen_gee <- read_csv("./GEE/Shenzhen.csv", col_types = "ccdT") %>%
  filter(!is.na(mean))

lima_gee <- read_csv("./GEE/Lima.csv", col_types = "ccdT") %>%
  filter(!is.na(mean))

#---------------------------------------------------------------------------------------------------------------------------------------------
# join datasets
e <- environment()

pwalk(list(
  i = list(lima_gee, london_gee, paris_gee, shenzhen_gee), 
  j = list(complete_ground_stations_lima, complete_ground_stations_london, 
           complete_ground_stations_paris, complete_ground_stations_shenzhen),
  n = list("lima_gee", "london_gee", "paris_gee", "shenzhen_gee")),
  function(i, j, n) {
    i$nearest_ground_measurement <- find_min_date(i, j)
    i <- i %>%
      join_no2_data(j) %>%
      filter(!is.na(mean) & !is.na(average))
    assign(n, i, envir = e)
  }
)

#---------------------------------------------------------------------------------------------------------------------------------------------
# again shapiro-wil test for normality
shapiro.test(lima_gee$mean)
shapiro.test(lima_gee$average)

shapiro.test(london_gee$mean)
shapiro.test(london_gee$average)

shapiro.test(paris_gee$mean)
shapiro.test(paris_gee$average)

shapiro.test(shenzhen_gee$mean)
shapiro.test(shenzhen_gee$average)

#---------------------------------------------------------------------------------------------------------------------------------------------
# MK Test
mk_test_paris <- MKTest(paris_gee$mean, paris_gee$average)

mk_test_london <- MKTest(london_gee$mean, london_gee$average)

mk_test_lima <- MKTest(lima_gee$mean, lima_gee$average)

mk_test_shenzhen <- MKTest(shenzhen_gee$mean, shenzhen_gee$average)

#---------------------------------------------------------------------------------------------------------------------------------------------
# Sen-Steigung und "Sen Y-Achsenabschnitt"; für Abbildungen

sen_slope_paris <- TSSlope(paris_gee$mean, paris_gee$average)

sen_intercept_paris <- median(paris_gee$mean - TSScore(paris_gee$mean, sen_slope_paris, paris_gee$average)$coeff)

sen_slope_london <- TSSlope(london_gee$mean, london_gee$average)

sen_intercept_london <- median(london_gee$mean - TSScore(london_gee$mean, sen_slope_london, london_gee$average)$coeff)

sen_slope_lima <- TSSlope(lima_gee$mean, lima_gee$average)

sen_intercept_lima <- median(lima_gee$mean - TSScore(lima_gee$mean, sen_slope_lima, lima_gee$average)$coeff)

sen_slope_shenzhen <- TSSlope(shenzhen_gee$mean, shenzhen_gee$average)

sen_intercept_shenzhen <- median(shenzhen_gee$mean - TSScore(shenzhen_gee$mean, sen_slope_shenzhen, shenzhen_gee$average)$coeff)

#---------------------------------------------------------------------------------------------------------------------------------------------
# Abbildungen
concordance_paris <- ggplot(paris_gee) +
  geom_point(aes(x = average, y = mean), alpha = 0.7, shape = 16) +
  geom_abline(slope = sen_slope_paris, intercept = sen_intercept_paris, color = "#D95F02", size = 1) +
  labs(title = "Paris",
       subtitle = TeX("$\\tau = 0,44$"),
       x = "NO<sub>2</sub> in µg/m³",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE) & OpenAQ") +
  scale_y_continuous(labels = scales::scientific) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_markdown(size = 8))

ggsave("./Plots/concor_paris.png", concordance_paris,
       width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

concordance_london <- ggplot(london_gee) +
  geom_point(aes(x = average, y = mean), alpha = 0.7, shape = 16) +
  geom_abline(slope = sen_slope_london, intercept = sen_intercept_london, color = "#D95F02", size = 1) +
  labs(title = "London",
       subtitle = TeX("$\\tau = 0,41$"),
       x = "NO<sub>2</sub> in µg/m³",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE) & OpenAQ") +
  scale_y_continuous(labels = scales::scientific) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_markdown(size = 8))

ggsave("./Plots/concor_london.png", concordance_london,
       width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

concordance_lima <- ggplot(lima_gee) +
  geom_point(aes(x = average, y = mean), alpha = 0.7, shape = 16) +
  geom_abline(slope = sen_slope_lima, intercept = sen_intercept_lima, color = "#D95F02", size = 1) +
  labs(title = "Lima",
       subtitle = TeX("$\\tau = 0,25$"),
       x = "NO<sub>2</sub> in µg/m³",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE) & OpenAQ") +
  scale_y_continuous(labels = scales::scientific) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_markdown(size = 8))

ggsave("./Plots/concor_lima.png", concordance_lima,
       width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

concordance_shenzhen <- ggplot(shenzhen_gee) +
  geom_point(aes(x = average, y = mean), alpha = 0.7, shape = 16) +
  geom_abline(slope = sen_slope_shenzhen, intercept = sen_intercept_shenzhen, color = "#D95F02", size = 1) +
  labs(title = "Shenzhen",
       subtitle = TeX("$\\tau = 0,38$"),
       x = "NO<sub>2</sub> in µg/m³",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE) & OpenAQ") +
  scale_y_continuous(labels = scales::scientific) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_markdown(size = 8))

ggsave("./Plots/concor_shenzhen.png", concordance_shenzhen,
       width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)
