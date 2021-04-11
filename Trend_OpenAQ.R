library(tidyverse)
library(vctrs)
library(lubridate)
library(hms)
library(sf)
library(ggtext)
library(patchwork)

source("./custom_functions.R")
source("./Neuauflage_MK_TS.R")

# Trendanalyse mit OpenAQ Daten
# Vorgehen wie bei Trendanalyse von Sentinel-5 P Daten

#-------------------------------------------------------------------------------------------------------------------------------------------

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
  filter(hour >= as.POSIXct("2020-01-01 00:00:00") & hour < as.POSIXct("2020-04-01 00:00:00"), !is.na(average))

complete_ground_stations_lima <- ground_files %>%
  filter(station_id %in% mess_lima$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00") & hour < as.POSIXct("2020-04-01 00:00:00"), !is.na(average))

complete_ground_stations_paris <- ground_files %>%
  filter(station_id %in% mess_paris$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00") & hour < as.POSIXct("2020-04-01 00:00:00"), !is.na(average))

complete_ground_stations_shenzhen <- ground_files %>%
  filter(station_id %in% mess_shenzhen$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0) %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00") & hour < as.POSIXct("2020-04-01 00:00:00"), !is.na(average))

#-------------------------------------------------------------------------------------------------------------------------------------------
# Shapiro-Wilk Test auf Normalverteilung
shapiro.test(complete_ground_stations_london$average)
shapiro.test(complete_ground_stations_paris$average)
shapiro.test(complete_ground_stations_lima$average)
shapiro.test(complete_ground_stations_shenzhen$average)

#-------------------------------------------------------------------------------------------------------------------------------------------

#### Paris
sen_test_paris <- TSTest(complete_ground_stations_paris$average, complete_ground_stations_paris$hour)
tfpw_paris <- adjust_ts(complete_ground_stations_paris$average, sen_test_paris$coeff)
mk_test_paris <- MKTest(tfpw_paris, complete_ground_stations_paris$hour[2:nrow(complete_ground_stations_paris)],
                        alternative = "less")

#### London
sen_test_london <- TSTest(complete_ground_stations_london$average, complete_ground_stations_london$hour)
tfpw_london <- adjust_ts(complete_ground_stations_london$average, sen_test_london$coeff)
mk_test_london <- MKTest(tfpw_london, complete_ground_stations_london$hour[2:nrow(complete_ground_stations_london)],
                         alternative = "less")

#### Lima
sen_test_lima <- TSTest(complete_ground_stations_lima$average, complete_ground_stations_lima$hour)
tfpw_lima <- adjust_ts(complete_ground_stations_lima$average, sen_test_lima$coeff)
mk_test_lima <- MKTest(tfpw_lima, complete_ground_stations_lima$hour[2:nrow(complete_ground_stations_lima)],
                       alternative = "less")

#### Shenzhen
sen_test_shenzhen <- TSTest(complete_ground_stations_shenzhen$average, complete_ground_stations_shenzhen$hour)
tfpw_shenzhen <- adjust_ts(complete_ground_stations_shenzhen$average, sen_test_shenzhen$coeff)
mk_test_shenzhen <- MKTest(tfpw_shenzhen, complete_ground_stations_shenzhen$hour[2:nrow(complete_ground_stations_shenzhen)],
                           alternative = "less")

#-------------------------------------------------------------------------------------------------------------------------------------------
# Autokorrelationen
acf(complete_ground_stations_paris$average, na.action = na.pass, main = "Autokorrelation Paris (OpenAQ)")
acf(complete_ground_stations_london$average, na.action = na.pass, main = "Autokorrelation London (OpenAQ)")
acf(complete_ground_stations_lima$average, na.action = na.pass, main = "Autokorrelation Lima (OpenAQ)")
acf(complete_ground_stations_shenzhen$average, na.action = na.pass, main = "Autokorrelation Shenzhen (OpenAQ)")
