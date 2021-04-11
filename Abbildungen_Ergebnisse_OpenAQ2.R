library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(sf)
library(lubridate)
library(vctrs)
library(purrr)
library(stringr)

# brauche "fill_missing_dates"
source("./custom_functions.R")

# Hier die entsprechenden Abbildungen der von den Bodenstationen gemessenen NO2-Konzentrationen - mit allen Stationen pro Stadt
# gemittelt.

#------------------------------------------------------------------------------------------------------------------------------------------

london_geo <- st_read("./Geo/GBR.shp") %>%
  st_transform(3035)

paris_geo <- st_read("./Geo/FRA.shp") %>%
  st_transform(3035)

lima_geo <- st_read("./Geo/PER.shp") %>%
  st_transform(5389)

shenzhen_geo <- st_read("./Geo/CHN.shp") %>%
  st_transform(2326)

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
  fill_missig_dates(column = "hour", "hour", 0)

complete_ground_stations_lima <- ground_files %>%
  filter(station_id %in% mess_lima$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0)

complete_ground_stations_paris <- ground_files %>%
  filter(station_id %in% mess_paris$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0)

complete_ground_stations_shenzhen <- ground_files %>%
  filter(station_id %in% mess_shenzhen$locationId) %>%
  combined_read() %>%
  mutate(average = ifelse(average >= 0, average, NA)) %>%
  group_by(hour) %>%
  summarise(average = mean(average)) %>%
  fill_missig_dates(column = "hour", "hour", 0)

#------------------------------------------------------------------------------------------------------------------------------------------

p1.1 <- ggplot() +
  geom_sf(data = paris_geo) +
  geom_sf(data = mess_paris, color = "blue") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

p1 <- complete_ground_stations_paris %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
         hour < as.POSIXct("2020-04-01 00:00:00")) %>%
  ggplot() +
  geom_line(aes(x = hour, y = average), alpha = 0.9) +
  scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "DOY 2020",
       y = "NO<sub>2</sub> in µg/m³",
       title = "Paris") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown()) +
  annotation_custom(grob = ggplotGrob(p1.1), 
                    xmin = as.POSIXct("2019 360", format = "%Y %j"), 
                    xmax = as.POSIXct("2020 030", format = "%Y %j"),
                    ymin = 76,
                    ymax = 100)

lo1.1 <- ggplot() +
  geom_sf(data = london_geo) +
  geom_sf(data = mess_london, color = "blue") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

lo1 <- complete_ground_stations_london %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
         hour < as.POSIXct("2020-04-01 00:00:00")) %>%
  ggplot() +
  geom_line(aes(x = hour, y = average), alpha = 0.9) +
  scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "DOY 2020",
       y = "NO<sub>2</sub> in µg/m³",
       title = "London") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown()) +
  annotation_custom(grob = ggplotGrob(lo1.1), 
                    xmin = as.POSIXct("2019 360", format = "%Y %j"), 
                    xmax = as.POSIXct("2020 030", format = "%Y %j"),
                    ymin = 76,
                    ymax = 100)

li1.1 <- ggplot() +
  geom_sf(data = lima_geo) +
  geom_sf(data = mess_lima, color = "blue") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

li1 <- complete_ground_stations_lima %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
         hour < as.POSIXct("2020-04-01 00:00:00")) %>%
  ggplot() +
  geom_line(aes(x = hour, y = average), alpha = 0.9) +
  scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "DOY 2020",
       y = "NO<sub>2</sub> in µg/m³",
       title = "Lima") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown()) +
  annotation_custom(grob = ggplotGrob(li1.1), 
                    xmin = as.POSIXct("2019 360", format = "%Y %j"), 
                    xmax = as.POSIXct("2020 030", format = "%Y %j"),
                    ymin = 65,
                    ymax = 100)

sh1.1 <- ggplot() +
  geom_sf(data = shenzhen_geo) +
  geom_sf(data = mess_shenzhen, color = "blue") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

sh1 <- complete_ground_stations_shenzhen %>%
  filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
         hour < as.POSIXct("2020-04-01 00:00:00")) %>%
  ggplot() +
  geom_line(aes(x = hour, y = average), alpha = 0.9) +
  scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "DOY 2020",
       y = "NO<sub>2</sub> in µg/m³",
       title = "Shenzhen") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown()) +
  annotation_custom(grob = ggplotGrob(sh1.1), 
                    xmin = as.POSIXct("2020 055", format = "%Y %j"), 
                    xmax = as.POSIXct("2020 090", format = "%Y %j"),
                    ymin = 76,
                    ymax = 100)

total <- (p1 + lo1) / (li1 + sh1) + plot_annotation(caption = "Daten: GADM & OpenAQ", tag_levels = "a")

ggsave(filename = "./Plots/OpenAQ_gesamt.png", plot = total,
       width = 300, height = 240, units = "mm", dpi = "retina", scale = 1.5)
