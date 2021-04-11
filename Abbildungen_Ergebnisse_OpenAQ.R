library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(sf)
library(vctrs)
library(lubridate)

# brauche "fill_missing_dates"
source("./custom_functions.R")

##### Abbildungen "Ergebnisse" OpenAQ

# Zunächst lediglich einige Stationen pro Stadt solo plotten.
# Dann die gemittelten Stationswerte pro Stadt plotten -> Sollte wenn nicht ruhiger, so immerhin kontinuierlichere Datenreihe sein.

# -------------------------------------------------------------------------------------------------------------------

rand_paris1 <- read_csv("./OpenAQ/preprocessed_NO2_FR_4111.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

rand_paris2 <- read_csv("./OpenAQ/preprocessed_NO2_FR_4124.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

sf_paris <- st_read("./Geo/FRA.shp") %>%
	st_transform(3035)
mess_paris <- st_read("./Geo/Messstationen_Paris.gpkg") %>%
	st_transform(3035)

p1.1 <- ggplot() +
	geom_sf(data = sf_paris) +
	geom_sf(data = filter(mess_paris, locationId == 4111), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

p1 <- rand_paris1 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>%
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 100)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 4111*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(p1.1), 
					  xmin = as.POSIXct("2019 365", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 034", format = "%Y %j"),
					  ymin = 69,
					  ymax = 99)

p2.1 <- ggplot() +
	geom_sf(data = sf_paris) +
	geom_sf(data = filter(mess_paris, locationId == 4124), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

p2 <- rand_paris2 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>% 
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 100)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 4142*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(p2.1), 
					  xmin = as.POSIXct("2019 365", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 034", format = "%Y %j"),
					  ymin = 72,
					  ymax = 102)

p_complete <- p1 + p2 + plot_annotation(caption = "Daten: GADM & OpenAQ", tag_levels = "a")

ggsave("./Plots/Paris_einzeln.png", p_complete, 
	   width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)
# -------------------------------------------------------------------------------------------------------------------

rand_london1 <- read_csv("./OpenAQ/preprocessed_NO2_GB_157.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

rand_london2 <- read_csv("./OpenAQ/preprocessed_NO2_GB_141.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

sf_london <- st_read("./Geo/GBR.shp") %>%
	st_transform(3035)
mess_london <- st_read("./Geo/Messstationen_London.gpkg") %>%
	st_transform(3035)

lo1.1 <- ggplot() +
	geom_sf(data = sf_london) +
	geom_sf(data = filter(mess_london, locationId == 157), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

lo1 <- rand_london1 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>%
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 115)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 157*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(lo1.1), 
					  xmin = as.POSIXct("2019 365", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 034", format = "%Y %j"),
					  ymin = 92,
					  ymax = 117)

lo2.1 <- ggplot() +
	geom_sf(data = sf_london) +
	geom_sf(data = filter(mess_london, locationId == 141), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

lo2 <- rand_london2 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>% 
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 115)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 141*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(lo2.1), 
					  xmin = as.POSIXct("2019 365", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 034", format = "%Y %j"),
					  ymin = 92,
					  ymax = 117)

lo_complete <- lo1 + lo2 + plot_annotation(caption = "Daten: GADM & OpenAQ", tag_levels = "a")

ggsave("./Plots/London_einzeln.png", lo_complete, 
	   width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

# -------------------------------------------------------------------------------------------------------------------

rand_shenzhen1 <- read_csv("./OpenAQ/preprocessed_NO2_CN_9995.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

rand_shenzhen2 <- read_csv("./OpenAQ/preprocessed_NO2_CN_10020.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

sf_shenzhen <- st_read("./Geo/CHN.shp") %>%
	st_transform(2326)
mess_shenzhen <- st_read("./Geo/Messstationen_Shenzhen.gpkg") %>%
	st_transform(2326)

sh1.1 <- ggplot() +
	geom_sf(data = sf_shenzhen) +
	geom_sf(data = filter(mess_shenzhen, locationId == 9995), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

sh1 <- rand_shenzhen1 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>%
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 115)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 9995*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(sh1.1), 
					  xmin = as.POSIXct("2020 060", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 089", format = "%Y %j"),
					  ymin = 90,
					  ymax = 120)

sh2.1 <- ggplot() +
	geom_sf(data = sf_shenzhen) +
	geom_sf(data = filter(mess_shenzhen, locationId == 10020), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

sh2 <- rand_shenzhen2 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>% 
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 115)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 10020*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(sh2.1), 
					  xmin = as.POSIXct("2020 060", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 089", format = "%Y %j"),
					  ymin = 90,
					  ymax = 120)

sh_complete <- sh1 + sh2 + plot_annotation(caption = "Daten: GADM & OpenAQ", tag_levels = "a")

ggsave("./Plots/Shenzhen_einzeln.png", sh_complete, 
	   width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

# -------------------------------------------------------------------------------------------------------------------

rand_lima1 <- read_csv("./OpenAQ/preprocessed_NO2_PE_2302.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

rand_lima2 <- read_csv("./OpenAQ/preprocessed_NO2_PE_2277.csv", col_types = "iTicdcccii") %>%
  fill_missig_dates(column = "hour", "hour", 0)

sf_lima <- st_read("./Geo/PER.shp") %>%
	st_transform(5389)
mess_lima <- st_read("./Geo/Messstationen_Lima.gpkg") %>%
	st_transform(5389)

li1.1 <- ggplot() +
	geom_sf(data = sf_lima) +
	geom_sf(data = filter(mess_lima, locationId == 2302), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

li1 <- rand_lima1 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>%
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 80)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 2302*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(li1.1), 
					  xmin = as.POSIXct("2020 075", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 096", format = "%Y %j"),
					  ymin = 42,
					  ymax = 80)

li2.1 <- ggplot() +
	geom_sf(data = sf_lima) +
	geom_sf(data = filter(mess_lima, locationId == 2277), color = "blue") +
	coord_sf(expand = FALSE) +
	theme_minimal() +
	theme(axis.line = element_blank(),
		  axis.text = element_blank(),
		  panel.grid = element_blank())

li2 <- rand_lima2 %>%
	filter(hour >= as.POSIXct("2020-01-01 00:00:00"),
		   hour < as.POSIXct("2020-04-01 00:00:00")) %>% 
	ggplot() +
	geom_line(aes(x = hour, y = average), alpha = 0.9) +
	scale_x_datetime(date_labels = "%j", date_breaks = "2 weeks") +
	scale_y_continuous(limits = c(0, 80)) +
	labs(x = "DOY 2020",
		 y = "NO<sub>2</sub> in µg/m³",
		 caption = "*OpenAQ Stations-ID: 2277*") +
	theme_minimal() +
	theme(plot.caption = element_markdown(),
		  axis.title.y = element_markdown()) +
	annotation_custom(grob = ggplotGrob(li2.1), 
					  xmin = as.POSIXct("2020 075", format = "%Y %j"), 
					  xmax = as.POSIXct("2020 096", format = "%Y %j"),
					  ymin = 42,
					  ymax = 80)

li_complete <- li1 + li2 + plot_annotation(caption = "Daten: GADM & OpenAQ", tag_levels = "a")

ggsave("./Plots/Lima_einzeln.png", li_complete, 
	   width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

# -------------------------------------------------------------------------------------------------------------------
