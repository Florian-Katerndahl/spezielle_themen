library(tidyverse)
library(vctrs)
library(lubridate)
library(hms)
library(sf)
library(ggtext)
library(patchwork)
library(forcats)
library(RColorBrewer)

source("./custom_functions.R")
source("./Neuauflage_MK_TS.R")

# Datenprozessierung der aus der Google Earth Engine exportierten Sentinel-5 P Daten.
# Abbildungen der zeitlichen variabilität der Daten
# Trendanalyse

#-------------------------------------------------------------------------------------------------------------------------------------------
# brauche ich nur, damit die Daten auf GithUb hochgeladen werden können

if (FALSE) {
  walk(list("./GEE/London.csv", "./GEE/Lima.csv", "./GEE/Paris.csv", "./GEE/Milano.csv", "./GEE/Shenzhen.csv"),
       function(i) {
         d <- read_csv(i, col_types = "cccdc") %>%
           select(-"system:index", -".geo") %>%
           mutate(start_dt = convert_gee_dates(ImageID, "start")) %>%
           clean_up_dates() %>%
           fill_missig_dates()
         
         write_csv(d, i, append = FALSE)
       }
  )
}

#-------------------------------------------------------------------------------------------------------------------------------------------

london_gee <- read_csv("./GEE/London.csv", col_types = "ccdT") %>%
  mutate(record_month = as_factor(month(start_dt)),
         record_year = year(start_dt)) %>%
  filter(start_dt >= as.POSIXct("2019-01-01 00:00:00") & start_dt < as.POSIXct("2019-04-01 00:00:00") |
           start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"))

counts_london <- london_gee %>%
  filter(!is.na(mean)) %>%
  select(record_month, record_year) %>%
  group_by(record_month, record_year) %>%
  mutate(N = paste("n =", n()),
         y_pos = 0) %>%
  rename(x_pos = record_month) %>%
  unique()

paris_gee <- read_csv("./GEE/Paris.csv", col_types = "ccdT") %>%
  mutate(record_month = as_factor(month(start_dt)),
         record_year = year(start_dt)) %>%
  filter(start_dt >= as.POSIXct("2019-01-01 00:00:00") & start_dt < as.POSIXct("2019-04-01 00:00:00") |
           start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"))

counts_paris <- paris_gee %>%
  filter(!is.na(mean)) %>%
  select(record_month, record_year) %>%
  group_by(record_month, record_year) %>%
  mutate(N = paste("n =", n()),
         y_pos = 0) %>%
  rename(x_pos = record_month) %>%
  unique()

shenzhen_gee <- read_csv("./GEE/Shenzhen.csv", col_types = "ccdT") %>%
  mutate(record_month = as_factor(month(start_dt)),
         record_year = year(start_dt)) %>%
  filter(start_dt >= as.POSIXct("2019-01-01 00:00:00") & start_dt < as.POSIXct("2019-04-01 00:00:00") |
           start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"))

counts_shenzhen <- shenzhen_gee %>%
  filter(!is.na(mean)) %>%
  select(record_month, record_year) %>%
  group_by(record_month, record_year) %>%
  mutate(N = paste("n =", n()),
         y_pos = 0) %>%
  rename(x_pos = record_month) %>%
  unique()

lima_gee <- read_csv("./GEE/Lima.csv", col_types = "ccdT") %>%
  mutate(record_month = as_factor(month(start_dt)),
         record_year = year(start_dt)) %>%
  filter(start_dt >= as.POSIXct("2019-01-01 00:00:00") & start_dt < as.POSIXct("2019-04-01 00:00:00") |
           start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"))

counts_lima <- lima_gee %>%
  filter(!is.na(mean)) %>%
  select(record_month, record_year) %>%
  group_by(record_month, record_year) %>%
  mutate(N = paste("n =", n()),
         y_pos = 0) %>%
  rename(x_pos = record_month) %>%
  unique()

milano_gee <- read_csv("./GEE/Milano.csv", col_types = "ccdT") %>%
  mutate(record_month = as_factor(month(start_dt)),
         record_year = year(start_dt)) %>%
  filter(start_dt >= as.POSIXct("2019-01-01 00:00:00") & start_dt < as.POSIXct("2019-04-01 00:00:00") |
           start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"))

counts_milano <- milano_gee %>%
  filter(!is.na(mean)) %>%
  select(record_month, record_year) %>%
  group_by(record_month, record_year) %>%
  mutate(N = paste("n =", n()),
         y_pos = 0) %>%
  rename(x_pos = record_month) %>%
  unique()

#-------------------------------------------------------------------------------------------------------------------------------------------
# ordinäre Zeitreihen plotten

static_breaks <- c(as.POSIXct("2019 006", format = "%Y %j"), as.POSIXct("2019 020", format = "%Y %j"), 
                   as.POSIXct("2019 034", format = "%Y %j"), as.POSIXct("2019 048", format = "%Y %j"),
                   as.POSIXct("2019 062", format = "%Y %j"), as.POSIXct("2019 076", format = "%Y %j"),
                   as.POSIXct("2019 090", format = "%Y %j"), as.POSIXct("2020 006", format = "%Y %j"),
                   as.POSIXct("2020 020", format = "%Y %j"), as.POSIXct("2020 034", format = "%Y %j"),
                   as.POSIXct("2020 048", format = "%Y %j"), as.POSIXct("2020 062", format = "%Y %j"),
                   as.POSIXct("2020 076", format = "%Y %j"), as.POSIXct("2020 090", format = "%Y %j"))

static_labels <- rep(c("006", "020", "034", "048", "062", "076", "090"), times = 2)

paris_first_ts <- paris_gee %>% 
  ggplot() +
  geom_line(aes(x = start_dt, y = mean), alpha = 0.9) +
  geom_point(aes(x = start_dt, y = mean), alpha = 0.9) +
  scale_x_datetime(labels = static_labels, breaks = static_breaks) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  labs(title = "Paris",
       x = "DOY",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  facet_wrap(~record_year, ncol = 2, scales = "free_x", strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Paris_S5p_TS.png", plot = paris_first_ts, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

london_first_ts <- london_gee %>% 
  ggplot() +
  geom_line(aes(x = start_dt, y = mean), alpha = 0.9) +
  geom_point(aes(x = start_dt, y = mean), alpha = 0.9) +
  scale_x_datetime(labels = static_labels, breaks = static_breaks) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  labs(title = "London",
       x = "DOY",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  facet_wrap(~record_year, ncol = 2, scales = "free_x", strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/London_S5p_TS.png", plot = london_first_ts, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

lima_first_ts <- lima_gee  %>% 
  ggplot() +
  geom_line(aes(x = start_dt, y = mean), alpha = 0.9) +
  geom_point(aes(x = start_dt, y = mean), alpha = 0.9) +
  scale_x_datetime(labels = static_labels, breaks = static_breaks) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  labs(title = "Lima",
       x = "DOY",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  facet_wrap(~record_year, ncol = 2, scales = "free_x", strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/lima_S5p_TS.png", plot = lima_first_ts, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

shenzhen_first_ts <- shenzhen_gee %>% 
  ggplot() +
  geom_line(aes(x = start_dt, y = mean), alpha = 0.9) +
  geom_point(aes(x = start_dt, y = mean), alpha = 0.9) +
  scale_x_datetime(labels = static_labels, breaks = static_breaks) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  labs(title = "Shenzhen",
       x = "DOY",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  facet_wrap(~record_year, ncol = 2, scales = "free_x", strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Shenzhen_S5p_TS.png", plot = shenzhen_first_ts, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

milano_first_ts <- milano_gee %>% 
  ggplot() +
  geom_line(aes(x = start_dt, y = mean), alpha = 0.9) +
  geom_point(aes(x = start_dt, y = mean), alpha = 0.9) +
  scale_x_datetime(labels = static_labels, breaks = static_breaks) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  labs(title = "Mailand",
       x = "DOY",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  facet_wrap(~record_year, ncol = 2, scales = "free_x", strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Mailand_S5p_TS.png", plot = milano_first_ts, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

#-------------------------------------------------------------------------------------------------------------------------------------------
# Boxplots und Violin-/dichteplots
violin_paris <- paris_gee %>% 
  ggplot(aes(x = record_month, y = mean)) +
  geom_violin(aes(fill = record_month), width = 1, show.legend = FALSE, color = NA, scale = "area", alpha = 0.9) +
  geom_boxplot(width = 0.3, alpha = 0.4, outlier.alpha = 1) +
  geom_label(data = counts_paris, aes(x = x_pos, y = y_pos, label = N), label.size = NA, fontface = "italic", fill = NA) +
  labs(title = "Paris",
       x = "",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  scale_x_discrete(labels = c("Jan", "Feb", "März")) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  facet_wrap(~record_year, strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Violine_Paris.png", plot = violin_paris, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

violin_london <- london_gee %>% 
  ggplot(aes(x = record_month, y = mean)) +
  geom_violin(aes(fill = record_month), width = 1, show.legend = FALSE, color = NA, scale = "area", alpha = 0.9) +
  geom_boxplot(width = 0.3, alpha = 0.4, outlier.alpha = 1) +
  geom_label(data = counts_london, aes(x = x_pos, y = y_pos, label = N), label.size = NA, fontface = "italic", fill = NA) +
  labs(title = "London",
       x = "",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  scale_x_discrete(labels = c("Jan", "Feb", "März")) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  facet_wrap(~record_year, strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Violine_London.png", plot = violin_london, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

violin_lima <- lima_gee %>% 
  ggplot(aes(x = record_month, y = mean)) +
  geom_violin(aes(fill = record_month), width = 1, show.legend = FALSE, color = NA, scale = "area", alpha = 0.9) +
  geom_boxplot(width = 0.3, alpha = 0.4, outlier.alpha = 1) +
  geom_label(data = counts_lima, aes(x = x_pos, y = y_pos, label = N), label.size = NA, fontface = "italic", fill = NA) +
  labs(title = "Lima",
       x = "",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  scale_x_discrete(labels = c("Jan", "Feb", "März")) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  facet_wrap(~record_year, strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Violine_Lima.png", plot = violin_lima, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

violin_shenzhen <- shenzhen_gee %>% 
  ggplot(aes(x = record_month, y = mean)) +
  geom_violin(aes(fill = record_month), width = 1, show.legend = FALSE, color = NA, scale = "area", alpha = 0.9) +
  geom_boxplot(width = 0.3, alpha = 0.4, outlier.alpha = 1) +
  geom_label(data = counts_shenzhen, aes(x = x_pos, y = y_pos, label = N), label.size = NA, fontface = "italic", fill = NA) +
  labs(title = "Shenzhen",
       x = "",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  scale_x_discrete(labels = c("Jan", "Feb", "März")) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  facet_wrap(~record_year, strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Violine_Shenzhen.png", plot = violin_shenzhen, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

violin_milano <- milano_gee %>% 
  ggplot(aes(x = record_month, y = mean)) +
  geom_violin(aes(fill = record_month), width = 1, show.legend = FALSE, color = NA, scale = "area", alpha = 0.9) +
  geom_boxplot(width = 0.3, alpha = 0.4, outlier.alpha = 1) +
  geom_label(data = counts_milano, aes(x = x_pos, y = y_pos, label = N), label.size = NA, fontface = "italic", fill = NA) +
  labs(title = "Mailand",
       x = "",
       y = "NO<sub>2</sub> in molecules/cm²",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)") +
  scale_x_discrete(labels = c("Jan", "Feb", "März")) +
  scale_y_continuous(limits = c(0, 0.0007)) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  facet_wrap(~record_year, strip.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_markdown(),
        plot.caption = element_text(size = 8))

ggsave("./Plots/Violine_Mailand.png", plot = violin_milano, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

#-------------------------------------------------------------------------------------------------------------------------------------------
#  Dichteverteilung: Scheint irgendwas einer NVT zu folgen?

united_density <- bind_rows(london_gee, paris_gee, lima_gee, shenzhen_gee, milano_gee, .id = "group") %>%
  filter(start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00")) %>%
  mutate(group = str_replace_all(string = group, c("1" = "London", "2" = "Paris", "3" = "Lima", "4" = "Shenzhen", "5"  = "Mailand"))) %>%
  group_by(group) %>%
  mutate("shapiro_p" = shapiro.test(mean)$`p.value`,
         "shapiro" = shapiro.test(mean)$`statistic`,
         "n" = n()) %>%
  ungroup()

## Ergebnisse Shapiro Tests
shapiro.test(filter(united_density, group == "London")$mean)
shapiro.test(filter(united_density, group == "Paris")$mean)
shapiro.test(filter(united_density, group == "Lima")$mean)
shapiro.test(filter(united_density, group == "Shenzhen")$mean)
shapiro.test(filter(united_density, group == "Mailand")$mean)

## Abbildungen

united_density_plots <- united_density %>%
  ggplot() +
  geom_density(aes(mean, y = ..scaled.., color = group), key_glyph = "path", size = 1) +
  scale_color_manual(values = brewer.pal(5, "Dark2")) +
  labs(title = "Verteilungsdichte der NO<sub>2</sub> Werte",
       x = "NO<sub>2</sub> in molecules/cm²",
       y = "Dichte",
       caption = "Daten: ESA COPERNICUS Sentinel-5 P, vorprozessiert von der Google Earth Engine (GEE)<br>
       Dichtefunktionen skaliert") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_markdown(),
        plot.caption = element_markdown(),
        plot.title = element_markdown(size = 13))

ggsave("./Plots/Density_Plot.png", plot = united_density_plots, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)

united_qq <- united_density %>%
  ggplot(aes(sample = mean, group = group)) +
  geom_qq_line(show.legend = FALSE) +
  geom_qq(show.legend = FALSE, alpha = 0.5, shape = 16) +
  labs(x = "Theoretische Quantile",
       y = "NO<sub>2</sub> in molecules/cm²") +
  facet_wrap(~group, strip.position = "bottom") +
  theme_minimal() +
  theme(axis.title.y = element_markdown())

ggsave("./Plots/QQ_Plot.png", plot = united_qq, width = 250, height = 120, units = "mm", dpi = "retina", scale = 1.5)
#-------------------------------------------------------------------------------------------------------------------------------------------
# Mann-Kendall Test

#### Paris
filtered_paris <- paris_gee %>%
  filter(start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"), !is.na(mean))

sen_test_paris <- TSTest(Y = filtered_paris$mean, t = filtered_paris$start_dt, alpha_val = 0.05)
tfpw_paris <- adjust_ts(Y = filtered_paris$mean, coeff = sen_test_paris$coeff)
mk_test_paris <- MKTest(Y = tfpw_paris, t = filtered_paris$start_dt[2:nrow(filtered_paris)], alpha_val = 0.05, alternative = "less")

#### London
filtered_london <- london_gee %>%
  filter(start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"), !is.na(mean))

sen_test_london <- TSTest(filtered_london$mean, filtered_london$start_dt)
tfpw_london <- adjust_ts(filtered_london$mean, sen_test_london$coeff)
mk_test_london <- MKTest(tfpw_london, filtered_london$start_dt[2:nrow(filtered_london)], alternative = "less")

#### Lima
filtered_lima <- lima_gee %>%
  filter(start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"), !is.na(mean))

sen_test_lima <- TSTest(filtered_lima$mean, filtered_lima$start_dt)
tfpw_lima <- adjust_ts(filtered_lima$mean, sen_test_lima$coeff)
mk_test_lima <- MKTest(tfpw_lima, filtered_lima$start_dt[2:nrow(filtered_lima)], alternative = "less")

#### Mailand
filtered_milano <- milano_gee %>%
  filter(start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"), !is.na(mean))

sen_test_milano <- TSTest(filtered_milano$mean, filtered_milano$start_dt)
tfpw_milano <- adjust_ts(filtered_milano$mean, sen_test_milano$coeff)
mk_test_milano <- MKTest(tfpw_milano, filtered_milano$start_dt[2:nrow(filtered_milano)], alternative = "less")

#### Shenzhen
filtered_shenzhen <- shenzhen_gee %>%
  filter(start_dt >= as.POSIXct("2020-01-01 00:00:00") & start_dt < as.POSIXct("2020-04-01 00:00:00"), !is.na(mean))

sen_test_shenzhen <- TSTest(filtered_shenzhen$mean, filtered_shenzhen$start_dt)
tfpw_shenzhen <- adjust_ts(filtered_shenzhen$mean, sen_test_shenzhen$coeff)
mk_test_shenzhen <- MKTest(tfpw_shenzhen, filtered_shenzhen$start_dt[2:nrow(filtered_shenzhen)], alternative = "less")

#-------------------------------------------------------------------------------------------------------------------------------------------
# Autokorrelationen
acf(
  pull(
    filter(paris_gee, start_dt >= as.POSIXct("2020-01-01 00:00:00") & 
             start_dt < as.POSIXct("2020-04-01 00:00:00")
    )
    , mean), 
  na.action = na.pass, main = "Autokorrelation Paris (S-5P)")
acf(
  pull(
    filter(london_gee, start_dt >= as.POSIXct("2020-01-01 00:00:00") & 
             start_dt < as.POSIXct("2020-04-01 00:00:00")
    )
    , mean), 
  na.action = na.pass, main = "Autokorrelation London (S-5P)")
acf(
  pull(
    filter(lima_gee, start_dt >= as.POSIXct("2020-01-01 00:00:00") & 
             start_dt < as.POSIXct("2020-04-01 00:00:00")
    )
    , mean), 
  na.action = na.pass, main = "Autokorrelation Lima (S-5P)")
acf(
  pull(
    filter(shenzhen_gee, start_dt >= as.POSIXct("2020-01-01 00:00:00") & 
             start_dt < as.POSIXct("2020-04-01 00:00:00")
    )
    , mean), 
  na.action = na.pass, main = "Autokorrelation Shenzhen (S-5P)")
acf(
  pull(
    filter(shenzhen_gee, start_dt >= as.POSIXct("2020-01-01 00:00:00") & 
             start_dt < as.POSIXct("2020-04-01 00:00:00")
    )
    , mean), 
  na.action = na.pass, main = "Autokorrelation Mailand (S-5P)")
