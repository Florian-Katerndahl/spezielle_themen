library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)
library(hms)

# source("./custom_functions.R")

# API ansprechen für OpenAQ Daten
# Stationen wurden in früheren Skripten ermittelt

#-------------------------------------------------------------------------------------------------------------------------------------------

unique_stations <- c(60, 141, 146, 147, 148, 150, 152, 153, 154, 155, 157, 159, 2395, 2489, 2560, 151, 2541, 4082, 
					 4088, 4142, 4153, 4156, 4162, 4165, 4079, 4123, 4060, 4041, 4146, 5681, 4043, 4067, 4084, 4093, 
					 4094, 4096, 4111, 4116, 4120, 4121, 4124, 4127, 4138, 4154, 4115, 4039, 4053, 4058, 4063, 4066, 
					 4069, 4070, 4075, 4086, 4099, 2681, 8022, 8038, 8128, 8165, 8242, 8257, 8258, 8273, 8278, 8279, 
					 8283, 8296, 8301, 8303, 8309, 8313, 8321, 8331, 8333, 8336, 8338, 8340, 8347, 8348, 8350, 8352, 
					 8356, 8357, 8358, 8361, 8362, 8376, 8406, 8007, 8166, 8243, 8274, 8310, 8364, 8075, 8276, 8163, 
					 9073, 9215, 9508, 9534, 9556, 9616, 10007, 10020, 10067, 8987, 9746, 9995, 2289, 2302, 2397, 2402, 
					 7464, 2277, 2415, 819, 2294, 2403, 2351, 971, 974, 1496, 857, 1122)

mapped_countries <- data.frame(
	locationId = c(60, 141, 146, 147, 148, 150, 152, 
				   153, 154, 155, 157, 159, 2395, 2489, 2560, 151, 2541, 4082, 4088, 
				   4142, 4153, 4156, 4162, 4165, 4079, 4123, 4060, 4041, 4146, 5681, 
				   4043, 4067, 4084, 4093, 4094, 4096, 4111, 4116, 4120, 4121, 4124, 
				   4127, 4138, 4154, 4115, 4039, 4053, 4058, 4063, 4066, 4069, 4070, 
				   4075, 4086, 4099, 2681, 8022, 8038, 8128, 8165, 8242, 8257, 8258, 
				   8273, 8278, 8279, 8283, 8296, 8301, 8303, 8309, 8313, 8321, 8331, 
				   8333, 8336, 8338, 8340, 8347, 8348, 8350, 8352, 8356, 8357, 8358, 
				   8361, 8362, 8376, 8406, 8007, 8166, 8243, 8274, 8310, 8364, 8075, 
				   8276, 8163, 9073, 9215, 9508, 9534, 9556, 9616, 10007, 10020, 
				   10067, 8987, 9746, 9995, 2289, 2302, 2397, 2402, 7464, 2277, 
				   2415, 819, 2294, 2403, 2351, 971, 974, 1496, 857, 1122),
	country = c("GB","GB", "GB", "GB", "GB", "GB", "GB", "GB", "GB", "GB", "GB", "GB", 
				"GB", "GB", "GB", "GB", "GB", "FR", "FR", "FR", "FR", "FR", "FR", 
				"FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", 
				"FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", 
				"FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", 
				"IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", 
				"IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", 
				"IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", 
				"IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "CN", "CN", 
				"CN", "CN", "CN", "CN", "CN", "CN", "CN", "CN", "CN", "CN", "PE", 
				"PE", "PE", "PE", "PE", "PE", "PE", "PE", "PE", "PE", "PE", "US", 
				"US", "US", "US", "US")
	)

start_date <- ymd_hms("2017-12-01 00:00:00")
end_date <- ymd_hms("2020-12-01 00:00:00")
datetime_ranges <- tibble(
	begin = seq(start_date, end_date, by = "month"),
	end = lead(begin, default = end_date + months(1)),
	api_start = strftime(begin, "%Y-%m-%dT00:00:00+00:00"),
	api_end = strftime(end, "%Y-%m-%dT00:00:00+00:00")
)

#-------------------------------------------------------------------------------------------------------------------------------------------

base_url <- "https://u50g7n0cbj.execute-api.us-east-1.amazonaws.com/v2/averages"

idx <- 1
chunksize <- 100000L
times_rejected <- 0
while (idx <= length(unique_stations)) {
	# new data frame for each station
	# name == ID, subtitle == Name
	measurements <- tibble(
		id = integer(), hour = character(), name = character(), unit = character(), average = numeric(),
		subtitle = character(), parameter = character(), displayName = character(), parameterId = numeric(),
		measurement_count = numeric()
	)
	t <- 1
	
	while (t <= nrow(datetime_ranges)) {
		# how many observations are there in the specified time frame?
		initial_request <- GET(base_url, query = list(
			date_from = datetime_ranges[t, "api_start"],
			date_to = datetime_ranges[t, "api_end"],
			parameter = "no2",
			limit = 1,
			page = 1,
			sort = "desc",
			spatial = "location",
			temporal = "hour",
			location = unique_stations[idx],
			group = "false"
		))
		
		if (initial_request$status_code != 200) {
			if (times_rejected <= 1) {
				Sys.sleep(60)
				times_rejected <- times_rejected + 1
				next
			} else if (times_rejected == 2) {
				Sys.sleep(180)
				times_rejected <- times_rejected + 1
				next
			} else if (times_rejected > 2) {
				# break holt mich nur aus dem innersten Loop raus, also muss ich mit Fehlern arbeiten!?
				stop("IP got blacklisted")
			}
		} else {
			if (initial_request$status_code == 200) {
				N <- initial_request %>%
					content(as = "text", encoding = "UTF-8") %>%
					fromJSON() %>%
					.[["meta"]] %>%
					as_tibble() %>%
					pull(found)
				
				pages <- ceiling(N / chunksize)
				api_page <- 1
				while (api_page <= pages) {
					# hier muss eigentlich auch nochmal ein Status Code check hin...
					request <- GET(base_url, query = list(
						date_from = datetime_ranges[t, "api_start"],
						date_to = datetime_ranges[t, "api_end"],
						parameter = "no2",
						limit = chunksize,
						page = api_page,
						sort = "desc",
						spatial = "location",
						temporal = "hour",
						location = unique_stations[idx],
						group = "false"
					))
					
					if (request$status_code != 200) {
						if (times_rejected <= 1) {
							Sys.sleep(60)
							times_rejected <- times_rejected + 1
							next
						} else if (times_rejected == 2) {
							Sys.sleep(180)
							times_rejected <- times_rejected + 1
							next
						} else if (times_rejected > 2) {
							# break holt mich nur aus dem innersten Loop raus, also muss ich mit Fehlern arbeiten!?
							stop("IP got blacklisted")
						}
					} else if (request$status_code == 200) {
						
						# continue normally
						measurements <- as_tibble(fromJSON(content(request, "text", encoding = "UTF-8"))[["results"]]) %>%
							bind_rows(measurements, .)
						api_page <- api_page + 1
					}
				}
			} 
		}
		
		t <- t + 1
		Sys.sleep(4)
	}
	
	# Mein Preprocessing mit Datumsformatierung und so'n Shit
	measurements <- measurements %>%
		mutate(hour = str_replace(hour, "(?<=[0-9]{2})T(?=[0-9]{2})", " ")) %>%
		# bisschen ineffektiv, aber was solls
		mutate(hour = as.POSIXct(str_extract(hour, ".*(?=[+-][0-9]{2}:[0-9]{2})"),
								 format = "%Y-%m-%d %H:%M:%S")) %>%
		arrange(hour) %>%
		mutate(hour = strftime(hour, "%Y-%m-%d %H:%M:%S")) %>%
		unique()
	
	countr <- mapped_countries[mapped_countries$locationId == unique_stations[idx], "country"]
	
	write_csv(measurements, file = paste0(
		"./OpenAQ/preprocessed_NO2_",
		countr,
		"_",
		unique_stations[idx],
		".csv"
	))
	
	idx <- idx + 1
}
