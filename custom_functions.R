save_simple_time_series <- function(x, c , i, path) {
  plt <- x %>% 
    mutate(hour = ymd_hms(hour)) %>% {
      ggplot(.) +
        geom_line(aes(x = hour, y = average)) +
        scale_x_datetime(date_labels = "%Y %b") +
        labs(y = paste("NO2 in", unique(.$unit)),
             x = "Zeit",
             title = paste("Land:", c, "ID:", i),
             caption = paste("Abdeckung:", round(nrow(.) / 26298, 2) * 100, "%"))
    }
  
  ggsave(plot = plt, filename = paste0(path, "simpleTimeSeries_", c, "_", i, ".png"),
         device = "png", width = 26, height = 15, units = "cm", dpi = 420)
}

convert_gee_dates <- function(x, type) {
  if (type == "start") {
    date <- str_extract(x, "^.*(?=_)") %>%
      str_replace("T", " ") %>%
      ymd_hms()
  } else if (type == "end") {
    date <- str_extract(x, "(?<=_).*$") %>%
      str_replace("T", " ") %>%
      ymd_hms()
  } else {
    return(NULL)
  }
  return(date)
}

clean_up_dates <- function(x) {
  unique_dates <- unique(as.Date(x[["start_dt"]]))
  
  if (nrow(x) == length(unique_dates)) {
    return(x)
  } else {
    x$dt <- as.Date(x[["start_dt"]])
  }
  
  result <- tibble(ImageID = character(), NAME_2 = character(), mean = numeric(), start_dt = POSIXct())
  
  for (d in unique_dates) {
    
    idx <- which(x[["dt"]] == as.Date(d, origin = "1970-01-01"))
    
    if (length(idx) == 1) {
      result <- x %>%
        slice(idx) %>%
        select(-dt) %>%
        add_row(result, .)
    } else {
      result <- x %>%
        slice(idx) %>%
        select(-dt) %>%
        group_by(NAME_2) %>%
        # Aufnahmezeitpunkt wird auch gemittelt, weil ich nicht weiß, was ich ansonstenn machen soll.
        summarise(ImageID = first(ImageID), NAME_2 = first(NAME_2), mean = mean(mean), start_dt = median(start_dt)) %>%
        add_row(result, .)
    }
  }
  
  if (nrow(result) == length(unique_dates)) {
    result %>%
      arrange(start_dt) %>%
      return()
  } else {
    return(NULL)
  }
}

find_min_date <- function(x, y) {
  y_dates <- y[["hour"]]
  x_dates <- x[["start_dt"]]
  diff_vec <- map_dbl(x_dates, function(a, b) {
    # ich gebe 12 Stunden Puffer in beide Richtungen
    return(which.min(abs(difftime(a, b, units = "secs"))))
  }, b = y_dates)
  
  diff_vec <- ifelse(diff_vec <= 43200, diff_vec, NA)
  # Doppelungen vermeiden
  if (vec_duplicate_any(diff_vec)) {
    doubles <- vec_duplicate_id(diff_vec)
    diff_vec <- map_dbl(seq_along(diff_vec), ~ifelse(.x %in% doubles, diff_vec[.x], NA))
  }
  return(diff_vec)
}

join_no2_data <- function(x, y) {
  y_sub <- y %>%
    mutate(idx = 1:nrow(.)) %>%
    select(-any_of(c("displayName", "parameterId", "measurement_count")))
  x <- x %>%
    left_join(y_sub, by = c("nearest_ground_measurement" = "idx"))
  
  return(x)
}

combined_read <- function(x) {
  result <- map_dfr(x[["file_path"]], ~read_csv(.x, col_types = "iTdcdcccii"))
  return(result)
}

fill_missig_dates <- function(x, column = "start_dt", time_step = "day", diff_add = 12) {
  # dense_date_series disregards timestamp for now. Will be added later on
  if (time_step == "day") {
    date_fun <- as.Date
    
  } else if (time_step == "hour") {
    date_fun <- as.POSIXct
  }
  
  missing_dates <- seq(from = date_fun(min(x[[column]], na.rm = T)), to = date_fun(max(x[[column]], na.rm = T)), by = time_step)
  # + hours(x) für die GEE Zeitreihe, damit die neuen Zeilen eine Uhrzeit haben
  missing_dates <- missing_dates[!vec_in(missing_dates, date_fun(x[[column]]))] + hours(diff_add)
  x <- x %>%
    bind_rows(tibble("{column}" := missing_dates)) %>%
    arrange(.data[[column]])
}

count_na_sequence_length <- function(x) {
  vals <- x[["mean"]]
  idx <- vector("numeric")
  for (i in seq_along(vals)) {
    if (is.na(vals[i])) {
      idx <- append(idx, i)
    }
  }
  return(idx)
}

gee_rolling_avg <- function(x, columns, len = 7, fun = roll_meanr) {
  avg_temp <- apply(x[, c(columns)], 2, FUN = fun, n = len, na.rm = TRUE)
  # if (expr(fun))
  if (is.vector(avg_temp, mode = "numeric")) {
    if (length(avg_temp) != nrow(x)) {
      drop_seq <- c(seq(-1, ceiling(-len/2)), seq(-nrow(x), -nrow(avg_temp) + floor(-len/2)))
      x <- slice(x, drop_seq)
    }
  } else {
    if (nrow(avg_temp) != nrow(x)) {
      drop_seq <- c(seq(-1, ceiling(-len/2)), seq(-nrow(x), -nrow(avg_temp) + floor(-len/2)))
      x <- slice(x, drop_seq)
    }
  }
  
  for (i in seq_along(columns)) {
    x <- x %>%
      mutate("{columns[i]}" := avg_temp[, i])
  }
  return(x)
}
