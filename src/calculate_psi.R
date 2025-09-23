# Самодостаточная копия PSI функции

calculate_psi <- function(df, target_col, date_col, n_intervals = 6, min_bucket_size = 50) {
  if (!is.data.frame(df)) stop("df должен быть датафреймом")
  if (!target_col %in% names(df)) stop(paste("Столбец", target_col, "не найден в датафрейме"))
  if (!date_col %in% names(df)) stop(paste("Столбец", date_col, "не найден в датафрейме"))

  df[[date_col]] <- as.Date(df[[date_col]])
  df <- df[!is.na(df[[date_col]]) & !is.na(df[[target_col]]), ]

  unique_dates <- sort(unique(df[[date_col]]))
  n_unique_dates <- length(unique_dates)

  if (n_unique_dates <= n_intervals) {
    interval_breaks <- unique_dates
    actual_intervals <- n_unique_dates
    df$time_interval <- factor(df[[date_col]], levels = unique_dates, labels = paste0("Interval_", 1:n_unique_dates))
    interval_info <- data.frame(
      interval = paste0("Interval_", 1:n_unique_dates),
      start_date = unique_dates,
      end_date = unique_dates,
      days = rep(0, n_unique_dates)
    )
    n_intervals <- actual_intervals
  } else {
    date_range <- range(df[[date_col]], na.rm = TRUE)
    total_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
    interval_size <- total_days / n_intervals
    interval_breaks <- seq(date_range[1], date_range[2], by = interval_size)
    if (length(interval_breaks) <= n_intervals) interval_breaks <- c(interval_breaks, date_range[2])
    df$time_interval <- cut(df[[date_col]], breaks = interval_breaks, labels = paste0("Interval_", 1:n_intervals), include.lowest = TRUE)
    interval_info <- data.frame(
      interval = levels(df$time_interval),
      start_date = interval_breaks[1:n_intervals],
      end_date = interval_breaks[2:(n_intervals+1)],
      days = as.numeric(difftime(interval_breaks[2:(n_intervals+1)], interval_breaks[1:n_intervals], units = "days"))
    )
  }

  exclude_cols <- c(target_col, date_col, "time_interval")
  feature_cols <- setdiff(names(df), exclude_cols)
  for (col in feature_cols) {
    if (!is.numeric(df[[col]])) stop(paste("Столбец", col, "должен содержать числовые WOE значения"))
  }

  calculate_psi_between <- function(dist1, dist2) {
    dist1_norm <- dist1 / sum(dist1, na.rm = TRUE)
    dist2_norm <- dist2 / sum(dist2, na.rm = TRUE)
    epsilon <- 1e-10
    dist1_norm <- (dist1_norm + epsilon) / sum(dist1_norm + epsilon)
    dist2_norm <- (dist2_norm + epsilon) / sum(dist2_norm + epsilon)
    sum((dist1_norm - dist2_norm) * log(dist1_norm / dist2_norm), na.rm = TRUE)
  }

  results <- data.frame(variable = feature_cols, stringsAsFactors = FALSE)
  for (i in 1:n_intervals) results[[paste0("PSI_Interval_", i)]] <- 0

  for (var_idx in seq_along(feature_cols)) {
    var_name <- feature_cols[var_idx]
    var_data <- df[[var_name]]
    woe_by_interval <- vector("list", n_intervals)
    for (i in 1:n_intervals) {
      interval_name <- paste0("Interval_", i)
      interval_mask <- df$time_interval == interval_name
      if (sum(interval_mask) >= min_bucket_size) {
        woe_by_interval[[i]] <- mean(var_data[interval_mask], na.rm = TRUE)
      } else {
        woe_by_interval[[i]] <- NA
      }
    }
    for (i in 2:n_intervals) {
      if (!is.na(woe_by_interval[[i-1]]) && !is.na(woe_by_interval[[i]])) {
        dist1 <- c(woe_by_interval[[i-1]], 1 - abs(woe_by_interval[[i-1]]))
        dist2 <- c(woe_by_interval[[i]], 1 - abs(woe_by_interval[[i]]))
        results[var_idx, paste0("PSI_Interval_", i)] <- calculate_psi_between(dist1, dist2)
      }
    }
  }

  psi_cols <- grep("^PSI_", names(results), value = TRUE)
  results[psi_cols] <- round(results[psi_cols], 4)
  attr(results, "interval_info") <- interval_info
  results
}



