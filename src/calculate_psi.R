# Функция для расчета PSI (Population Stability Index) для кредитного скоринга
# Параметры:
# df - датафрейм с данными, где переменные уже содержат WOE значения
# target_col - название столбца с целевой переменной
# date_col - название столбца с датой
# n_intervals - количество временных интервалов (по умолчанию 6)
# min_bucket_size - минимальный размер бакета для анализа (по умолчанию 1)
# baseline_interval ("first"/"last"/номер/"Interval_k")

calculate_psi <- function(
    df,
    target_col,
    date_col,
    n_intervals = 6,
    min_bucket_size = 1,
    baseline_interval = "first",
    interval_method = c("quantile", "equal_days")
) {
  
  # Проверка входных параметров
  if (!is.data.frame(df)) {
    stop("df должен быть датафреймом")
  }
  
  if (!target_col %in% names(df)) {
    stop(paste("Столбец", target_col, "не найден в датафрейме"))
  }
  
  if (!date_col %in% names(df)) {
    stop(paste("Столбец", date_col, "не найден в датафрейме"))
  }
  
  # Преобразование даты в Date формат
  df[[date_col]] <- as.Date(df[[date_col]])
  
  # Удаление строк с NA в дате или таргете
  df <- df[!is.na(df[[date_col]]) & !is.na(df[[target_col]]), ]
  
  # Определение временных интервалов
  unique_dates <- sort(unique(df[[date_col]]))
  n_unique_dates <- length(unique_dates)
  
  # Если количество уникальных дат меньше или равно количеству интервалов, используем уникальные даты
  if (n_unique_dates <= n_intervals) {
    # Используем уникальные даты как интервалы
    interval_breaks <- unique_dates
    actual_intervals <- n_unique_dates
    
    # Создание временных интервалов на основе уникальных дат
    df$time_interval <- factor(df[[date_col]], levels = unique_dates, labels = paste0("Interval_", 1:n_unique_dates))
    
    # Информация об интервалах для уникальных дат
    interval_info <- data.frame(
      interval = paste0("Interval_", 1:n_unique_dates),
      start_date = unique_dates,
      end_date = unique_dates,
      days = rep(0, n_unique_dates)
    )
    
    # Обновляем количество интервалов для дальнейших вычислений
    n_intervals <- actual_intervals
    
  } else {
    interval_method <- match.arg(interval_method)
    if (identical(interval_method, "quantile")) {
      dnum <- as.numeric(df[[date_col]])
      br <- unique(stats::quantile(dnum, probs = seq(0, 1, length.out = n_intervals + 1), na.rm = TRUE, type = 7))
      if (length(br) > 2) {
        df$time_interval <- cut(dnum, breaks = br, labels = paste0("Interval_", seq_len(length(br) - 1)), include.lowest = TRUE, right = TRUE)
        interval_info <- data.frame(
          interval = levels(df$time_interval),
          start_date = as.Date(br[1:(length(br) - 1)], origin = "1970-01-01"),
          end_date   = as.Date(br[2:length(br)], origin = "1970-01-01"),
          days = as.numeric(as.Date(br[2:length(br)], origin = "1970-01-01") - as.Date(br[1:(length(br) - 1)], origin = "1970-01-01"))
        )
        n_intervals <- nlevels(df$time_interval)
      } else {
        interval_method <- "equal_days"
      }
    }
    if (identical(interval_method, "equal_days")) {
      date_range <- range(df[[date_col]], na.rm = TRUE)
      total_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
      interval_size <- ceiling(total_days / n_intervals)
      interval_breaks <- seq(date_range[1], date_range[2], by = interval_size)
      if (length(interval_breaks) <= n_intervals) interval_breaks <- c(interval_breaks, date_range[2])
      df$time_interval <- cut(df[[date_col]], breaks = interval_breaks, labels = paste0("Interval_", 1:(length(interval_breaks) - 1)), include.lowest = TRUE, right = TRUE)
      interval_info <- data.frame(
        interval = levels(df$time_interval),
        start_date = interval_breaks[1:(length(interval_breaks) - 1)],
        end_date   = interval_breaks[2:length(interval_breaks)],
        days = as.numeric(difftime(interval_breaks[2:length(interval_breaks)], interval_breaks[1:(length(interval_breaks) - 1)], units = "days"))
      )
      n_intervals <- nlevels(df$time_interval)
    }
  }
  
  # Получение списка переменных для анализа (исключая дату, таргет и созданный интервал)
  exclude_cols <- c(target_col, date_col, "time_interval")
  feature_cols <- setdiff(names(df), exclude_cols)
  
  # Проверяем, что все переменные содержат WOE значения
  for (col in feature_cols) {
    if (!is.numeric(df[[col]])) {
      stop(paste("Столбец", col, "должен содержать числовые WOE значения"))
    }
  }
  
  # Функция для расчета PSI между двумя распределениями
  calculate_psi_between <- function(dist1, dist2) {
    # Нормализация распределений
    dist1_norm <- dist1 / sum(dist1, na.rm = TRUE)
    dist2_norm <- dist2 / sum(dist2, na.rm = TRUE)
    
    # Добавление небольшого значения для избежания log(0)
    epsilon <- 1e-10
    dist1_norm <- dist1_norm + epsilon
    dist2_norm <- dist2_norm + epsilon
    
    # Нормализация снова
    dist1_norm <- dist1_norm / sum(dist1_norm)
    dist2_norm <- dist2_norm / sum(dist2_norm)
    
    # Расчет PSI
    psi <- sum((dist1_norm - dist2_norm) * log(dist1_norm / dist2_norm), na.rm = TRUE)
    
    return(psi)
  }
  
  # Определяем индекс базового интервала
  baseline_idx <- NA_integer_
  if (is.numeric(baseline_interval) && length(baseline_interval) == 1 && is.finite(baseline_interval)) {
    bi <- as.integer(baseline_interval)
    if (bi >= 1 && bi <= n_intervals) baseline_idx <- bi
  } else if (is.character(baseline_interval) && length(baseline_interval) == 1) {
    # Ожидается имя вида "Interval_k"
    labs <- paste0("Interval_", seq_len(n_intervals))
    match_idx <- match(baseline_interval, labs)
    if (!is.na(match_idx)) baseline_idx <- match_idx
  }
  if (is.na(baseline_idx)) baseline_idx <- 1L
  
  # Основной расчет PSI для каждой переменной
  results <- data.frame(
    variable = feature_cols,
    stringsAsFactors = FALSE
  )
  
  # Добавление столбцов для каждого интервала
  if (n_intervals > 0) {
    for (i in seq_len(n_intervals)) {
      results[[paste0("PSI_Interval_", i)]] <- NA_real_
    }
  }
  
  # Расчет PSI для каждой переменной
  for (var_idx in seq_along(feature_cols)) {
    var_name <- feature_cols[var_idx]
    
    # Получение данных для переменной
    var_data <- df[[var_name]]
    
    # Получение WOE значений для каждого временного интервала
    woe_by_interval <- list()
    
    if (n_intervals > 0) {
      for (i in seq_len(n_intervals)) {
        interval_name <- paste0("Interval_", i)
        interval_mask <- df$time_interval == interval_name
        
        if (sum(interval_mask) >= min_bucket_size) {
          # Используем уже готовые WOE значения
          woe_by_interval[[i]] <- mean(var_data[interval_mask], na.rm = TRUE)
        } else {
          woe_by_interval[[i]] <- NA
        }
      }
    }
    
    # Расчет PSI относительно базового интервала
    if (n_intervals >= 2) {
      base_val <- woe_by_interval[[baseline_idx]]
      if (!is.na(base_val)) {
        base_dist <- c(base_val, 1 - abs(base_val))
        for (i in seq_len(n_intervals)) {
          if (i == baseline_idx) next
          cur_val <- woe_by_interval[[i]]
          if (!is.na(cur_val)) {
            cur_dist <- c(cur_val, 1 - abs(cur_val))
            psi_value <- calculate_psi_between(base_dist, cur_dist)
            results[var_idx, paste0("PSI_Interval_", i)] <- psi_value
          }
        }
      }
    }
  }
  
  # Округление значений PSI до 6 знаков после запятой
  psi_cols <- grep("^PSI_", names(results), value = TRUE)
  results[psi_cols] <- round(results[psi_cols], 6)
  
  # Добавляем информацию об интервалах как атрибут результата
  attr(results, "interval_info") <- interval_info
  attr(results, "baseline_interval") <- paste0("Interval_", baseline_idx)
  
  return(results)
}
