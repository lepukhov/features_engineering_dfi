# Функция для расчета PSI (Population Stability Index) для кредитного скоринга
# Параметры:
# df - датафрейм с данными, где переменные уже содержат WOE значения
# target_col - название столбца с целевой переменной
# date_col - название столбца с датой
# n_intervals - количество временных интервалов (по умолчанию 6)
# min_bucket_size - минимальный размер бакета для анализа (по умолчанию 50)

calculate_psi <- function(df, target_col, date_col, n_intervals = 6, min_bucket_size = 50) {
  
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
  
  # Если количество уникальных дат меньше количества интервалов, используем уникальные даты
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
    # Стандартное разбиение на равные интервалы
    date_range <- range(df[[date_col]], na.rm = TRUE)
    total_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
    interval_size <- total_days / n_intervals
    
    # Создание границ интервалов с равным количеством дней
    interval_breaks <- seq(date_range[1], date_range[2], by = interval_size)
    
    # Добавляем последнюю границу, если нужно
    if (length(interval_breaks) <= n_intervals) {
      interval_breaks <- c(interval_breaks, date_range[2])
    }
    
    # Создание временных интервалов
    df$time_interval <- cut(df[[date_col]], 
                            breaks = interval_breaks,
                            labels = paste0("Interval_", 1:n_intervals),
                            include.lowest = TRUE)
    
    # Проверка равномерности интервалов (для отладки)
    interval_info <- data.frame(
      interval = levels(df$time_interval),
      start_date = interval_breaks[1:n_intervals],
      end_date = interval_breaks[2:(n_intervals+1)],
      days = as.numeric(difftime(interval_breaks[2:(n_intervals+1)], interval_breaks[1:n_intervals], units = "days"))
    )
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
  
  # Основной расчет PSI для каждой переменной
  results <- data.frame(
    variable = feature_cols,
    stringsAsFactors = FALSE
  )
  
  # Добавление столбцов для каждого интервала
  if (n_intervals > 0) {
    for (i in seq_len(n_intervals)) {
      results[[paste0("PSI_Interval_", i)]] <- 0
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
    
    # Расчет PSI между соседними интервалами
    if (n_intervals >= 2) {
      for (i in 2:n_intervals) {
        if (!is.na(woe_by_interval[[i-1]]) && !is.na(woe_by_interval[[i]])) {
          # Создание простых распределений на основе WOE
          dist1 <- c(woe_by_interval[[i-1]], 1 - abs(woe_by_interval[[i-1]]))
          dist2 <- c(woe_by_interval[[i]], 1 - abs(woe_by_interval[[i]]))
          
          psi_value <- calculate_psi_between(dist1, dist2)
          results[var_idx, paste0("PSI_Interval_", i)] <- psi_value
        }
      }
    }
  }
  
  # Округление значений PSI до 4 знаков после запятой
  psi_cols <- grep("^PSI_", names(results), value = TRUE)
  results[psi_cols] <- round(results[psi_cols], 4)
  
  # Добавляем информацию об интервалах как атрибут результата
  attr(results, "interval_info") <- interval_info
  
  return(results)
}
