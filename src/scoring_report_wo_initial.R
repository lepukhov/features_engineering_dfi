# Требуются пакеты:
# install.packages(c("openxlsx","ggplot2","data.table","scorecard"))

export_default_model_report_excel <- function(
    model,                          # glm(binomial) на WOE-фичах; если NULL, ожидаются заранее рассчитанные PD в колонке .__pd__
    train, test, oot,               # data.frame сырых данных (без WOE-замены)
    target_col,                     # имя целевой переменной (0/1 или фактор с 2 уровнями)
    date_col,                       # колонка даты; будет агрегирована до YYYY-MM
    bins,                           # ОБЯЗАТЕЛЬНО: bins из scorecard::woebin, использовавшиеся при разработке
    feature_cols = NULL,            # по умолчанию names(bins)
    id_col = NULL,                  # (опц.) идентификатор
    var_meta = NULL,                # (опц.) data.frame: variable, type, source
    oot_reason = "",                # (опц.) текст-обоснование периода OOT
    cutoffs = c(0.03, 0.05, 0.10, 0.20),  # PD cutoffs для cut-off анализа (approve: PD <= cut-off)
    loss_per_default = NA_real_,    # (опц.) потери на 1 дефолт, если нет exposure
    exposure_col = NULL,            # (опц.) колонка с экспозицией (EAD)
    lgd = 1.0,                      # (опц.) LGD (если есть exposure)
    top_share_for_lift = 0.10,      # доля для Lift-in-time (по самым рискованным PD)
    pdo = 50, score_ref = 600, odds_ref = 50, # параметры скоринга Score = A - B*log(odds)
    excel_path = "default_model_report.xlsx",
    # Лист: сведения по факторам (готовая таблица)
    dt_enriched_info = NULL,        # (опц.) готовая таблица для листа 2.2b
    overwrite = TRUE
) {
  req <- c("openxlsx","ggplot2","data.table","scorecard")
  miss <- req[!sapply(req, requireNamespace, quietly = TRUE)]
  if (length(miss)) stop("Требуются пакеты: ", paste(miss, collapse = ", "),
                         ". Установите: install.packages(c('", paste(miss, collapse="','"), "'))")
  library(ggplot2)
  library(data.table)
  
  if (is.null(feature_cols)) feature_cols <- names(bins)
  if (!all(feature_cols %in% names(bins))) {
    stop("Некоторые feature_cols отсутствуют в bins: ", paste(setdiff(feature_cols, names(bins)), collapse = ", "))
  }
  
  white_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA)
    )
  
  as_month <- function(x) {
    if (inherits(x, "Date")) return(format(x, "%Y-%m"))
    if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m"))
    suppressWarnings({
      d <- as.Date(x)
      if (any(!is.na(d))) return(format(d, "%Y-%m"))
    })
    as.character(x)
  }
  ensure_binary <- function(y) {
    if (is.factor(y)) {
      if (nlevels(y) != 2) stop("Целевая переменная должна быть бинарной")
      as.integer(y == levels(y)[2])
    } else {
      vals <- sort(unique(as.integer(as.numeric(y))))
      if (length(vals) != 2) stop("Целевая переменная должна быть бинарной (0/1)")
      as.integer(as.numeric(y) == max(vals))
    }
  }
  pct_scale <- function() {
    scale_y_continuous(
      limits = c(0,1),
      breaks = seq(0, 1, 0.1),
      labels = function(x) paste0(sprintf("%.2f", x * 100), "%")
    )
  }
  pct_scale_x <- function() {
    scale_x_continuous(
      limits = c(0,1),
      breaks = seq(0, 1, 0.1),
      labels = function(x) paste0(sprintf("%.2f", x * 100), "%")
    )
  }
  y_2dec <- function() scale_y_continuous(labels = function(x) sprintf("%.2f", x))
  
  auc_gini_ks <- function(score, y) {
    y <- as.integer(y)
    ok <- is.finite(score) & !is.na(y)
    s <- score[ok]; y <- y[ok]
    if (length(s) == 0 || length(unique(y)) < 2) return(list(auc=NA_real_, gini=NA_real_, ks=NA_real_))
    if (length(unique(s)) <= 1) return(list(auc = 0.5, gini = 0.0, ks = 0.0))
    ord <- order(s, decreasing = TRUE)
    y <- y[ord]
    P <- sum(y); N <- length(y) - P
    if (P == 0 || N == 0) return(list(auc=NA_real_, gini=NA_real_, ks=NA_real_))
    tp <- cumsum(y); fp <- cumsum(1 - y)
    tpr <- c(0, tp / P, 1); fpr <- c(0, fp / N, 1)
    auc <- sum((fpr[-1] - fpr[-length(fpr)]) * (tpr[-1] + tpr[-length(tpr)]) / 2)
    ks <- max((tp / P) - (fp / N))
    list(auc = auc, gini = 2 * auc - 1, ks = ks)
  }
  ks_stat <- function(pd, y) {
    y <- ensure_binary(y)
    ok <- is.finite(pd) & !is.na(y)
    pd <- pd[ok]; y <- y[ok]
    ord <- order(pd, decreasing = TRUE)
    y <- y[ord]
    P <- sum(y); N <- length(y) - P
    if (P == 0 || N == 0) return(NA_real_)
    csp <- cumsum(y) / P
    csn <- cumsum(1 - y) / N
    max(abs(csp - csn))
  }
  score_from_pd <- function(pd, pdo = 50, score_ref = 600, odds_ref = 50) {
    pd <- pmin(pmax(pd, 1e-12), 1 - 1e-12)
    odds <- (1 - pd) / pd
    B <- pdo / log(2)
    A <- score_ref + B * log(odds_ref)
    as.integer(round(A - B * log(odds)))
  }
  make_deciles <- function(score, y, k = 10) {
    q <- quantile(score, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE, type = 7)
    grp <- cut(score, breaks = unique(q), include.lowest = TRUE, labels = FALSE)
    dt <- data.table(decile = grp, y = y, score = score)
    dt[!is.na(decile), .(n = .N, rate = mean(y), avg_score = mean(score)), by = decile][order(decile)]
  }
  lift_at_share <- function(pd, y, share = 0.1) {
    ok <- is.finite(pd) & !is.na(y)
    pd <- pd[ok]; y <- y[ok]
    if (!length(pd)) return(NA_real_)
    ord <- order(pd, decreasing = TRUE)
    n <- length(pd); top_n <- max(1L, floor(n * share))
    idx <- ord[seq_len(top_n)]
    rate_top <- mean(y[idx]); rate_all <- mean(y)
    if (rate_all == 0) return(NA_real_)
    (rate_top / rate_all)
  }
  psi_two <- function(expected, actual, breaks, min_bins = 10, eps = 1e-10) {
    expected <- expected[is.finite(expected)]
    actual   <- actual[is.finite(actual)]
    br <- sort(unique(as.numeric(breaks)))
    if (length(br) < 2) {
      rng <- range(expected, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) return(0)
      br <- seq(rng[1], rng[2], length.out = min_bins + 1)
    }
    br_ext <- sort(unique(c(-Inf, br, Inf)))
    e_tbl <- table(cut(expected, breaks = br_ext, include.lowest = TRUE, right = TRUE))
    a_tbl <- table(cut(actual,   breaks = br_ext, include.lowest = TRUE, right = TRUE))
    ep <- as.numeric(e_tbl) / sum(e_tbl)
    ap <- as.numeric(a_tbl) / sum(a_tbl)
    ep <- pmax(ep, eps); ap <- pmax(ap, eps)
    ep <- ep / sum(ep);  ap <- ap / sum(ap)
    sum((ap - ep) * log(ap / ep))
  }
  psi_feature_two <- function(x_train, x_other, bins = 10, eps = 1e-10) {
    x_train <- x_train[is.finite(x_train)]
    x_other <- x_other[is.finite(x_other)]
    if (length(x_train) == 0 || length(x_other) == 0) return(NA_real_)
    br <- quantile(x_train, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE, type = 7)
    br <- sort(unique(as.numeric(br)))
    if (length(br) < 2) {
      rng <- range(x_train, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) return(0)
      br <- seq(rng[1], rng[2], length.out = max(3, bins + 1))
    }
    br_ext <- sort(unique(c(-Inf, br, Inf)))
    e_tbl <- table(cut(x_train, breaks = br_ext, include.lowest = TRUE, right = TRUE))
    a_tbl <- table(cut(x_other, breaks = br_ext, include.lowest = TRUE, right = TRUE))
    ep <- as.numeric(e_tbl) / sum(e_tbl)
    ap <- as.numeric(a_tbl) / sum(a_tbl)
    ep <- pmax(ep, eps); ap <- pmax(ap, eps)
    ep <- ep / sum(ep);  ap <- ap / sum(ap)
    sum((ap - ep) * log(ap / ep))
  }
  miss_outlier_table <- function(df, feature_cols) {
    res <- lapply(feature_cols, function(v) {
      x <- df[[v]]
      miss <- mean(is.na(x))
      if (is.numeric(x)) {
        xna <- x[!is.na(x)]
        if (length(xna) > 4) {
          q <- stats::quantile(xna, c(.25,.75), na.rm = TRUE)
          iqr <- q[2] - q[1]; lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
          out <- mean(xna < lo | xna > hi)
        } else out <- NA_real_
      } else out <- NA_real_
      data.frame(variable = v, missing_share = miss, outlier_share = out, stringsAsFactors = FALSE)
    })
    do.call(rbind, res)
  }
  add_month <- function(df) { df[[".__month__"]] <- as_month(df[[date_col]]); df }
  
  train <- as.data.frame(train); test <- as.data.frame(test); oot <- as.data.frame(oot)
  for (nm in c(target_col, date_col)) {
    if (!all(c(nm %in% names(train), nm %in% names(test), nm %in% names(oot)))) {
      stop("Не найдена колонка '", nm, "' во всех выборках")
    }
  }
  train$.__y__ <- ensure_binary(train[[target_col]])
  test$.__y__  <- ensure_binary(test[[target_col]])
  oot$.__y__   <- ensure_binary(oot[[target_col]])
  train <- add_month(train); test <- add_month(test); oot <- add_month(oot)
  
  train_woe <- as.data.frame(scorecard::woebin_ply(train[, feature_cols, drop = FALSE], bins))
  test_woe  <- as.data.frame(scorecard::woebin_ply(test[,  feature_cols, drop = FALSE], bins))
  oot_woe   <- as.data.frame(scorecard::woebin_ply(oot[,   feature_cols, drop = FALSE], bins))
  
  if (!is.null(model)) {
    combine_with_woe <- function(df_raw, df_woe) {
      nd <- df_raw
      for (cn in names(df_woe)) nd[[cn]] <- df_woe[[cn]]
      nd
    }
    nd_train <- combine_with_woe(train, train_woe)
    nd_test  <- combine_with_woe(test,  test_woe)
    nd_oot   <- combine_with_woe(oot,   oot_woe)
    train$.__pd__ <- stats::predict(model, newdata = nd_train, type = "response")
    test$.__pd__  <- stats::predict(model, newdata = nd_test,  type = "response")
    oot$.__pd__   <- stats::predict(model, newdata = nd_oot,   type = "response")
  } else {
    for (nm in c("train","test","oot")) {
      if (!(".__pd__" %in% names(get(nm)))) stop("Если model=NULL, ожидается колонка .__pd__ в каждом наборе (", nm, ")")
    }
  }
  
  # 1.1 — Описание выборок + объединённый график (кол-во = столбцы, дефолт-рейт = линия)
  sample_desc <- function(df, name) {
    data.frame(
      выборка = name,
      n = nrow(df),
      дефолтов = sum(df$.__y__, na.rm = TRUE),
      доля_дефолтов = mean(df$.__y__, na.rm = TRUE),
      период_min = if (all(is.na(df$.__month__))) NA_character_ else as.character(min(df$.__month__, na.rm = TRUE)),
      период_max = if (all(is.na(df$.__month__))) NA_character_ else as.character(max(df$.__month__, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  }
  samples_tbl <- rbind(sample_desc(train,"Train"), sample_desc(test,"Test"), sample_desc(oot,"OOT"))
  
  monthly_counts <- function(df, name) {
    if (all(is.na(df$.__month__))) return(NULL)
    data.table(df)[, .(n = .N, дефолтрейт = mean(.__y__)), by = .(месяц = .__month__)][, выборка := name][order(месяц)]
  }
  mc <- rbindlist(Filter(Negate(is.null), list(
    monthly_counts(train, "Train"),
    monthly_counts(test, "Test"),
    monthly_counts(oot,  "OOT")
  )), fill = TRUE)
  if (nrow(mc)) mc$месяц <- factor(mc$месяц, levels = sort(unique(mc$месяц)))
  
  plot_counts_defrate <- NULL
  if (nrow(mc)) {
    mc_total <- data.table(mc)[, .(n_total = sum(n)), by = месяц]
    den <- max(mc_total$n_total, na.rm = TRUE); den <- ifelse(is.finite(den) && den > 0, den, 1)
    plot_counts_defrate <- ggplot() +
      geom_col(data = mc, aes(x = месяц, y = n, fill = выборка), position = "stack", alpha = 0.6) +
      geom_line(data = mc, aes(x = месяц, y = дефолтрейт * den, color = выборка, group = выборка), size = 1.2) +
      geom_point(data = mc, aes(x = месяц, y = дефолтрейт * den, color = выборка, group = выборка), size = 1.5) +
      scale_y_continuous(
        name = "Число наблюдений",
        labels = function(x) sprintf("%.2f", x),
        limits = c(0, den),
        sec.axis = sec_axis(
          ~ . / den,
          name = "Доля дефолтов",
          breaks = seq(0, 1, 0.1),
          labels = function(x) paste0(sprintf("%.2f", x * 100), "%")
        )
      ) +
      labs(title = "Наблюдения и доля дефолтов по месяцам", x = "Месяц") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      white_theme
  }
  
  # 1.2 — Качество данных + PSI во времени
  miss_train <- miss_outlier_table(train, feature_cols); miss_train$выборка <- "Train"
  miss_test  <- miss_outlier_table(test,  feature_cols); miss_test$выборка  <- "Test"
  miss_oot   <- miss_outlier_table(oot,   feature_cols); miss_oot$выборка   <- "OOT"
  miss_tbl <- rbind(miss_train, miss_test, miss_oot)
  
  train_scores <- score_from_pd(train$.__pd__, pdo, score_ref, odds_ref)
  breaks_score <- seq(200, 1000, by = 20)
  psi_time <- function(df, name) {
    if (all(is.na(df$.__month__))) return(NULL)
    sc <- score_from_pd(df$.__pd__, pdo, score_ref, odds_ref)
    dt <- data.table(month = df$.__month__, score = sc)
    dt <- dt[is.finite(score)]
    base <- train_scores[is.finite(train_scores)]
    out <- dt[, .(PSI = psi_two(expected = base, actual = score, breaks = breaks_score)), by = month][order(month)]
    out$выборка <- name
    out
  }
  psi_df <- rbindlist(Filter(Negate(is.null), list(
    psi_time(train, "Train"), psi_time(test, "Test"), psi_time(oot, "OOT")
  )), fill = TRUE)
  
  plot_psi_time <- ggplot(psi_df, aes(x = month, y = PSI, color = выборка, group = выборка)) +
    geom_line() + geom_point(size = 1.5) +
    labs(title = "Стабильность распределения скоринговых баллов (PSI) во времени", x = "Месяц", y = "PSI") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
    white_theme +
    geom_hline(yintercept = c(0.1, 0.25), linetype = "dashed", color = "gray50")
  
  # 1.3 — OOT
  psi_features <- lapply(feature_cols, function(v) {
    vn <- paste0(v, "_woe")
    data.frame(
      variable = v,
      PSI_train_vs_test = tryCatch(psi_feature_two(train_woe[[vn]], test_woe[[vn]], bins = 10), error=function(e) NA_real_),
      PSI_train_vs_oot  = tryCatch(psi_feature_two(train_woe[[vn]],  oot_woe[[vn]],  bins = 10), error=function(e) NA_real_),
      stringsAsFactors = FALSE
    )
  })
  psi_features_df <- data.table::rbindlist(psi_features, fill = TRUE)
  psi_overall <- data.frame(
    сравнение = c("Train vs Test", "Train vs OOT"),
    PSI_score = c(
      psi_two(train_scores, score_from_pd(test$.__pd__, pdo, score_ref, odds_ref), breaks_score),
      psi_two(train_scores, score_from_pd(oot$.__pd__,  pdo, score_ref, odds_ref), breaks_score)
    ),
    stringsAsFactors = FALSE
  )
  
  # 2.2 — Вклад/IV/Gini (объединённый)
  # Wald
  wald_contrib <- function(model) {
    sm <- summary(model)$coefficients
    df <- data.frame(
      term = rownames(sm),
      estimate = sm[, "Estimate"],
      std_error = sm[, "Std. Error"],
      z = sm[, "z value"],
      p_value = sm[, "Pr(>|z|)"],
      row.names = NULL
    )
    df <- df[df$term != "(Intercept)", , drop = FALSE]
    df$wald_chi2 <- (df$z)^2
    s <- sum(df$wald_chi2, na.rm = TRUE)
    df$contribution_pct <- if (s > 0) 100 * df$wald_chi2 / s else NA_real_
    df$variable <- sub("_woe$", "", df$term)
    df
  }
  contrib_df <- if (!is.null(model)) wald_contrib(model) else data.frame()
  
  # IV из bins (train) и на test
  get_iv_from_bins <- function(bin_df) {
    if ("total_iv" %in% names(bin_df)) {
      ivs <- unique(na.omit(as.numeric(bin_df$total_iv)))
      if (length(ivs) >= 1) return(ivs[length(ivs)])
    }
    if ("bin_iv" %in% names(bin_df)) return(sum(as.numeric(bin_df$bin_iv), na.rm = TRUE))
    NA_real_
  }
  iv_df <- data.frame(variable = names(bins),
                      iv_train = sapply(bins, get_iv_from_bins),
                      row.names = NULL, check.names = FALSE)
  
  compute_iv_by_dataset <- function(woe_df, y, feature_cols, eps = 1e-10) {
    y <- as.integer(y)
    sapply(feature_cols, function(v) {
      vn <- paste0(v, "_woe"); x <- woe_df[[vn]]
      ok <- is.finite(x) & !is.na(y); x <- x[ok]; yy <- y[ok]
      if (!length(x) || length(unique(yy)) < 2) return(NA_real_)
      total_good <- sum(yy == 0); total_bad <- sum(yy == 1)
      if (total_good == 0 || total_bad == 0) return(NA_real_)
      dt <- data.table(woe = round(x, 10), y = yy)
      agg <- dt[, .(good = sum(y == 0), bad = sum(y == 1)), by = woe]
      p_good <- agg$good / total_good; p_bad <- agg$bad / total_bad
      w <- log(pmax(p_good, eps) / pmax(p_bad, eps))
      sum((p_good - p_bad) * w, na.rm = TRUE)
    }, simplify = TRUE, USE.NAMES = TRUE)
  }
  iv_test_vec <- compute_iv_by_dataset(test_woe, test$.__y__, feature_cols)
  
  # Per-feature Gini (train/test/oot)
  per_feature_gini <- lapply(feature_cols, function(v) {
    vn <- paste0(v, "_woe")
    data.frame(
      variable   = v,
      gini_train = abs(tryCatch(auc_gini_ks(train_woe[[vn]], train$.__y__)$gini, error=function(e) NA_real_)),
      gini_test  = abs(tryCatch(auc_gini_ks(test_woe[[vn]],  test$.__y__)$gini,  error=function(e) NA_real_)),
      gini_oot   = abs(tryCatch(auc_gini_ks(oot_woe[[vn]],   oot$.__y__)$gini,   error=function(e) NA_real_)),
      stringsAsFactors = FALSE
    )
  })
  per_feature_gini_df <- data.table::rbindlist(per_feature_gini, fill = TRUE)
  
  feature_metrics_df <- Reduce(function(x, y) merge(x, y, by = "variable", all = TRUE),
                               list(iv_df, per_feature_gini_df))
  feature_metrics_df$iv_test <- as.numeric(iv_test_vec[match(feature_metrics_df$variable, names(iv_test_vec))])
  # Таблица для листа 2.2b — берём готовую dt_enriched_info, если передана
  enriched_info_df <- if (!is.null(dt_enriched_info)) as.data.frame(dt_enriched_info) else data.frame()
  
  # Пошаговый вклад в итоговый GINI (Train), порядок по значимости (Wald chi2)
  if (!is.null(model) && nrow(contrib_df)) {
    order_vars <- contrib_df[order(-contrib_df$wald_chi2, contrib_df$variable), "variable"]
    coefs <- stats::coef(model)
    b0 <- unname(ifelse("(Intercept)" %in% names(coefs), coefs["(Intercept)"], 0))
    woe_names <- paste0(order_vars, "_woe")
    woe_names <- woe_names[woe_names %in% names(coefs) & woe_names %in% names(train_woe)]
    order_vars <- sub("_woe$", "", woe_names)
    lp <- rep(b0, nrow(train))
    g_prev <- auc_gini_ks(plogis(lp), train$.__y__)$gini
    g_full <- auc_gini_ks(plogis(b0 + as.matrix(train_woe[, woe_names, drop = FALSE]) %*% coefs[woe_names]), train$.__y__)$gini
    inc_pp <- numeric(length(woe_names)); cum_pp <- numeric(length(woe_names))
    for (k in seq_along(woe_names)) {
      wn <- woe_names[k]
      lp <- lp + coefs[wn] * train_woe[[wn]]
      g_now <- auc_gini_ks(plogis(lp), train$.__y__)$gini
      inc_pp[k] <- (g_now - g_prev) * 100
      cum_pp[k] <- (g_now) * 100
      g_prev <- g_now
    }
    diff_total <- (g_full * 100) - sum(inc_pp, na.rm = TRUE)
    if (is.finite(diff_total) && length(inc_pp) > 0) inc_pp[1] <- inc_pp[1] + diff_total
    gini_seq_df <- data.frame(
      variable = order_vars,
      order = seq_along(order_vars),
      gini_increment_pp = round(inc_pp, 2),
      gini_cumulative_pp = round(cum_pp, 2),
      contrib_display = ifelse(seq_along(order_vars) == 1,
                               paste0(sprintf("%.2f", round(inc_pp,2)[1]), "%"),
                               paste0(ifelse(round(inc_pp,2)[-1] >= 0, "+", ""),
                                      sprintf("%.2f", round(inc_pp,2)[-1]))),
      gini_train_total_pp = round(g_full * 100, 2),
      stringsAsFactors = FALSE
    )
    # Объединяем всё в единую таблицу для листа 2.2
    contrib_keep <- contrib_df[, c("variable","estimate","std_error","z","p_value","wald_chi2","contribution_pct")]
    combined_22 <- Reduce(function(x, y) merge(x, y, by = "variable", all = TRUE),
                          list(feature_metrics_df, contrib_keep, gini_seq_df))
  } else {
    combined_22 <- feature_metrics_df
  }
  
  # 2.3 — WOE
  woe_tables <- lapply(names(bins), function(v) cbind(variable = v, bins[[v]]))
  woe_table_df <- data.table::rbindlist(woe_tables, fill = TRUE)
  woe_plots <- tryCatch(scorecard::woebin_plot(bins), error = function(e) NULL)
  
  # Корреляции (WOE, Train)
  cor_plot <- NULL
  {
    woe_cols <- grep("_woe$", names(train_woe), value = TRUE)
    if (length(woe_cols) >= 2) {
      woe_num <- as.data.frame(train_woe[, woe_cols, drop = FALSE])
      woe_num <- woe_num[, sapply(woe_num, is.numeric), drop = FALSE]
      if (ncol(woe_num) >= 2) {
        cormat <- suppressWarnings(stats::cor(woe_num, use = "pairwise.complete.obs"))
        cm <- as.data.frame(as.table(cormat)); names(cm) <- c("x","y","corr")
        cor_plot <- ggplot(cm, aes(x = x, y = y, fill = corr)) +
          geom_tile() +
          scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0) +
          labs(title = "Матрица корреляций (WOE, Train)", x = NULL, y = NULL, fill = "corr") +
          white_theme +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }
    }
  }
  
  # Качество модели (объединяем 3.5 + 3.6 + 3.7)
  metrics_set <- function(df) {
    ranker <- pmin(pmax(df$.__pd__, 1e-12), 1 - 1e-12)
    m <- auc_gini_ks(ranker, df$.__y__)
    list(auc = m$auc, gini = m$gini, ks = ks_stat(df$.__pd__, df$.__y__))
  }
  ms_train <- metrics_set(train); ms_test <- metrics_set(test); ms_oot <- metrics_set(oot)
  metrics_overall <- data.frame(
    выборка = c("Train","Test","OOT"),
    AUC = c(ms_train$auc, ms_test$auc, ms_oot$auc),
    GINI = c(ms_train$gini, ms_test$gini, ms_oot$gini),
    KS = c(ms_train$ks, ms_test$ks, ms_oot$ks),
    stringsAsFactors = FALSE
  )
  
  roc_data <- function(df, name) {
    ok <- is.finite(df$.__pd__) & !is.na(df$.__y__)
    pd <- df$.__pd__[ok]; y <- df$.__y__[ok]
    ord <- order(pd, decreasing = TRUE)
    y <- y[ord]; P <- sum(y); N <- length(y) - P
    tp <- cumsum(y); fp <- cumsum(1 - y)
    data.frame(выборка = name, fpr = c(0, fp / N, 1), tpr = c(0, tp / P, 1))
  }
  roc_df <- rbind(roc_data(train,"Train"), roc_data(test,"Test"), roc_data(oot,"OOT"))
  roc_plot <- ggplot(roc_df, aes(x = fpr, y = tpr, color = выборка)) +
    geom_line(size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(title = "ROC-кривые", x = "FPR", y = "TPR") +
    pct_scale_x() + pct_scale() + white_theme
  
  monthly_metric <- function(df, name) {
    if (all(is.na(df$.__month__))) return(NULL)
    data.table(df)[, .(
      GINI = tryCatch(auc_gini_ks(pmin(pmax(.__pd__, 1e-12), 1-1e-12), .__y__)$gini, error=function(e) NA_real_),
      LIFT = tryCatch(lift_at_share(.__pd__, .__y__, top_share_for_lift), error=function(e) NA_real_)
    ), by = .(месяц = .__month__)][, выборка := name][order(месяц)]
  }
  gini_lift_month <- rbindlist(Filter(Negate(is.null), list(
    monthly_metric(train,"Train"), monthly_metric(test,"Test"), monthly_metric(oot,"OOT")
  )), fill = TRUE)
  
  gini_time_plot <- ggplot(gini_lift_month, aes(x = месяц, y = GINI, color = выборка, group = выборка)) +
    geom_line() + geom_point(size = 1.5) +
    labs(title = "Gini по месяцам", x = "Месяц", y = "GINI") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    pct_scale() + white_theme
  
  lift_time_plot <- ggplot(gini_lift_month, aes(x = месяц, y = LIFT, color = выборка, group = выборка)) +
    geom_line() + geom_point(size = 1.5) +
    labs(title = sprintf("Lift по месяцам (топ %.0f%% по риску)", 100 * top_share_for_lift), x = "Месяц", y = "Lift") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    y_2dec() + white_theme +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50")
  
  # Score анализ
  add_score <- function(df) { df$.__score__ <- score_from_pd(df$.__pd__, pdo, score_ref, odds_ref); df }
  train <- add_score(train); test <- add_score(test); oot <- add_score(oot)
  score_deciles_tbl <- function(df, name) {
    dec <- make_deciles(df$.__score__, df$.__y__, 10); dec$выборка <- name; dec
  }
  score_deciles <- rbind(score_deciles_tbl(train,"Train"), score_deciles_tbl(test,"Test"), score_deciles_tbl(oot,"OOT"))
  drate_by_decile_plot <- ggplot(score_deciles, aes(x = factor(decile), y = rate, fill = выборка)) +
    geom_col(position = position_dodge()) +
    labs(title = "Средний дефолт-рейт по децилям Score", x = "Дециль (Score)", y = "Доля дефолтов") +
    pct_scale() + white_theme
  score_hist <- ggplot(rbind(
    data.frame(score = train$.__score__, выборка = "Train"),
    data.frame(score = test$.__score__,  выборка = "Test"),
    data.frame(score = oot$.__score__,   выборка = "OOT")
  ), aes(x = score, fill = выборка)) +
    geom_histogram(position = "identity", alpha = 0.4, bins = 50) +
    labs(title = "Распределение скоринговых баллов", x = "Score", y = "Count") +
    y_2dec() + white_theme
  
  # Cut-off — лист и расчёты убраны по требованию
  
  # Книга Excel
  wb <- openxlsx::createWorkbook()
  save_plot_png <- function(plot_obj, width = 8, height = 5, dpi = 150) {
    fn <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = fn, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
    fn
  }
  
  # 1.1 — описание + комбинированный график
  openxlsx::addWorksheet(wb, "1.1_Описание_выборок")
  openxlsx::writeData(wb, "1.1_Описание_выборок", samples_tbl)
  if (!is.null(plot_counts_defrate)) {
    openxlsx::insertImage(wb, "1.1_Описание_выборок", save_plot_png(plot_counts_defrate), startRow = 10, startCol = 1, width = 10, height = 5, units = "in")
  }
  
  # Новая страница: 2.2b — Описания/IV/Drop Reasons (enriched info)
  if (nrow(enriched_info_df)) {
    openxlsx::addWorksheet(wb, "1.2_Отбор_переменных")
    openxlsx::writeData(wb, "1.2_Отбор_переменных", enriched_info_df)
  }
  
  # 1.2 — качество данных
  openxlsx::addWorksheet(wb, "1.3_Качество_данных")
  openxlsx::writeData(wb, "1.3_Качество_данных", miss_tbl)
  openxlsx::insertImage(wb, "1.3_Качество_данных", save_plot_png(plot_psi_time), startRow = 2, startCol = 10, width = 10, height = 5, units = "in")
  
  # 1.3 — OOT
  openxlsx::addWorksheet(wb, "1.3_OOT")
  openxlsx::writeData(wb, "1.3_OOT", data.frame(обоснование_OOT = oot_reason, stringsAsFactors = FALSE))
  openxlsx::writeData(wb, "1.3_OOT", psi_overall, startRow = 5, startCol = 1)
  openxlsx::writeData(wb, "1.3_OOT", psi_features_df, startRow = 10, startCol = 1)
  
  # 2.2 — объединённый IV/Gini/Вклад
  openxlsx::addWorksheet(wb, "2.2_IV_Gini_Вклад")
  # Уберём дублирующие/пустые столбцы автоматически
  keep_cols <- c("variable","iv_train","iv_test","gini_train","gini_test","gini_oot",
                 "estimate","std_error","z","p_value","wald_chi2","contribution_pct",
                 "order","gini_increment_pp","gini_cumulative_pp","contrib_display","gini_train_total_pp")
  present <- intersect(keep_cols, names(combined_22))
  combined_out <- combined_22[, present, drop = FALSE]
  # Сортировка: по order, затем по iv_train у оставшихся
  ord_idx <- if ("order" %in% names(combined_out) && any(!is.na(combined_out$order))) {
    order(combined_out$order, na.last = TRUE)
  } else if ("iv_train" %in% names(combined_out)) {
    order(-combined_out$iv_train)
  } else {
    order(combined_out$variable)
  }
  openxlsx::writeData(wb, "2.2_IV_Gini_Вклад", combined_out[ord_idx, ])

  
  # 2.3 — WOE таблицы/графики
  openxlsx::addWorksheet(wb, "2.3_WOE_Таблицы")
  openxlsx::writeData(wb, "2.3_WOE_Таблицы", woe_table_df)
  if (!is.null(woe_plots) && length(woe_plots)) {
    openxlsx::addWorksheet(wb, "2.3_WOE_Графики")
    row <- 1
    for (i in seq_along(woe_plots)) {
      fn <- save_plot_png(woe_plots[[i]] + white_theme)
      openxlsx::insertImage(wb, "2.3_WOE_Графики", fn, startRow = row, startCol = 1, width = 7.5, height = 4.5, units = "in")
      row <- row + 28
    }
  }
  
  # 2.4 — Корреляции (если есть)
  if (!is.null(cor_plot)) {
    openxlsx::addWorksheet(wb, "2.4_Корреляции")
    openxlsx::insertImage(wb, "2.4_Корреляции", save_plot_png(cor_plot), startRow = 2, startCol = 1, width = 8, height = 8, units = "in")
  }
  
  # 3 — Качество модели (совмещённый блок)
  openxlsx::addWorksheet(wb, "3_Качество_Модели")
  openxlsx::writeData(wb, "3_Качество_Модели", metrics_overall)
  openxlsx::insertImage(wb, "3_Качество_Модели", save_plot_png(roc_plot),       startRow = 10, startCol = 1, width = 8, height = 6, units = "in")
  openxlsx::insertImage(wb, "3_Качество_Модели", save_plot_png(gini_time_plot), startRow = 28, startCol = 1, width = 8, height = 5, units = "in")
  openxlsx::insertImage(wb, "3_Качество_Модели", save_plot_png(lift_time_plot), startRow = 45, startCol = 1, width = 8, height = 5, units = "in")
  
  # 4.1 — Score распределения
  openxlsx::addWorksheet(wb, "4.1_Score_распределения")
  openxlsx::writeData(wb, "4.1_Score_распределения", score_deciles)
  openxlsx::insertImage(wb, "4.1_Score_распределения", save_plot_png(score_hist),           startRow = 15, startCol = 1, width = 8, height = 5, units = "in")
  openxlsx::insertImage(wb, "4.1_Score_распределения", save_plot_png(drate_by_decile_plot), startRow = 35, startCol = 1, width = 8, height = 5, units = "in")
  

  dir.create(dirname(excel_path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, excel_path, overwrite = overwrite)
  invisible(excel_path)
}

