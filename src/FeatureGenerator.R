#Алгоритм работы
#  
#  Фиксация seed, стратифицированные фолды по таргету
#Очистка категориальных: NA/"" → "MISSING"
#Композитные признаки:
#  
#  log1p для неотрицательных числовых: log1p__<var>
#  Для топ‑K числовых (по |Spearman| с таргетом) создаются:
#отношения: ratio__A__over__B
#разности: diff__A__minus__B
#произведения: prod__A__x__B
#Счётчик пропусков в числовых: count_missing_num
#
#Полиномиальные признаки (degree=2):
  
#квадраты: poly2__<var>__sq
#попарные произведения: poly2__A__x__B
#Только для выбранных топ‑K числовых

#Модельные OOF‑признаки (без утечки):
#rpart: oof_rpart_prob
#ranger (случайный лес): oof_rf_prob
#XGBoost: oof_xgb_prob (+ опционально листовые признаки)
#CatBoost: oof_cat_prob
#OOF-логика: для каждого фолда обучается модель на трен‑части и предсказывается hold‑out фолд; склейка даёт полный OOF-вектор.
#Обучение финальных полных моделей (на всей train выборке) для последующего скоринга:
  
#сохраняются в result$models
#XGBoost leaf index encoding (опционально):
  
#На полной XGBoost‑модели извлекаются индексы листьев (predleaf=TRUE)
#Режимы:
#index: столбцы xgb_leaf_tNNN с номерами листьев (целые)
#onehot: one‑hot матрица листьев; уровни сохраняются в meta$xgb_leaf_meta для корректного скоринга
#Можно ограничить число используемых деревьев (первые N)

# ============================================================

.stratified_folds <- function(y, k = 5, seed = 42) {
  set.seed(seed)
  y_fac <- as.factor(y)
  idx_by_class <- split(seq_along(y), y_fac)
  assign <- integer(length(y))
  for (cls in names(idx_by_class)) {
    ids <- idx_by_class[[cls]]
    ids <- sample(ids, length(ids))
    fold_ids <- rep(1:k, length.out = length(ids))
    assign[ids] <- fold_ids
  }
  split(seq_along(y), assign)
}

.safe_ratio <- function(a, b) {
  denom_guard <- stats::sd(b, na.rm = TRUE)
  if (!is.finite(denom_guard)) denom_guard <- 0
  denom_guard <- denom_guard * 0.1 + 1e-6
  out <- a / (b + denom_guard)
  out[!is.finite(out)] <- 0
  out
}

.select_top_numeric_by_corr <- function(df, y01, numeric_vars, top_k = 5) {
  if (length(numeric_vars) == 0) return(character(0))
  cs <- sapply(numeric_vars, function(v) {
    val <- suppressWarnings(stats::cor(df[[v]], y01, use = "complete.obs", method = "spearman"))
    if (is.na(val) || !is.finite(val)) 0 else abs(val)
  })
  names(sort(cs, decreasing = TRUE))[seq_len(min(top_k, length(cs)))]
}

.make_poly2 <- function(df, vars) {
  out <- list()
  for (v in vars) {
    x <- df[[v]]
    out[[paste0("poly2__", v, "__sq")]] <- x * x
  }
  if (length(vars) >= 2) {
    comb <- utils::combn(vars, 2, simplify = FALSE)
    for (p in comb) {
      x <- df[[p[1]]]; y <- df[[p[2]]]
      out[[paste0("poly2__", p[1], "__x__", p[2])]] <- x * y
    }
  }
  as.data.frame(out, check.names = FALSE)
}

.impute_num_with <- function(df, num_vars, medians_named) {
  for (v in num_vars) {
    if (!v %in% names(df)) next
    x <- df[[v]]
    fill <- suppressWarnings(as.numeric(medians_named[[v]]))
    if (!is.finite(fill) || is.na(fill)) fill <- 0
    x[is.na(x)] <- fill
    df[[v]] <- x
  }
  df
}

# XGBoost helper: try GPU, fallback to CPU if unsupported
.xgb_train <- function(params, data, nrounds, use_gpu = FALSE, gpu_id = 0, verbose = 0) {
  if (use_gpu) {
    params_gpu <- utils::modifyList(params, list(tree_method = "gpu_hist", predictor = "gpu_predictor", gpu_id = as.integer(gpu_id)))
    mod <- tryCatch(
      xgboost::xgb.train(params = params_gpu, data = data, nrounds = nrounds, verbose = verbose),
      error = function(e) NULL
    )
    if (!is.null(mod)) return(list(model = mod, used_gpu = TRUE))
  }
  params_cpu <- utils::modifyList(params, list(tree_method = "hist"))
  mod <- xgboost::xgb.train(params = params_cpu, data = data, nrounds = nrounds, verbose = verbose)
  list(model = mod, used_gpu = FALSE)
}

engineer_features_train <- function(
    df,
    target,
    n_folds = 5,
    top_num_for_pairs = 5,
    poly_degree = 2,
    seed = 42,
    use_rpart = TRUE,
    use_ranger = TRUE,
    use_xgboost = TRUE,
    use_catboost = FALSE,
    max_tree_depth = 3L,
    verbose = TRUE,
    progress = TRUE,
    xgb_use_gpu = TRUE,
    xgb_gpu_id = 0,
    id_col = NULL,
    
    # XGBoost hyperparams (OOF)
    xgb_objective = "binary:logistic",
    xgb_eval_metric = "logloss",
    xgb_max_depth = 4L,
    xgb_eta_oof = 0.10,
    xgb_subsample_oof = 0.80,
    xgb_colsample_bytree_oof = 0.80,
    xgb_min_child_weight = 8,
    xgb_gamma = 0,
    xgb_lambda = 1,
    xgb_alpha = 0,
    xgb_max_bin = 256L,
    xgb_nrounds_oof = 220L,
    xgb_params_extra = NULL,
    
    # XGBoost hyperparams (FULL)
    xgb_eta_full = 0.08,
    xgb_subsample_full = 0.85,
    xgb_colsample_bytree_full = 0.85,
    xgb_nrounds_full = 320L,
    xgb_params_full_extra = NULL,
    
    # XGBoost leaf features
    xgb_add_leaf_features = FALSE,          # TRUE to add leaf features
    xgb_leaf_encoding = c("index", "onehot")[2],  # "index" or "onehot" (default onehot)
    xgb_leaf_use_first_n_trees = NULL       # e.g. 100 to limit trees used for leaf features
) {
  stopifnot(target %in% names(df))
  t0 <- Sys.time()
  .log <- function(fmt, ...) if (isTRUE(verbose)) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), sprintf(fmt, ...)))
  .make_pb <- function(total) if (isTRUE(progress)) utils::txtProgressBar(min = 0, max = total, style = 3) else NULL
  .set_pb <- function(pb, i) if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  .close_pb <- function(pb) if (!is.null(pb)) close(pb)
  .headn <- function(x, n = 10) paste(utils::head(x, n), collapse = ", ")
  
  set.seed(seed)
  n_rows <- nrow(df); n_cols <- ncol(df)
  .log("Start FE: rows=%d, cols=%d, folds=%d, seed=%d", n_rows, n_cols, n_folds, seed)
  
  # Preserve ID and target
  id_vec <- NULL
  if (!is.null(id_col)) {
    if (!id_col %in% names(df)) stop(sprintf("id_col '%s' not found in data", id_col))
    id_vec <- df[[id_col]]
  }
  y_in <- df[[target]]
  
  # Target to 0/1
  y <- y_in
  if (is.factor(y) || is.character(y)) {
    y <- as.factor(y)
    if (nlevels(y) != 2L) stop("Target must be binary.")
    y01 <- as.integer(y == levels(y)[2L])
  } else {
    uy <- sort(unique(stats::na.omit(y)))
    if (length(uy) != 2L || !all(uy %in% c(0, 1))) stop("Numeric target must be 0/1.")
    y01 <- as.integer(y)
  }
  # Remove target and id from modeling frame
  df[[target]] <- NULL
  if (!is.null(id_col)) df[[id_col]] <- NULL
  
  .log("Target positive rate=%.4f; id_col=%s", mean(y01), ifelse(is.null(id_col), "NULL", id_col))
  
  # Types
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  num_vars <- names(df)[sapply(df, is.numeric) | sapply(df, is.integer)]
  for (v in cat_vars) {
    x <- df[[v]]
    x <- as.character(x)
    x[is.na(x) | x == ""] <- "MISSING"
    df[[v]] <- factor(x)
  }
  .log("Detected vars: numeric=%d, categorical=%d", length(num_vars), length(cat_vars))
  
  # Folds
  folds <- .stratified_folds(y01, k = n_folds, seed = seed)
  .log("Folds prepared (stratified)")
  
  # Composite features
  t1 <- Sys.time()
  composite_list <- list()
  for (v in num_vars) {
    x <- df[[v]]
    if (all(is.na(x))) next
    minx <- suppressWarnings(min(x, na.rm = TRUE))
    if (is.finite(minx) && minx >= 0) composite_list[[paste0("log1p__", v)]] <- log1p(x)
  }
  top_num <- .select_top_numeric_by_corr(df, y01, num_vars, top_k = top_num_for_pairs)
  if (length(top_num) >= 2) {
    comb <- utils::combn(top_num, 2, simplify = FALSE)
    for (p in comb) {
      a <- df[[p[1]]]; b <- df[[p[2]]]
      composite_list[[paste0("ratio__", p[1], "__over__", p[2])]] <- .safe_ratio(a, b)
      composite_list[[paste0("diff__", p[1], "__minus__", p[2])]] <- a - b
      composite_list[[paste0("prod__", p[1], "__x__", p[2])]] <- a * b
    }
  }
  if (length(num_vars) > 0) {
    composite_list[["count_missing_num"]] <- rowSums(sapply(num_vars, function(v) is.na(df[[v]])))
  }
  composite_df <- if (length(composite_list)) as.data.frame(composite_list, check.names = FALSE) else NULL
  .log("Composite: top-num=%d [%s]; features=%d; time=%.2fs",
       length(top_num), ifelse(length(top_num)>0, .headn(top_num), ""),
       ifelse(is.null(composite_df), 0L, ncol(composite_df)),
       as.numeric(difftime(Sys.time(), t1, units = "secs")))
  
  # Polynomial features (degree=2)
  t1 <- Sys.time()
  poly_df <- NULL
  if (poly_degree >= 2 && length(top_num) >= 1) {
    poly_df <- .make_poly2(df, top_num)
  }
  .log("Polynomial(d2): features=%d; time=%.2fs",
       ifelse(is.null(poly_df), 0L, ncol(poly_df)),
       as.numeric(difftime(Sys.time(), t1, units = "secs")))
  
  # rpart OOF
  rpart_oof <- NULL
  rpart_model_full <- NULL
  if (use_rpart && requireNamespace("rpart", quietly = TRUE)) {
    .log("rpart OOF: training (maxdepth=%d)", max_tree_depth)
    t1 <- Sys.time()
    rpart_oof <- rep(NA_real_, nrow(df))
    df_rp <- df
    df_rp[[target]] <- factor(y01, levels = c(0, 1))
    form <- stats::as.formula(paste(target, "~ ."))
    pb <- .make_pb(n_folds)
    for (i in seq_along(folds)) {
      te_idx <- folds[[i]]; tr_idx <- setdiff(seq_len(nrow(df)), te_idx)
      mod <- rpart::rpart(
        formula = form,
        data = df_rp[tr_idx, , drop = FALSE],
        method = "class",
        control = rpart::rpart.control(maxdepth = max_tree_depth, cp = 0.001, minsplit = 50, xval = 0)
      )
      pp <- stats::predict(mod, newdata = df_rp[te_idx, , drop = FALSE], type = "prob")
      rpart_oof[te_idx] <- pp[, 2]
      .set_pb(pb, i)
    }
    .close_pb(pb)
    rpart_model_full <- rpart::rpart(
      formula = form,
      data = df_rp,
      method = "class",
      control = rpart::rpart.control(maxdepth = max_tree_depth, cp = 0.001, minsplit = 50, xval = 0)
    )
    .log("rpart OOF done in %.2fs", as.numeric(difftime(Sys.time(), t1, units = "secs")))
  } else if (use_rpart) {
    .log("rpart not available -> skipped")
  }
  
  # ranger OOF (with median imputation per fold)
  rf_oof <- NULL
  rf_model_full <- NULL
  rf_medians_full <- NULL
  if (use_ranger && requireNamespace("ranger", quietly = TRUE)) {
    .log("ranger OOF: training (num.trees=400, respect.unordered.factors='order')")
    t1 <- Sys.time()
    rf_oof <- rep(NA_real_, nrow(df))
    df_rf <- df
    df_rf[[target]] <- factor(y01, levels = c(0, 1))
    form <- stats::as.formula(paste(target, "~ ."))
    pb <- .make_pb(n_folds)
    for (i in seq_along(folds)) {
      te_idx <- folds[[i]]; tr_idx <- setdiff(seq_len(nrow(df)), te_idx)
      
      med_tr <- vapply(num_vars, function(v) suppressWarnings(stats::median(df_rf[[v]][tr_idx], na.rm = TRUE)), numeric(1))
      names(med_tr) <- num_vars
      train_fold <- .impute_num_with(df_rf[tr_idx, , drop = FALSE], num_vars, med_tr)
      test_fold  <- .impute_num_with(df_rf[te_idx, , drop = FALSE],  num_vars, med_tr)
      
      mod <- ranger::ranger(
        formula = form,
        data = train_fold,
        num.trees = 400,
        probability = TRUE,
        classification = TRUE,
        respect.unordered.factors = "order",
        max.depth = 6,
        min.node.size = 50,
        seed = seed,
        verbose = FALSE
      )
      pp <- stats::predict(mod, data = test_fold)$predictions
      rf_oof[te_idx] <- pp[, 2]
      .set_pb(pb, i)
    }
    .close_pb(pb)
    
    rf_medians_full <- vapply(num_vars, function(v) suppressWarnings(stats::median(df_rf[[v]], na.rm = TRUE)), numeric(1))
    names(rf_medians_full) <- num_vars
    df_rf_full <- .impute_num_with(df_rf, num_vars, rf_medians_full)
    
    rf_model_full <- ranger::ranger(
      formula = form,
      data = df_rf_full,
      num.trees = 600,
      probability = TRUE,
      classification = TRUE,
      respect.unordered.factors = "order",
      max.depth = 6,
      min.node.size = 50,
      seed = seed,
      verbose = FALSE
    )
    .log("ranger OOF done in %.2fs", as.numeric(difftime(Sys.time(), t1, units = "secs")))
  } else if (use_ranger) {
    .log("ranger not available -> skipped")
  }
  
  # XGBoost OOF (GPU optional) + leaf features
  xgb_oof <- NULL
  xgb_model_full <- NULL
  xgb_terms <- NULL
  xgb_gpu_used <- FALSE
  xgb_leaf_features_train <- NULL
  xgb_leaf_meta <- NULL
  
  if (use_xgboost && requireNamespace("xgboost", quietly = TRUE)) {
    .log("XGBoost OOF: building design matrix with na.pass")
    t1 <- Sys.time()
    xgb_terms <- stats::terms(~ . - 1, data = df)
    mf_all <- stats::model.frame(xgb_terms, data = df, na.action = stats::na.pass)
    xgb_terms <- attr(mf_all, "terms")
    X_all <- stats::model.matrix(xgb_terms, mf_all)
    .log("XGBoost matrix: rows=%d, cols=%d", nrow(X_all), ncol(X_all))
    
    # OOF params
    base_param <- list(
      objective = xgb_objective,
      eval_metric = xgb_eval_metric,
      max_depth = as.integer(xgb_max_depth),
      eta = xgb_eta_oof,
      subsample = xgb_subsample_oof,
      colsample_bytree = xgb_colsample_bytree_oof,
      min_child_weight = xgb_min_child_weight,
      gamma = xgb_gamma,
      lambda = xgb_lambda,
      alpha = xgb_alpha,
      max_bin = as.integer(xgb_max_bin)
    )
    if (!is.null(xgb_params_extra)) base_param <- utils::modifyList(base_param, xgb_params_extra)
    
    # OOF predictions
    xgb_oof <- rep(NA_real_, nrow(df))
    pb <- .make_pb(n_folds)
    for (i in seq_along(folds)) {
      te_idx <- folds[[i]]; tr_idx <- setdiff(seq_len(nrow(df)), te_idx)
      dtr <- xgboost::xgb.DMatrix(data = X_all[tr_idx, , drop = FALSE], label = y01[tr_idx], missing = NA)
      tr_res <- .xgb_train(params = base_param, data = dtr, nrounds = as.integer(xgb_nrounds_oof),
                           use_gpu = xgb_use_gpu, gpu_id = xgb_gpu_id, verbose = 0)
      if (i == 1 && xgb_use_gpu) {
        if (tr_res$used_gpu) .log("XGBoost: GPU enabled (gpu_id=%d)", xgb_gpu_id) else .log("XGBoost: GPU requested but not available -> CPU fallback")
      }
      if (tr_res$used_gpu) xgb_gpu_used <- TRUE
      xdte <- xgboost::xgb.DMatrix(data = X_all[te_idx, , drop = FALSE], missing = NA)
      xgb_oof[te_idx] <- predict(tr_res$model, xdte)
      .set_pb(pb, i)
    }
    .close_pb(pb)
    
    # FULL params
    full_param <- utils::modifyList(
      base_param,
      list(
        eta = xgb_eta_full,
        subsample = xgb_subsample_full,
        colsample_bytree = xgb_colsample_bytree_full
      )
    )
    if (!is.null(xgb_params_full_extra)) full_param <- utils::modifyList(full_param, xgb_params_full_extra)
    
    # FULL train
    dall <- xgboost::xgb.DMatrix(data = X_all, label = y01, missing = NA)
    full_res <- .xgb_train(params = full_param, data = dall, nrounds = as.integer(xgb_nrounds_full),
                           use_gpu = xgb_use_gpu, gpu_id = xgb_gpu_id, verbose = 0)
    xgb_model_full <- full_res$model
    if (full_res$used_gpu) xgb_gpu_used <- TRUE
    .log("XGBoost OOF done in %.2fs", as.numeric(difftime(Sys.time(), t1, units = "secs")))
    
    # Leaf index encoding (based on FULL model)
    if (isTRUE(xgb_add_leaf_features) && !is.null(xgb_model_full)) {
      .log("XGBoost leaf features: encoding=%s", xgb_leaf_encoding)
      leaf_train <- predict(xgb_model_full, dall, predleaf = TRUE)
      if (!is.matrix(leaf_train)) leaf_train <- matrix(leaf_train, nrow = nrow(X_all))
      if (!is.null(xgb_leaf_use_first_n_trees)) {
        nkeep <- min(ncol(leaf_train), as.integer(xgb_leaf_use_first_n_trees))
        leaf_train <- leaf_train[, seq_len(nkeep), drop = FALSE]
      }
      num_trees_used <- ncol(leaf_train)
      
      if (identical(xgb_leaf_encoding, "index")) {
        colnames(leaf_train) <- sprintf("xgb_leaf_t%03d", seq_len(num_trees_used))
        xgb_leaf_features_train <- as.data.frame(leaf_train, check.names = FALSE)
        xgb_leaf_meta <- list(
          encoding = "index",
          num_trees = num_trees_used,
          use_first_n_trees = if (is.null(xgb_leaf_use_first_n_trees)) NA_integer_ else as.integer(xgb_leaf_use_first_n_trees),
          colnames = colnames(xgb_leaf_features_train),
          levels = NULL
        )
      } else {
        leaf_df <- setNames(vector("list", num_trees_used), sprintf("xgb_leaf_t%03d", seq_len(num_trees_used)))
        levels_list <- vector("list", length = num_trees_used)
        names(levels_list) <- names(leaf_df)
        for (tix in seq_len(num_trees_used)) {
          t_name <- names(leaf_df)[tix]
          levs <- sort(unique(leaf_train[, tix]))
          levels_list[[t_name]] <- levs
          leaf_df[[t_name]] <- factor(leaf_train[, tix], levels = levs)
        }
        leaf_df <- as.data.frame(leaf_df, check.names = FALSE)
        leaf_mm <- model.matrix(~ . - 1, data = leaf_df)
        xgb_leaf_features_train <- as.data.frame(leaf_mm, check.names = FALSE)
        
        xgb_leaf_meta <- list(
          encoding = "onehot",
          num_trees = num_trees_used,
          use_first_n_trees = if (is.null(xgb_leaf_use_first_n_trees)) NA_integer_ else as.integer(xgb_leaf_use_first_n_trees),
          colnames = colnames(xgb_leaf_features_train),
          levels = levels_list
        )
      }
      .log("XGBoost leaf features: trees=%d, cols=%d", num_trees_used, ncol(xgb_leaf_features_train))
    }
  } else if (use_xgboost) {
    .log("xgboost not available -> skipped")
  }
  
  # CatBoost OOF
  cat_oof <- NULL
  cat_model_full <- NULL
  cat_cat_idx <- NULL
  if (use_catboost && requireNamespace("catboost", quietly = TRUE)) {
    .log("CatBoost OOF: training")
    t1 <- Sys.time()
    cat_cat_idx <- which(names(df) %in% cat_vars) - 1L
    pb <- .make_pb(n_folds)
    for (i in seq_along(folds)) {
      te_idx <- folds[[i]]; tr_idx <- setdiff(seq_len(nrow(df)), te_idx)
      tr_pool <- catboost::catboost.load_pool(data = df[tr_idx, , drop = FALSE], label = y01[tr_idx], cat_features = cat_cat_idx)
      mod <- catboost::catboost.train(
        learn_pool = tr_pool,
        test_pool = NULL,
        params = list(
          loss_function = "Logloss",
          depth = 6,
          iterations = 500,
          learning_rate = 0.08,
          random_seed = seed,
          verbose = FALSE
        )
      )
      te_pool <- catboost::catboost.load_pool(data = df[te_idx, , drop = FALSE], cat_features = cat_cat_idx)
      pp <- catboost::catboost.predict(mod, te_pool, prediction_type = "Probability")
      if (is.null(cat_oof)) cat_oof <- rep(NA_real_, nrow(df))
      cat_oof[te_idx] <- pp
      .set_pb(pb, i)
    }
    .close_pb(pb)
    full_pool <- catboost::catboost.load_pool(data = df, label = y01, cat_features = cat_cat_idx)
    cat_model_full <- catboost::catboost.train(
      learn_pool = full_pool,
      test_pool = NULL,
      params = list(
        loss_function = "Logloss",
        depth = 6,
        iterations = 700,
        learning_rate = 0.06,
        random_seed = seed,
        verbose = FALSE
      )
    )
    .log("CatBoost OOF done in %.2fs", as.numeric(difftime(Sys.time(), t1, units = "secs")))
  } else if (use_catboost) {
    .log("catboost not available -> skipped")
  }
  
  # Combine all train features
  feature_frames <- list()
  if (!is.null(composite_df)) feature_frames <- c(feature_frames, list(composite_df))
  if (!is.null(poly_df)) feature_frames <- c(feature_frames, list(poly_df))
  if (!is.null(rpart_oof)) feature_frames <- c(feature_frames, list(data.frame(oof_rpart_prob = rpart_oof, check.names = FALSE)))
  if (!is.null(rf_oof)) feature_frames <- c(feature_frames, list(data.frame(oof_rf_prob = rf_oof, check.names = FALSE)))
  if (!is.null(xgb_oof)) feature_frames <- c(feature_frames, list(data.frame(oof_xgb_prob = xgb_oof, check.names = FALSE)))
  if (!is.null(cat_oof)) feature_frames <- c(feature_frames, list(data.frame(oof_cat_prob = cat_oof, check.names = FALSE)))
  if (!is.null(xgb_leaf_features_train)) feature_frames <- c(feature_frames, list(xgb_leaf_features_train))
  
  if (!length(feature_frames)) stop("No features were generated. Check inputs and settings.")
  train_features <- do.call(cbind, feature_frames)
  
  # Prepend ID and target to train_features
  prefix_frames <- list()
  if (!is.null(id_vec)) prefix_frames <- c(prefix_frames, list(setNames(data.frame(id_vec, check.names = FALSE), id_col)))
  prefix_frames <- c(prefix_frames, list(setNames(data.frame(as.numeric(y01), check.names = FALSE), target)))
  if (length(prefix_frames)) train_features <- do.call(cbind, c(prefix_frames, list(train_features)))
  
  .log("Assembled train features: cols=%d (with ID/target if provided)", ncol(train_features))
  .log("Done in %.2fs", as.numeric(difftime(Sys.time(), t0, units = "secs")))
  
  # Scoring function for new data (keeps ID; no target in output)
  predict_new <- function(new_df) {
    nd <- new_df
    new_frames <- list()
    
    # Keep ID if present
    if (!is.null(id_col) && id_col %in% names(nd)) {
      new_frames <- c(new_frames, list(setNames(data.frame(nd[[id_col]], check.names = FALSE), id_col)))
    }
    
    # Prepare categoricals - handle new levels by converting to "MISSING"
    for (v in cat_vars) {
      if (!v %in% names(nd)) next
      x <- nd[[v]]
      x <- as.character(x)
      x[is.na(x) | x == ""] <- "MISSING"
      
      # Get training levels for this variable
      if (v %in% names(df)) {
        train_levels <- levels(df[[v]])
        # Convert new levels to "MISSING"
        x[!x %in% train_levels] <- "MISSING"
      }
      
      nd[[v]] <- factor(x, levels = if (v %in% names(df)) levels(df[[v]]) else NULL)
    }
    
    # Composite
    comp_new <- list()
    for (v in num_vars) {
      if (!v %in% names(nd)) next
      x <- nd[[v]]
      minx <- suppressWarnings(min(x, na.rm = TRUE))
      if (is.finite(minx) && minx >= 0) comp_new[[paste0("log1p__", v)]] <- log1p(x)
    }
    if (length(top_num) >= 2) {
      comb <- utils::combn(top_num, 2, simplify = FALSE)
      for (p in comb) {
        if (!all(p %in% names(nd))) next
        a <- nd[[p[1]]]; b <- nd[[p[2]]]
        comp_new[[paste0("ratio__", p[1], "__over__", p[2])]] <- .safe_ratio(a, b)
        comp_new[[paste0("diff__", p[1], "__minus__", p[2])]] <- a - b
        comp_new[[paste0("prod__", p[1], "__x__", p[2])]] <- a * b
      }
    }
    if (length(num_vars) > 0) {
      comp_new[["count_missing_num"]] <- rowSums(sapply(num_vars, function(v) if (v %in% names(nd)) is.na(nd[[v]]) else rep(TRUE, nrow(nd))))
    }
    if (length(comp_new)) new_frames <- c(new_frames, list(as.data.frame(comp_new, check.names = FALSE)))
    
    # Polynomial
    if (poly_degree >= 2 && length(top_num) >= 1 && all(top_num %in% names(nd))) {
      poly_new_df <- .make_poly2(nd, top_num)
      new_frames <- c(new_frames, list(poly_new_df))
    }
    
    # Model-based predictions
    if (!is.null(rpart_model_full)) {
      nd_rp <- nd; nd_rp[[target]] <- factor(0, levels = c(0, 1))
      # Ensure all categorical variables have the same levels as in training
      for (v in cat_vars) {
        if (v %in% names(nd_rp) && v %in% names(df)) {
          nd_rp[[v]] <- factor(nd_rp[[v]], levels = levels(df[[v]]))
        }
      }
      pp <- stats::predict(rpart_model_full, newdata = nd_rp, type = "prob")
      new_frames <- c(new_frames, list(data.frame(oof_rpart_prob = pp[, 2], check.names = FALSE)))
    }
    if (!is.null(rf_model_full)) {
      nd_rf <- nd; nd_rf[[target]] <- factor(0, levels = c(0, 1))
      # Ensure all categorical variables have the same levels as in training
      for (v in cat_vars) {
        if (v %in% names(nd_rf) && v %in% names(df)) {
          nd_rf[[v]] <- factor(nd_rf[[v]], levels = levels(df[[v]]))
        }
      }
      nd_rf <- .impute_num_with(nd_rf, num_vars, rf_medians_full)
      pp <- stats::predict(rf_model_full, data = nd_rf)$predictions
      new_frames <- c(new_frames, list(data.frame(oof_rf_prob = pp[, 2], check.names = FALSE)))
    }
    if (!is.null(xgb_model_full) && !is.null(xgb_terms)) {
      mf_new <- stats::model.frame(xgb_terms, data = nd, na.action = stats::na.pass)
      X_new <- stats::model.matrix(xgb_terms, mf_new)
      dnew <- xgboost::xgb.DMatrix(data = X_new, missing = NA)
      pp <- predict(xgb_model_full, dnew)
      new_frames <- c(new_frames, list(data.frame(oof_xgb_prob = pp, check.names = FALSE)))
      
      # Leaf features for new data
      if (isTRUE(xgb_add_leaf_features)) {
        leaf_new <- predict(xgb_model_full, dnew, predleaf = TRUE)
        if (!is.matrix(leaf_new)) leaf_new <- matrix(leaf_new, nrow = nrow(X_new))
        
        # apply same tree limit as in train
        if (!is.null(xgb_leaf_meta$use_first_n_trees) && !is.na(xgb_leaf_meta$use_first_n_trees)) {
          nkeep <- min(ncol(leaf_new), as.integer(xgb_leaf_meta$use_first_n_trees))
          leaf_new <- leaf_new[, seq_len(nkeep), drop = FALSE]
        }
        
        if (identical(xgb_leaf_meta$encoding, "index")) {
          colnames(leaf_new) <- xgb_leaf_meta$colnames
          new_frames <- c(new_frames, list(as.data.frame(leaf_new, check.names = FALSE)))
        } else if (identical(xgb_leaf_meta$encoding, "onehot")) {
          leaf_df <- setNames(vector("list", length(xgb_leaf_meta$levels)), names(xgb_leaf_meta$levels))
          for (t_name in names(leaf_df)) {
            t_ix <- as.integer(sub("^xgb_leaf_t0*", "", t_name))
            if (!is.na(t_ix) && t_ix <= ncol(leaf_new)) {
              # Handle new levels by using first training level
              leaf_values <- leaf_new[, t_ix]
              train_levels <- xgb_leaf_meta$levels[[t_name]]
              # Replace new levels with first training level
              leaf_values[!leaf_values %in% train_levels] <- train_levels[1]
              leaf_df[[t_name]] <- factor(leaf_values, levels = train_levels)
            } else {
              leaf_df[[t_name]] <- factor(rep(xgb_leaf_meta$levels[[t_name]][1], nrow(leaf_new)), levels = xgb_leaf_meta$levels[[t_name]])
            }
          }
          leaf_df <- as.data.frame(leaf_df, check.names = FALSE)
          leaf_mm_new <- model.matrix(~ . - 1, data = leaf_df)
          leaf_mm_new_df <- as.data.frame(leaf_mm_new, check.names = FALSE)
          
          # align columns to training onehot
          missing_cols <- setdiff(xgb_leaf_meta$colnames, colnames(leaf_mm_new_df))
          for (mc in missing_cols) leaf_mm_new_df[[mc]] <- 0
          leaf_mm_new_df <- leaf_mm_new_df[, xgb_leaf_meta$colnames, drop = FALSE]
          new_frames <- c(new_frames, list(leaf_mm_new_df))
        }
      }
    }
    if (!is.null(cat_model_full)) {
      # Ensure all categorical variables have the same levels as in training
      nd_cat <- nd
      for (v in cat_vars) {
        if (v %in% names(nd_cat) && v %in% names(df)) {
          nd_cat[[v]] <- factor(nd_cat[[v]], levels = levels(df[[v]]))
        }
      }
      np <- catboost::catboost.load_pool(data = nd_cat, cat_features = cat_cat_idx)
      pp <- catboost::catboost.predict(cat_model_full, np, prediction_type = "Probability")
      new_frames <- c(new_frames, list(data.frame(oof_cat_prob = pp, check.names = FALSE)))
    }
    
    if (!length(new_frames)) stop("No features generated for new data.")
    out_df <- do.call(cbind, new_frames)
    
    # Align to training columns excluding target
    train_cols <- colnames(train_features)
    align_cols <- setdiff(train_cols, target)
    missing_cols <- setdiff(align_cols, colnames(out_df))
    for (mc in missing_cols) out_df[[mc]] <- NA_real_
    out_df <- out_df[, align_cols, drop = FALSE]
    out_df
  }
  
  list(
    train_features = train_features,  # includes ID (if provided) and target as first columns
    meta = list(
      target = target,
      num_vars = num_vars,
      cat_vars = cat_vars,
      top_num_for_pairs = top_num,
      xgb_terms = xgb_terms,
      cat_cat_idx = cat_cat_idx,
      rf_num_medians = rf_medians_full,
      xgb_gpu_used = xgb_gpu_used,
      xgb_gpu_id = if (isTRUE(xgb_gpu_used)) xgb_gpu_id else NA_integer_,
      id_col = id_col,
      xgb_leaf_meta = xgb_leaf_meta
    ),
    models = list(
      rpart = rpart_model_full,
      rf = rf_model_full,
      xgb = xgb_model_full,
      cat = cat_model_full
    ),
    predict_new = predict_new
  )
}