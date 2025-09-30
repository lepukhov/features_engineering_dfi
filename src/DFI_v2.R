library('readr')
library('lubridate')
library('dplyr')
library('data.table')
library('ggplot2')
library('stringr')
library('rsample')
library('scorecard')
library('ModelMetrics')
library('modelsummary')
library('tidypredict')
library('precrec')
library('caret')
library('tornado')
library('tidyverse')
library('bootStepAIC')
library('readxl')
library('glmnet')
library('openxlsx')
library('doParallel')
library('foreach')
library('car')

source(file.path('src','FeatureGenerator.R'))
source(file.path('src','scoring_report_wo_initial.R'))
source(file.path('src','LLMFeatureGenerator.R'))
source(file.path('src','calculate_psi.R'))
source('config.R')

# Runtime options
VERBOSE <- get0('VERBOSE', ifnotfound = FALSE)

# Data paths
factors_path <- file.path(DATA_DIR, INPUT_FACTORS_FILE)
juicy_path <- file.path(DATA_DIR, JUICY_FILE)
result_path <- file.path(DATA_DIR, RESULT_FILE)

all_loans <- readr::read_delim(factors_path, quote = "\"", delim =';', locale = readr::locale(encoding = "Windows-1251"), show_col_types = FALSE, progress = FALSE)
juicy <- openxlsx::read.xlsx(juicy_path)
result <- openxlsx::read.xlsx(result_path)

all_loans$loan_id <- gsub("[^[:alnum:].]", "", all_loans$loan_id)
all_loans$loan_id <- as.double(all_loans$loan_id)

juicy$loan_id <- gsub("[^[:alnum:].]", "", juicy$loan_id)
juicy$loan_id <- as.double(juicy$loan_id)

all_loans <- all_loans %>% left_join(result %>% dplyr::select(loan_id, DPD30_factor))

N_CORES <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
cl <- NULL
if (N_CORES > 1L) {
  cl <- parallel::makeCluster(N_CORES)
  doParallel::registerDoParallel(cl)
}

if (!is.null(cl)) on.exit({ try(parallel::stopCluster(cl), silent = TRUE) }, add = TRUE)

if (VERBOSE) glimpse(all_loans)
all_loans_a <- all_loans
all_loans_a <- all_loans_a %>% mutate(across(where(is.character), ~ gsub(",", ".", .)))
all_loans_a <- all_loans_a %>% mutate(across(where(is.character), ~  gsub("[^[:alnum:].]", "", .)))
all_loans_a$total_score <- as.numeric(all_loans_a$total_score)
all_loans_a <- all_loans_a %>% filter(!is.na(DPD30_factor))
all_loans_a$DPD30_factor <- as.double(all_loans_a$DPD30_factor)
all_loans_a$total_score <- as.numeric(all_loans_a$total_score)
cutoff_all_loans <- max(all_loans_a[[loan_date]]) - months(oot_period_months)
all_loans_a_init <- all_loans_a %>% filter(issuedate < cutoff_all_loans)
all_loans_a_oot <- all_loans_a %>%filter(issuedate >= cutoff_all_loans)

initial_gini <- gini(all_loans_a_init[[target]],all_loans_a_init$total_score)
initial_oot_gini <- gini(all_loans_a_oot[[target]],all_loans_a_oot$total_score)

factors_first_portion = names(all_loans)[24:142]
dt <- all_loans %>% dplyr::select(any_of(c(id,target, loan_date,factors_first_portion))) %>% left_join(juicy)

dt <- dt %>% filter(dt[[loan_date]] <= OOT_CUTOFF_DATE)
if (VERBOSE) glimpse(dt)

dt <- dt %>% filter(!is.na(DPD30_factor))
dt$DPD30_factor <- as.double(dt$DPD30_factor)

filter <- c('client_id', 'timeLocal', 'FPD30')
baseline <- names(dt)[!(names(dt) %in% filter)]
dt <- dt %>% dplyr::select(any_of(c(baseline)))

dt <- dt %>% mutate(across(where(is.character), ~ gsub(",", ".", .)))
dt <- dt %>% mutate(across(where(is.character), ~  gsub("[^[:alnum:].]", "", .)))

replace_zero_with_na_in_categoricals <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data.frame or tibble")
  data[] <- lapply(data, function(col) {
    if (is.factor(col)) {
      old_levels <- levels(col)
      col_char <- as.character(col)
      idx <- trimws(col_char) == "0"
      if (any(idx)) {
        col_char[idx] <- NA_character_
      }
      zero_levels <- old_levels[trimws(old_levels) == "0"]
      new_levels <- setdiff(old_levels, zero_levels)
      factor(col_char, levels = new_levels, ordered = is.ordered(col))
    } else if (is.character(col)) {
      idx <- trimws(col) == "0"
      if (any(idx)) {
        col[idx] <- NA_character_
      }
      col
    } else {
      col
    }
  })
  data
}
dt <- replace_zero_with_na_in_categoricals(dt)

coerce_numeric_cols <- function(df) {
  convert_col <- function(x) {
    if (is.numeric(x)) return(x)
    if (is.logical(x)) return(as.numeric(x))
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(x)
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x_trim <- trimws(x)
      x_trim[x_trim == ""] <- NA_character_
      suppressWarnings(x_num <- as.numeric(x_trim))
      non_empty <- !is.na(x_trim)
      if (all(!is.na(x_num[non_empty]))) return(x_num)
    }
    x
  }
  df[] <- lapply(df, convert_col)
  df
}
dt <- coerce_numeric_cols(dt)

cutoff_dt <- max(dt[[loan_date]]) - months(oot_period_months)
dt_initial <- dt %>% filter(issuedate < cutoff_dt)
dt_oot <- dt %>% filter(issuedate >= cutoff_dt)
dt_list = split_df(dt_initial, y=target, ratios = c(ratio_train, 1 - ratio_train), seed = 14)

join_if_exists <- function(base_df, dict_path, key_col) {
  if (!file.exists(dict_path)) {
    if (VERBOSE) message("Файл словаря не найден: ", dict_path)
    return(base_df)
  }
  dict <- tryCatch(readr::read_csv(dict_path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(dict) || !key_col %in% names(dict)) {
    if (VERBOSE) message("Словарь невалиден или не содержит столбец ", key_col, ": ", dict_path)
    return(base_df)
  }
  dict[[key_col]] <- gsub(",", ".", dict[[key_col]])
  dict[[key_col]] <- gsub("[^[:alnum:].]", "", dict[[key_col]])
  dplyr::left_join(base_df, dict, join_by(!!as.name(key_col)))
}

dt_list$train <- dt_list$train %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPCITY_FILE), 'ipCity') %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPOWNER_FILE), 'ipOwner') %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPREGION_FILE), 'ipRegionName')

dt_list$test <- dt_list$test %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPCITY_FILE), 'ipCity') %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPOWNER_FILE), 'ipOwner') %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPREGION_FILE), 'ipRegionName')

dt_oot <- dt_oot %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPCITY_FILE), 'ipCity') %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPOWNER_FILE), 'ipOwner') %>%
  join_if_exists(file.path(DATA_DIR, DICT_IPREGION_FILE), 'ipRegionName')

filter <- c('ipCity', 'ipOwner', 'ipRegionName')

baseline_llm <- names(dt_list$train)[!(names(dt_list$train) %in% filter)]
dt_list$train <- dt_list$train %>% dplyr::select(any_of(c(baseline_llm)))
dt_list$test <- dt_list$test %>% dplyr::select(any_of(c(baseline_llm)))
dt_oot <- dt_oot %>% dplyr::select(any_of(c(baseline_llm)))

fe <- engineer_features_train(dt_list$train,
                              target = target,
                              top_num_for_pairs = top_num_for_pairs,
                              id_col = id,
                              poly_degree = 2,
                              use_catboost = use_catboost,
                              use_ranger = use_ranger,
                              use_rpart = use_rpart,
                              use_xgboost = use_xgboost,
                              xgb_add_leaf_features = xgb_add_leaf_features,
                              xgb_leaf_encoding = "onehot",
                              xgb_leaf_use_first_n_trees = xgb_leaf_use_first_n_trees,
                              n_folds = n_folds)

dt_train_enriched <- fe$train_features
dt_train_enriched <- dt_train_enriched %>% dplyr::select(!any_of(c(target)))
dt_train_enriched <- dt_list$train %>% left_join(dt_train_enriched, join_by(loan_id))

dt_test_enriched <- fe$predict_new(dt_list$test)
dt_test_enriched <- dt_list$test %>% left_join(dt_test_enriched, join_by(loan_id))

dt_oot_enriched <- fe$predict_new(dt_oot)
dt_oot_enriched <- dt_oot %>% left_join(dt_oot_enriched, join_by(loan_id))

if (VERBOSE) glimpse(dt_oot_enriched)

data_train <- rbind(dt_train_enriched,dt_test_enriched)
dt_list$train = dt_train_enriched
dt_list$test = dt_test_enriched

drop_info_df <- data.frame(
  Var = character(),
  Reason = character(),
  stringsAsFactors = FALSE
)

bins = woebin(dt_train_enriched, y=c(target), check_cate_num = FALSE, method = 'tree', stop_limit = stop_limit, count_distr_limit = count_distr_limit, no_cores = NULL, bin_num_limit = bin_num_limit)
dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))

dt_woe_list$train = var_filter(dt_woe_list$train, y=c(target) ,lims = list(missing_rate = missing_rate, identical_rate = identical_rate, info_value = info_value_cutoff))

drop_info_var_filter <- setdiff(colnames(dt_train_enriched), sub("_woe$", "", colnames(dt_woe_list$train)))
if (length(drop_info_var_filter) > 0) {
drop_info_df <- rbind(drop_info_df, data.frame(Var = drop_info_var_filter, Reason = 'IV, Missing Rate, Identity Filter'))
}  

m1 = glm(as.formula(paste(target,' ~ .')), family = binomial(), data = dt_woe_list$train)

cat('\n Stage 1 \n', 'Features:', length(names(m1$model)), 'GINI:', gini(dt_woe_list$train[[target]],predict(m1, dt_woe_list$train)),
gini(dt_woe_list$test[[target]],predict(m1, dt_woe_list$test)), '\n')

vars <- dt_woe_list$train %>% dplyr::select(where(is.numeric))
vars <- vars %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
vars <- vars %>% dplyr::select(!(names(vars)[findLinearCombos(vars)$remove]))
vars <- vars %>% dplyr::select(!(names(vars)[findCorrelation(cor(vars), cutoff = correlation_cutoff)]))
dt_woe_list$train <- dt_woe_list$train %>% dplyr::select(names(vars))

drop_info_corr <- setdiff(colnames(dt_train_enriched), c(sub("_woe$", "",colnames(dt_woe_list$train)), drop_info_df$Var))
if (length(drop_info_corr) > 0) {
drop_info_df <- rbind(drop_info_df, data.frame(Var = drop_info_corr, Reason = 'Correlation Filter'))
}

m1_1 = glm(as.formula(paste(target,' ~ .')), family = binomial(), data = vars)
cat('\n Stage 2 \n', 'Features:', length(names(m1_1$model)), 'GINI:', gini(dt_woe_list$train[[target]],predict(m1_1, dt_woe_list$train)),
    gini(dt_woe_list$test[[target]],predict(m1_1, dt_woe_list$test)), '\n')

vifs <- vif(m1_1)
vifs <- tibble(name = names(vifs), gvif = vifs)
vifs <- vifs %>% dplyr::filter( gvif > gvif_cutoff) %>% dplyr::select(name)
vars <- vars %>% dplyr::select(!any_of(vifs$name))
m1_1 = glm(as.formula(paste(target,' ~ .')), family = binomial(), data = vars)
cat('\n Stage 3 \n', 'Features:', length(names(m1_1$model)), 'GINI:', gini(dt_woe_list$train[[target]],predict(m1_1, dt_woe_list$train)),
    gini(dt_woe_list$test[[target]],predict(m1_1, dt_woe_list$test)), '\n')

selected_vifs <- coef(m1_1)
selected_features_vifs <- names(selected_vifs)
selected_features_vifs <- selected_features_vifs[-1]
dt_woe_list$train <- dt_woe_list$train %>% dplyr::select(any_of(c(selected_features_vifs,target)))

drop_info_VIF <- setdiff(colnames(dt_train_enriched), c(sub("_woe$", "",colnames(dt_woe_list$train)), drop_info_df$Var))
if (length(drop_info_VIF) > 0) {
drop_info_df <- rbind(drop_info_df, data.frame(Var = drop_info_VIF, Reason = 'VIF Filter'))
}


x <- model.matrix(as.formula(paste(target, '~ .')), data = dt_woe_list$train)[, -1]
y <- dt_woe_list$train[[target]]

alphas <- seq(0, 1, by = 0.01)
cv_errors <- numeric(length(alphas))
for (i in seq_along(alphas)) {
  cv_model <- cv.glmnet(x, y, family = "binomial", alpha = alphas[i], nfolds = cv_nfolds, type.measure = "auc")
  cv_errors[i] <- min(cv_model$cvm)
}
best_alpha <- alphas[which.min(cv_errors)]

cv_elastic_net <- cv.glmnet(
  x = x, y = y,
  family = "binomial",
  alpha = best_alpha,
  nfolds = cv_nfolds,
  type.measure = "auc",
  #parallel = !is.null(cl)
)
best_lambda <- cv_elastic_net$lambda.min

lasso_model <- glmnet(x, y, alpha = best_alpha, lambda = best_lambda, family = "binomial")
selected <- coef(lasso_model)
selected <- selected[selected[,1] != 0, ]
selected_features <- names(selected)
selected_features <- selected_features[-1]
dt_woe_list$train <- dt_woe_list$train %>% dplyr::select(any_of(c(selected_features, target)))

drop_info_lasso <- setdiff(colnames(dt_train_enriched), c(sub("_woe$", "", colnames(dt_woe_list$train)), drop_info_df$Var))
if (length(drop_info_lasso) > 0) {
drop_info_df <- rbind(drop_info_df, data.frame(Var = drop_info_lasso, Reason = 'Lasso Filter'))
}

m2_1 = glm(as.formula(paste(target,' ~ .')), family = binomial(), data = dt_woe_list$train)
cat('\n Stage 4 \n', 'Features:', length(names(m2_1$model)), 'GINI:', gini(dt_woe_list$train[[target]],predict(m2_1, dt_woe_list$train)),
    gini(dt_woe_list$test[[target]],predict(m2_1, dt_woe_list$test)), '\n')

choose_B <- function(n_vars) {
  if (n_vars <= 20) {
    return(100)
  } else if (n_vars <= 50) {
    return(200)
  } else if (n_vars <= 100) {
    return(300)
  } else {
    return(500)
  }
}

boot2_1 <- boot.stepAIC(
  m2_1,
  dt_woe_list$train,
  B = choose_B(ncol(dt_woe_list$train)-1),
  direction = "both",
  parallel = !is.null(cl),
  ncpus = if (!is.null(cl)) N_CORES else 1,
  verbose = VERBOSE
)

selected_boot <- boot2_1$OrigStepAIC$coefficients
selected_boot <- names(selected_boot)
selected_boot <- selected_boot[-1]

dt_woe_list$train <- dt_woe_list$train %>% dplyr::select(any_of(c(selected_boot,target)))

drop_info_boot <- setdiff(colnames(dt_train_enriched), c(sub("_woe$", "",colnames(dt_woe_list$train)), drop_info_df$Var))
if (length(drop_info_boot) > 0) {
drop_info_df <- rbind(drop_info_df, data.frame(Var = drop_info_boot, Reason = 'Bootstraping Filter'))
}

m3_1 = glm(as.formula(boot2_1$OrigStepAIC$formula), family = binomial(), data = dt_woe_list$train)
cat('\n Stage 5 \n', 'Features:', length(names(m3_1$model)), 'GINI:', gini(dt_woe_list$train[[target]],predict(m3_1, dt_woe_list$train)),
    gini(dt_woe_list$test[[target]],predict(m3_1, dt_woe_list$test)), '\n')


cat('\n Initial GINI:', initial_gini,' - vs -','Final Model GINI:',gini(dt_woe_list$train[[target]],predict(m3_1, dt_woe_list$train)), '/', gini(dt_woe_list$test[[target]],predict(m3_1, dt_woe_list$test)))

final_model <- m3_1
final_model_features <-  sub("_woe$", "", names(m3_1$model))
final_bins <- bins[names(bins) %in% final_model_features]

dt_initial <- dt_train_enriched
dt_final <- split_df(dt_train_enriched %>% dplyr::select(any_of(c(final_model_features, target, loan_date))), y=target, ratios = c(ratio_train, 1 - ratio_train), seed = 14)
dt_final$oot <- dt_oot_enriched %>% dplyr::select(any_of(c(final_model_features, target, loan_date)))

dt_enriched_woe <- woebin_ply(dt_train_enriched %>% dplyr::select(!any_of(c(id))),bins = bins)
dt_enriched_describe <- scorecard::describe(dt_enriched_woe %>% dplyr::select(!any_of(c(loan_date, id, target))))
dt_enriched_iv <- scorecard::iv(dt = dt_enriched_woe %>% dplyr::select(!any_of(c(loan_date, id))) , y = c(target))
dt_enriched_psi <- calculate_psi(df = dt_enriched_woe, target_col = target, date_col = loan_date)

dt_enriched_info <- dt_enriched_describe %>% left_join(dt_enriched_iv)  %>% left_join(dt_enriched_psi)
drop_info_df_final <- drop_info_df
drop_info_df_final$Var <- paste0(drop_info_df_final$Var, "_woe")
names(drop_info_df_final)[1] = 'variable'
dt_enriched_info <- dt_enriched_info %>% left_join(drop_info_df_final)
#readr::write_csv(dt_enriched_info, file.path(project_directory ,paste0("dt_enriched_info_", format(Sys.time(), "%d%m%Y%H%M%S") ,".csv")))

export_default_model_report_excel(
  model = final_model,
  train = dt_final$train,
  test  = dt_final$test,
  oot   = dt_final$oot,
  target_col = target,
  date_col   = loan_date,
  bins = final_bins,
  feature_cols = names(final_bins),
  excel_path = file.path(project_directory, paste0("model_report_", format(Sys.time(), "%d%m%Y%H%M%S") ,".xlsx")),
  dt_enriched_info = dt_enriched_info,
  overwrite = TRUE
)

if (!is.null(cl)) {
  parallel::stopCluster(cl)
}


