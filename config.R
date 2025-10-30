# Конфигурация проекта DFI

# Пути
DATA_DIR <- file.path(getwd(), 'data')
OUTPUT_DIR <- file.path(getwd(), 'output')
project_directory = OUTPUT_DIR

# Имена входных файлов (положите их в папку data/)
INPUT_FACTORS_FILE <- 'df_mx8_2.csv'
SKIP_FACTORS <- c('client_ID', 'uuid','issueDate','FPDvalues','score_pred_CATBOOST_MX8','id_next','isnext_DPD30')

# Рантайм настройки
VERBOSE <- TRUE
OOT_CUTOFF_DATE <- as.Date('2025-09-01') #значения старше этой даты будут выкидываться

#Настройки базовые
target = 'isDPD30' #поле где содержится целевая переменная в формате 0/1
id = 'loan_ID' #поле в котором содержится loan_id
loan_date = 'Idate' #поле в котором содержится дата 
ratio_train = 0.7 #проценты обучающей выборки (за искл. oot)
OOT_SHARE = 0.1 #доля выборки которая уйдет на oot

#PSI конфигурация
n_intervals = 6 #количество интвервалов
interval_method = 'quantile'#метод "quantile", либо "equal_days". Делит на интервалы процентильно либо по равному количеству дней.
baseline_interval = 'last' #интервал с которым происходит сравнение "first"/"last"/номер/"Interval_k"

#Feature Engeneering Settings
top_num_for_pairs = 15 #количество топ-переменных которые будут попарно взаимодействия, выбираются по спирману
use_catboost = FALSE #использовать катбус или нет (нужно сначала установить и проверить катбус вручную)
use_ranger = FALSE #использовать decision tree или нет
use_rpart = FALSE #использовать random forest или нет
use_xgboost = FALSE #использовать xgboost или нет
xgb_add_leaf_features = FALSE # использовать листья xgboost в качестве one-hot переменной или нет
xgb_leaf_use_first_n_trees = 20 # количество первых деревьев
n_folds = 5 #количество фолдов
fe_nfolds = 5

# Тогглы для пайплайна признаков/комбинаций WOE
use_engineer_features = FALSE   # использовать engineer_features_train (если FALSE, работаем только с базовыми полями)
use_woe_combos = TRUE          # добавлять комбинированные WOE-признаки (если FALSE, пропускаем build/add_woe_cross_features)

#комбинирование переменных
top_num_for_combos = 20
max_num_woe_combos = 200
woe_combo_exclude_vars = NULL

#гиперпараметры для catboost
cat_loss_function = "AUC"
cat_depth = 4L
cat_iterations_oof = 500L
cat_iterations_full = 700L
cat_learning_rate_oof = 0.08
cat_learning_rate_full = 0.06

#гиперпараметры xgboost
# XGBoost hyperparams (OOF)
xgb_objective = "binary:logistic"
xgb_eval_metric = "logloss"
xgb_max_depth = 4L
xgb_eta_oof = 0.10
xgb_subsample_oof = 0.80
xgb_colsample_bytree_oof = 0.80
xgb_min_child_weight = 8
xgb_gamma = 0
xgb_lambda = 1
xgb_alpha = 0
xgb_max_bin = 256L
xgb_nrounds_oof = 220L
xgb_params_extra = NULL

# XGBoost hyperparams (FULL)
xgb_eta_full = 0.08
xgb_subsample_full = 0.85
xgb_colsample_bytree_full = 0.85
xgb_nrounds_full = 320L
xgb_params_full_extra = NULL


#Binning Settings
bin_num_limit = 6 #максимальное количество биннингов для каждой из переменных
count_distr_limit = 0.05 #минимальное значение распределения для каждого из биннинга
stop_limit = 0.1


#Filtering Settings
info_value_cutoff = 0.02 #Фильтр по IV
missing_rate = 0.5 #фильтр по доле пропусков
identical_rate = 0.9 #фильтр по идентичности
cv_nfolds = 5 #количество фолдов, которое используется при L1 регуляризации
correlation_cutoff = 0.7 #фильтр по корреляции
gvif_cutoff = 5 #фильтр по мультколлинеарности