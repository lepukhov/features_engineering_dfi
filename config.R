# Конфигурация проекта DFI

# Пути
DATA_DIR <- file.path(getwd(), 'data')
OUTPUT_DIR <- file.path(getwd(), 'output')
project_directory = OUTPUT_DIR

# Имена входных файлов (положите их в папку data/)
INPUT_FACTORS_FILE <- 'factors_first_portion.csv'
SKIP_FACTORS <- c('true_caller_city', 'whatsapp_about')

#только для v2
JUICY_FILE <- 'Juicy.xlsx'
RESULT_FILE <- 'Result.xlsx'
# Словари для LLM-атрибутов (опционально, если есть)
DICT_IPOWNER_FILE <- 'dict_ipOwner_enrichment.csv'
DICT_IPREGION_FILE <- 'dict_ipRegionName_enrichment.csv'
DICT_IPCITY_FILE <- 'dict_ipCity_enrichment.csv'

# Рантайм настройки
VERBOSE <- TRUE
OOT_CUTOFF_DATE <- as.Date('2025-10-01') #значения старше этой даты будут выкидываться

#Настройки базовые
target = 'target' #поле где содержится целевая переменная в формате 0/1
id = 'loan_id' #поле в котором содержится loan_id
loan_date = 'loan_date' #поле в котором содержится дата 
ratio_train = 0.7 #проценты обучающей выборки (за искл. oot)
oot_period_months = 1 #количество месяцевы, которые попадют в out-of-time

#Feature Engeneering Settings
top_num_for_pairs = 5 #количество топ-переменных которые будут попарно взаимодействия, выбираются по спирману
use_catboost = FALSE #использовать катбус или нет (нужно сначала установить и проверить катбус вручную)
use_ranger = TRUE #использовать decision tree или нет
use_rpart = TRUE #использовать random forest или нет
use_xgboost = FALSE #использовать xgboost или нет
xgb_add_leaf_features = FALSE # использовать листья xgboost в качестве one-hot переменной или нет
xgb_leaf_use_first_n_trees = 50 # количество первых деревьев
n_folds = 5 #количество фолдов
fe_nfolds = 5

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
