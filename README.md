## DFI Project

Структура проекта:
- `run.R` — точка входа. Запускает полный процесс.
- `config.R` — конфигурация путей и параметров (редактируйте под себя).
- `src/` — исходники:
  - `DFI_v2.R` — основной скрипт (адаптирован под относительные пути).
  - `FeatureGenerator.R` — скрипт для функции генератора фичей
  - `LLMFeatureGenerator.R` — скрипт для использовнаия LLM для фича инжиниринга
  - `scoring_report_wo_initial.R` — скрипт для экспорт отчета в Excel
  - `calculate_psi.R` — подключает PSI из корня проекта (при необходимости замените).
- `data/` — входные данные (положите сюда свои файлы):
- `output/` — результаты (создаётся автоматически).

Запуск:

1. Скопируйте входные файлы в папку `data/`.
2. Откройте R/RStudio в папке `dfi_project/`.
3. Выполните:

```r
source('run.R')
```

Зависимости:

Скрипт сам проверит и установит необходимые пакеты при первом запуске.

Фичи:

Фиксация seed, стратифицированные фолды по таргету
Очистка категориальных: NA/"" → "MISSING"
Композитные признаки:
  
log1p для неотрицательных числовых: log1p__<var>
Для топ‑K числовых (по |Spearman| с таргетом) создаются:
- отношения: ratio__A__over__B
- разности: diff__A__minus__B
- произведения: prod__A__x__B

Счётчик пропусков в числовых: count_missing_num

Полиномиальные признаки (degree=2) только для выбранных топ‑K числовых
квадраты: poly2__<var>__sq
попарные произведения: poly2__A__x__B


Модельные OOF‑признаки (без утечки):
- rpart: oof_rpart_prob
- ranger (случайный лес): oof_rf_prob
- XGBoost: oof_xgb_prob (+ опционально листовые признаки)
- CatBoost: oof_cat_prob

OOF-логика: для каждого фолда обучается модель на трен‑части и предсказывается hold‑out фолд; склейка даёт полный OOF-вектор.
Обучение финальных полных моделей (на всей train выборке) для последующего скоринга:

сохраняются в result$models
XGBoost leaf index encoding (опционально):
На полной XGBoost‑модели извлекаются индексы листьев (predleaf=TRUE)

Режимы:
index: столбцы xgb_leaf_tNNN с номерами листьев (целые)
onehot: one‑hot матрица листьев; уровни сохраняются в meta$xgb_leaf_meta для корректного скоринга
Можно ограничить число используемых деревьев (первые N)


Как потом можно использовать результаты?

Коротко: загрузите сохранённые файлы (список моделей и метаданные) и используйте их для скоринга новых данных по каждому алгоритму.
# 1) Загрузка
dir <- file.path(project_directory, "fe_models_YYYYMMDDHHMMSS")  # ваша папка
fe_models <- readRDS(file.path(dir, "fe_models_list.rds"))
rf_medians <- try(readRDS(file.path(dir, "ranger_num_medians.rds")), silent = TRUE)
xgb_terms  <- try(readRDS(file.path(dir, "xgb_terms.rds")), silent = TRUE)
xgb_leaf_meta <- try(readRDS(file.path(dir, "xgb_leaf_meta.rds")), silent = TRUE)

# Доступ к моделям
#rpart
m_rpart <- fe_models$rpart
m_rf    <- fe_models$rf
m_xgb   <- fe_models$xgb
m_cat   <- fe_models$cat

#Скоринг новых сырых данных nd (те же названия/типы столбцов, что и при обучении):
nd_rp <- nd
nd_rp[[target]] <- factor(0, levels = c(0,1))
p_rpart <- predict(m_rpart, newdata = nd_rp, type = "prob")[,2]

#ranger (импутация медиа́нами из train)
nd_rf <- nd
if (inherits(rf_medians, "numeric")) {
  for (v in names(rf_medians)) if (v %in% names(nd_rf)) {
    x <- nd_rf[[v]]; x[is.na(x)] <- rf_medians[[v]]; nd_rf[[v]] <- x
  }
}
nd_rf[[target]] <- factor(0, levels = c(0,1))
p_rf <- predict(m_rf, data = nd_rf)$predictions[,2]

#xgboost (тот же terms, что в train)
stopifnot(!inherits(xgb_terms, "try-error"))
mf <- model.frame(xgb_terms, data = nd, na.action = na.pass)
X  <- model.matrix(xgb_terms, mf)
d  <- xgboost::xgb.DMatrix(X, missing = NA)
p_xgb <- predict(m_xgb, d)
# Если нужны листовые фичи — используйте сохранённый xgb_leaf_meta для кодирования так же, как в train

#CatBoost (важно: факторы и порядок колонок как в train)
nd_cat <- nd
# приведение типов: char->factor("MISSING"), Date/POSIX->numeric, logical->integer
for (v in names(nd_cat)) {
  x <- nd_cat[[v]]
  if (inherits(x, c("Date","POSIXct","POSIXlt"))) nd_cat[[v]] <- as.numeric(x)
  else if (is.logical(x)) nd_cat[[v]] <- as.integer(x)
  else if (is.character(x)) { x[x == "" | is.na(x)] <- "MISSING"; nd_cat[[v]] <- factor(x) }
}
# Выровнять имена/порядок признаков под обучающую матрицу (как было в train)
nd_cat <- nd_cat[, names(nd_cat), drop = FALSE]
pool <- catboost::catboost.load_pool(data = as.data.frame(nd_cat), feature_names = colnames(nd_cat))
p_cat <- catboost::catboost.predict(m_cat, pool, prediction_type = "Probability")

#Если планируете полностью воспроизводимый скоринг фич, сохраняйте не только модели/мету по отдельности, а сразу весь объект fe целиком: saveRDS(fe, "fe_object.rds"). Тогда можно будет сделать fe_loaded <- readRDS(...); fe_loaded$predict_new(new_df).