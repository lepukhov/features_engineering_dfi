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
  - `factors_first_portion.csv`
  - `Juicy.xlsx`
  - `Result.xlsx`
  - словари: `dict_ipOwner_enrichment.csv`, `dict_ipRegionName_enrichment.csv`, `dict_ipCity_enrichment.csv`
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

