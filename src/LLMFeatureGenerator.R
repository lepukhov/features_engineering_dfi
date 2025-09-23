 enrich_categories_openai <- function(
     categories,
     attribute_names,
     attributes_descr = NULL,
     model = "gpt-4o-mini",
     api_key = NULL,
     max_concurrency = 5,
     max_retries = 4,
     extra_instructions = NULL,
     timeout_seconds = 300
 ) {
   if (is.null(api_key) || api_key == "") {
     api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
   }
   if (!nzchar(api_key)) {
     stop("OPENAI_API_KEY не задан. Установите Sys.setenv(OPENAI_API_KEY='...') или передайте api_key в функцию.")
   }
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Требуется пакет 'httr2'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Требуется пакет 'jsonlite'.")
  
  format_attr_descr <- function(attribute_names, attributes_descr) {
    if (is.null(attributes_descr)) return(NULL)
    
    as_named_char <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) {
        nm_col <- intersect(names(x), c("name","attribute","key"))
        ds_col <- intersect(names(x), c("description","desc"))
        if (length(nm_col) >= 1 && length(ds_col) >= 1) {
          nms <- as.character(x[[nm_col[1]]])
          dsc <- as.character(x[[ds_col[1]]])
          y <- dsc
          names(y) <- nms
          return(y)
        }
        return(NULL)
      }
      if (is.list(x)) {
        if (is.null(names(x))) return(NULL)
        y <- vapply(x, function(v) paste(as.character(v), collapse = " "), "", USE.NAMES = TRUE)
        names(y) <- names(x)
        return(y)
      }
      if (is.atomic(x)) {
        if (is.null(names(x))) return(NULL)
        y <- as.character(x)
        names(y) <- names(x)
        return(y)
      }
      NULL
    }
    
    named <- as_named_char(attributes_descr)
    if (is.null(named)) return(NULL)
    
    lines <- vapply(attribute_names, function(nm) {
      d <- named[[nm]]
      if (!is.null(d) && nzchar(d)) paste0("- ", nm, ": ", d) else NA_character_
    }, "", USE.NAMES = FALSE)
    lines <- lines[!is.na(lines)]
    if (length(lines) == 0) return(NULL)
    paste(c("Описание атрибутов:", lines), collapse = "\n")
  }
  
   build_messages <- function(category_value, attribute_names, extra_instructions, attributes_descr) {
    attributes_list <- paste(attribute_names, collapse = ", ")
    descr_block <- format_attr_descr(attribute_names, attributes_descr)
    system <- paste0(
      "Ты — помощник для обогащения категориальных значений структурированными атрибутами. ",
      "Всегда возвращай ТОЛЬКО один JSON-объект без пояснений и форматирования вне JSON. ",
      "Допустимы ТОЛЬКО ключи из списка."
    )
    if (!is.null(extra_instructions) && nzchar(extra_instructions)) {
      system <- paste0(system, " Дополнительные правила: ", extra_instructions)
    }
    if (!is.null(descr_block)) {
      system <- paste(system, descr_block, sep = "\n")
    }
    user <- paste0(
      "Категориальное значение: ", category_value, "\n",
      "Верни JSON с ключами: [", attributes_list, "]. ",
      "Строковые значения делай лаконичными; версии указывай в точном виде, если известны."
    )
    list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    )
   }
   call_one <- function(category_value) {
    messages <- build_messages(category_value, attribute_names, extra_instructions, attributes_descr)
    for (attempt in seq_len(max_retries)) {
      result <- tryCatch({
        req <- httr2::request("https://api.openai.com/v1/chat/completions")
        req <- httr2::req_auth_bearer_token(req, api_key)
        req <- httr2::req_timeout(req, timeout_seconds)
        body <- list(
          model = model,
          temperature = 0,
          messages = messages,
          response_format = list(type = "json_object")
        )
        req <- httr2::req_body_json(req, body)
         resp <- httr2::req_perform(req)
        payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        content <- payload$choices[[1]]$message$content
        if (is.null(content) || !nzchar(content)) content <- "{}"
        parsed <- tryCatch(jsonlite::fromJSON(content, simplifyVector = TRUE), error = function(e) NULL)
        
        row <- as.list(setNames(vector("list", length(attribute_names)), attribute_names))
        if (!is.null(parsed) && is.list(parsed)) {
          for (nm in attribute_names) {
            val <- parsed[[nm]]
            if (is.null(val)) {
              row[[nm]] <- NA
            } else if (is.atomic(val) && length(val) == 1) {
              row[[nm]] <- val
            } else {
              row[[nm]] <- jsonlite::toJSON(val, auto_unbox = TRUE)
            }
          }
        } else {
          for (nm in attribute_names) row[[nm]] <- NA
        }
        c(list(category = category_value), row)
      }, error = function(e) NULL)
      
      if (!is.null(result)) return(result)
      if (attempt == max_retries) {
        row <- as.list(setNames(rep(list(NA), length(attribute_names)), attribute_names))
        return(c(list(category = category_value), row))
      }
       Sys.sleep(runif(1, 0.4, 0.8) * (2^(attempt - 1)))
    }
  }
  
  cores <- max(1, min(max_concurrency, parallel::detectCores(logical = TRUE)))
   results <- if (.Platform$OS.type == "unix" && cores > 1) {
    parallel::mclapply(categories, call_one, mc.cores = cores)
  } else {
    lapply(categories, call_one)
  }
  
  dfs <- lapply(results, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  })
  out <- do.call(rbind, dfs)
  rownames(out) <- NULL
  out
}


