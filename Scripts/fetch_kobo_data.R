.libPaths("/home/runner/work/_temp/Rlibs")
library(httr)
library(jsonlite)
library(glue)

token <- Sys.getenv("KOBO_TOKEN")
form_id <- "aCrqNHbnLFYEEYBpjUkrac"
url <- glue::glue("https://eu.kobotoolbox.org/api/v2/assets/{form_id}/data.json")

print(url)

res <- httr::GET(url, httr::add_headers(Authorization = paste("Token", token)))
httr::stop_for_status(res)

data <- jsonlite::fromJSON(httr::content(res, "text"))$results
data <- jsonlite::flatten(data)

# Drop list-columns
list_cols <- names(data)[sapply(data, is.list)]
if (length(list_cols) > 0) {
  message("⚠️ Dropped list-columns: ", paste(list_cols, collapse = ", "))
}
data <- data[ , !sapply(data, is.list)]

dir.create("output", showWarnings = FALSE)
write.csv(data, "output/kobo_data.csv", row.names = FALSE)

print("✅ Data saved successfully.")
