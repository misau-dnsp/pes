.libPaths("/home/runner/work/_temp/Rlibs")
library(httr)
library(jsonlite)
library(glue)

# Get token from environment
token <- Sys.getenv("KOBO_TOKEN")

# Define form ID (replace with yours)
form_id <- "a1BcD2eF3GhijKLm"
base_url <- glue("https://eu.kobotoolbox.org/api/v2/assets/{form_id}/data.json")

# Fetch data
res <- GET(base_url, add_headers(Authorization = paste("Token", token)))
stop_for_status(res)
data <- fromJSON(content(res, "text"))$results

# Save to file
dir.create("output", showWarnings = FALSE)
write.csv(data, file = "output/kobo_data.csv", row.names = FALSE)
