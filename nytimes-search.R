library(data.table)
library(nytimes)

# nytimes_key("<KEY FROM https://developer.nytimes.com>")
days <- 10 * 365
res <- nytimes::ny_search("cyber attack", pages = days, since = Sys.Date() - days)

`%||%` <- function(lhs, rhs) if (length(lhs)) lhs else rhs

extract_article <- function(doc) {
  list(
    pub_date         = doc$pub_date,
    headline         = doc$headline$main,
    abstract         = doc$abstract,
    snippet          = doc$snippet,
    lead_paragraph   = doc$lead_paragraph,
    source           = doc$source %||% NA_character_,
    type_of_material = doc$type_of_material %||% NA_character_,
    document_type    = doc$document_type,
    word_count       = doc$word_count,
    web_url          = doc$web_url, 
    section_name     = doc$section_name %||% NA_character_,
    subsection_name  = doc$subsection_name %||% NA_character_,
    news_desk        = doc$news_desk
  )
}


nyt_df <- rbindlist(
  lapply(res$docs, function(.x) {
    rbindlist(lapply(.x, extract_article))
  })
)

nyt_df[, pub_date := as.POSIXct(pub_date, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
       ][, news_desk := fifelse(news_desk == "None", NA_character_, news_desk)
         ]

tibble::as_tibble(nyt_df)
dplyr::glimpse(nyt_df)

fwrite(nyt_df, "~/nyt_df.csv")
