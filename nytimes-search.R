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
#> # A tibble: 956 x 13
#>    pub_date            headline abstract snippet lead_paragraph source type_of_material document_type word_count
#>    <dttm>              <chr>    <chr>    <chr>   <chr>          <chr>  <chr>            <chr>              <int>
#>  1 2020-01-28 17:55:09 Travele… "Travel… "Trave… "(Reuters) - … Reute… News             article              278
#>  2 2020-01-28 17:52:24 UK Gove… "Britai… "Brita… "LONDON — Bri… Reute… News             article              794
#>  3 2020-01-28 10:00:36 In Snub… "Britai… "Brita… "LONDON — Bri… AP     News             article              919
#>  4 2020-01-27 11:10:28 Exclusi… "Sweepi… "Sweep… "LONDON — Swe… Reute… News             article              970
#>  5 2020-01-27 01:00:04 Yes, Se… "A form… "A for… "As Secretary… The N… Op-Ed            article              947
#>  6 2020-01-26 13:00:33 Lessons… "It’s b… "It’s … "It’s been mo… AP     News             article             1666
#>  7 2020-01-22 23:54:40 How Jef… "It mos… "It mo… "SAN FRANCISC… The N… News             article             1570
#>  8 2020-01-20 10:00:19 The Wro… "The Ar… "The A… "In the quick… The N… Op-Ed            article             1254
#>  9 2020-01-09 22:45:19 Hackers… "Travel… "Trave… "The numbers … The N… News             article              975
#> 10 2020-01-09 00:03:22 Its Mis… "Drones… "Drone… "American mil… The N… News             article             1589
#> # … with 946 more rows, and 4 more variables: web_url <chr>, section_name <chr>, subsection_name <chr>,
#> #   news_desk <chr>

dplyr::glimpse(nyt_df)
#> Observations: 956
#> Variables: 13
#> $ pub_date         <dttm> 2020-01-28 17:55:09, 2020-01-28 17:52:24, 2020-01-28 10:00:36, 2020-01-27 11:10:28, 2020-…
#> $ headline         <chr> "Travelex Says UK Money Transfer and Wire Services Back Online After Hack", "UK Government…
#> $ abstract         <chr> "Travelex's UK international money transfer service and wire offering is fully operational…
#> $ snippet          <chr> "Travelex's UK international money transfer service and wire offering is fully operational…
#> $ lead_paragraph   <chr> "(Reuters) - Travelex's UK international money transfer service and wire offering is fully…
#> $ source           <chr> "Reuters", "Reuters", "AP", "Reuters", "The New York Times", "AP", "The New York Times", "…
#> $ type_of_material <chr> "News", "News", "News", "News", "Op-Ed", "News", "News", "Op-Ed", "News", "News", "briefin…
#> $ document_type    <chr> "article", "article", "article", "article", "article", "article", "article", "article", "a…
#> $ word_count       <int> 278, 794, 919, 970, 947, 1666, 1570, 1254, 975, 1589, 1086, 1387, 993, 943, 1478, 634, 134…
#> $ web_url          <chr> "https://www.nytimes.com/reuters/2020/01/28/business/28reuters-britain-travelex.html", "ht…
#> $ section_name     <chr> "Business Day", "Business Day", "Business Day", "Business Day", "Opinion", "Business Day",…
#> $ subsection_name  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, "Middle East", NA, "Politics", NA, NA, NA, NA, "Politi…
#> $ news_desk        <chr> NA, NA, NA, NA, "OpEd", NA, "Business", "OpEd", "Business", "Foreign", "NYTNow", "Washingt…

fwrite(nyt_df, "~/nyt_df.csv")

