# nty-cyber

``` r
df <- data.table::fread(
  "https://raw.githubusercontent.com/knapply/nyt-cyber/master/nyt_df.csv",
  colClasses = list(POSIXct = "pub_date")
)


library(ggplot2)
for_gg <- df[, by_month := lubridate::floor_date(pub_date, "1 month")
             ][, .N, by = by_month
               ]

ggplot(for_gg, aes(x = by_month, N)) +
  geom_line(color = "red") +
  theme_minimal(base_family = "Palantino") +
  labs(title = '"cyber attack"', subtitle = 'Monthly NYT Articles',
       caption = "github.com/knapply/nyt-cyber",
       x = NULL, y = NULL)
```

![](https://i.imgur.com/MNEMvXd.png)
