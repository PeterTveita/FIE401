options(scipen = 999)

require(dplyr)
require(stargazer)

load("CAR_M&A.Rdata")   # lager objektet CAR_MA i environment

Table1 <- CAR_MA %>%
  group_by(yyyy) %>%
  summarise(
    Avg_Deal_Size = mean(deal_value, na.rm = TRUE),
    Avg_CAR       = mean(carbidder, na.rm = TRUE),
    Share_Private = mean(private, na.rm = TRUE),
    Share_Stock   = mean(all_stock, na.rm = TRUE),
    N             = n()
  )

stargazer(Table1,
          type = "latex",
          summary = FALSE,
          digits = 3,
          title = "Table 1. Descriptive Statistics by Year",
          notes = "This table reports yearly averages of deal size, bidder CAR, share of private targets, and share of all-stock deals. N is the number of deals in each year.")

stargazer(Table1,
          type = "text",
          summary = FALSE,
          digits = 3)

