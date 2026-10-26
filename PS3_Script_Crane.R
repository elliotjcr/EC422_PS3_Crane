library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

##head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

##head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

##head(retail_ts)
##has_gaps(retail_ts)

##key_vars(canadian_gas)
##index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

##has_gaps(can_gas)
##head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

##head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

## we want the sum as opposed to the average because average values would be taking us back to a monthly average for each quarter, rather than a holistic value representing teh total volume produced in the quarter.
## the quarterly series still appears to be seasonal, with regularly spaced peaks and valleys.

retail_ts |> 
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
  
## every year has one big spike in December followed by a big drop in Jan/Feb, SA does not seem to have this pattern
## the two series do follow the same trend over the long run, including matching up during recessions

can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Monthly Volumes of Gas Sold",
    x = "Date", y = "Millions of Liters", color = NULL
  ) +
  geom_smooth()
  theme_minimal()

## I added Geom_Sooth because I wanted to see the overall trendline through the messy peaks and valleys obstructing the data specifically in the 1980s.