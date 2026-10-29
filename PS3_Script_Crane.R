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
  
## At first, the seasonal swings really grow with the rise in gas sales (1980s), but then the swings seem to level off in the 1990s and 2000s, even as the amount of gas sold continues to increase.
  
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |> 
  autoplot(log_volume) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Monthly Growth Rate of Gas Sold",
    x = "Date", y = "Log Volume", color = NULL
  ) +
theme_minimal()

## The log plot shows a sharper relative increase in the first 20 years of data compared to the last 20 years, even though the actual amount changed was similar over both periods.
## This is better because the seasonal swings are now a more uniform size.
  
rsa <- retail_ts %>%
  filter(series_id == "RSAFSNA") %>%
  mutate(log_sales = log(sales))

rsa |>
  autoplot(sales) +
  labs(
    title = "RSAFSNA: level", y = "Millions of dollars",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

rsa |>
  autoplot(log_sales) +
  labs(
    title = "RSAFSNA: log scale", y = "log(millions of dollars)",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## I think log is more useful for transforming the canadian gas data because the graphs are more distinct. The retail data essentially remains the same shape plotted out in either level or log form, just with a concave up vs. concave down slope.

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Direct Growth Rate of Gas Sold",
    x = "Date", y = "Growth Rate (%)", color = NULL
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Gap Between Direct and Log Growth Rates",
    x = "Date", y = "Gap (%)", color = NULL
  ) +
  theme_minimal()

## The largest gap between the log difference and the growth rate is around 3%, which I think means the log overall did a good job estimating the percent changes in gas sales.

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

## The non-seasonally adjusted data for growth rates is far noisier. Every December, there is a large spike in the growth rate, followed by a large drop in January. The seasonally adjusted data is much smoother and more consistent.

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

## The seasonally adjusted data shows up in the plot as much smoother lines, showing overall trends throughout the year rather than bouncing around month to month. The seasonally adjusted plot also makes the covid dip much more apparent due to how much it stands out compared to otherwise minor trends.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "Millions of Liters"
  ) +
  theme_minimal()

## The summer months tend to have the lowest gas production/sales, which is honestly not what I would expect. I would think people would be going on more road trips in the summer and demand for gas would be higher.
## Maybe it has to do with skiing/snow plows/winter sports...
## OH!! it's NATURAL gas, which can be used to heat homes/water supplies. This makes more sense now.

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

## It is difficult, based on the scale of this plot, to see what is happening on a month by month basis. The seasonal swings seem to be slightly petering out as we get closer to the present(especially the upswings).
