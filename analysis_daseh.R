#Initial RHINO dataset load
library(tidyverse)
library(rio)
library(ggplot2)
library(maps)
library(lmtest)

#load washington county map
wa_county <- map_data("county") |> filter(region == "washington")
plot_1 <- 
  ggplot() + 
  geom_polygon(data = wa_county, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) 
coord_fixed(1.3)
#other ways to map with usmap
library(usmap) # `countypop` data and the `plot_usmap()` function

#wa_dat <- countypop |> filter(abbr == "WA")

#plot_3 <-
#  plot_usmap(data = wa_dat, values = "pop_2022", include = c("WA")) +
#  scale_fill_continuous() +
#  theme(legend.position = "right")
#more ways to merge data for map use this

# Get county boundaries
wa_county <- map_data("county") |> filter(region == "washington")

# Get county-level ("subregion") population
#wa_dat <- countypop |> filter(abbr == "WA") |>
#  mutate(subregion = tolower(str_remove(county, " County"))) |>
#  group_by(subregion) |> summarize(pop_2022 = sum(pop_2022))

# Combine the data
#wa_complete <- wa_county |> inner_join(wa_dat)

#Step 2: create the plot
#plot_4 <-
#  ggplot() + 
#  geom_polygon(data = wa_complete, 
#               aes(x = long, y = lat, group = group, fill = pop_2022)) +
#  geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
#  labs(
#    title = "Washington State Population and Cities, 2022",
#    x = "longitude", y = "latitude") +

#  coord_fixed(1.3)

# Get WA cities and their coordinates
#wa_cities <- us.cities |> filter(country.etc == "WA")

rhino <- import("C:/Users/mkett/Desktop/DaSEH project/rhino_sh_summer_hazards_downloadable.csv")
glimpse(rhino)
rhino <- rhino |> select(-'Date-Time Stamp')
rhino$Date <-mdy(rhino$Date)
#county only multiple years
rhino_county <- rhino |> filter(geography == "County") |> 
  mutate(location = tolower(str_remove(location, " County")))|>
  inner_join(wa_county, by = join_by(location==subregion))
#summary map

#2025 county data
rhino_2025_co <- rhino_county |> filter(year==2025)

#map 2025 highest PM2.5 and max temp


#map 2025 sum of heat hospitalizations
rhino_2025_sum_heat <- rhino_2025_co |>
  filter(hazard == "heat") |>
  group_by(location) |> summarize(max_pct_ed_visits = max(pct_ed_visits)) |>
  left_join(wa_county, by = join_by(location==subregion)) 
#2026 sum of smoke hospitalizations
rhino_2025_sum_smoke <- rhino_2025_co |>
  filter(hazard == "smoke") |>
  group_by(location) |> summarize(max_pct_ed_visits = max(pct_ed_visits)) |>
  full_join(wa_county, by = join_by(location==subregion))
#2026 sum of asthma hospitalizations
rhino_2025_sum_asthma <- rhino_2025_co |>
  filter(hazard == "asthma") |>
  group_by(location) |> summarize(max_pct_ed_visits = max(pct_ed_visits)) |>
  full_join(wa_county, by = join_by(location==subregion))


#plot this
plot_2025_heat <-
  ggplot() + 
  geom_polygon(data = rhino_2025_sum_heat, 
               aes(x = long, y = lat, group = group, fill = max_pct_ed_visits), color="red") +
  #geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington max heat related emergency visits, 2025",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)
plot_2025_heat

plot_2025_smoke <- 
  ggplot() + 
  geom_polygon(data = rhino_2025_sum_smoke, 
               aes(x = long, y = lat, group = group, fill = max_pct_ed_visits), color="red") +
  #geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington max smoke related emergency visits, 2025",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)
plot_2025_smoke

plot_2025_asthma <- 
  ggplot() + 
  geom_polygon(data = rhino_2025_sum_asthma, 
               aes(x = long, y = lat, group = group, fill = max_pct_ed_visits), color="red") +
  #geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington max asthma related emergency visits, 2025",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)
plot_2025_asthma

#rank counties by heat, smoke, and asthma max visits. 
#Use distinct to just return counties
rhino_2025_sum_heat |> group_by(location) |> arrange(desc(max_pct_ed_visits)) |>
  distinct(location)
  

##plot time series temp for Ferry county 2025
#ferry county data 2025 only
rhino_2025_adams <- rhino |> filter(geography == "County") |> 
  mutate(location = tolower(str_remove(location, " County")))|>
  filter(location == "adams") |> filter(year == 2025) |> filter(hazard == "heat")
plot_ts_2025_adams <-
  ggplot(data = rhino_2025_adams, 
         aes(x=Date, y=max_temp_degF)) +  geom_line() + scale_x_date(date_breaks = "1 month") +
        labs(
          title = "Daily temperature in Adams county summer 2025", 
          y = "Temperature (degrees F)"
        )
plot_ts_2025_adams

#try plotting hospital rates and temp on same graph
adams_2025.long <-rhino_2025_adams |>
    select(Date, max_temp_degF, pct_ed_visits) |>
    pivot_longer(-Date, names_to = "variable", values_to = "value")
ggplot(adams_2025.long, aes(Date, value, colour = variable)) + geom_line()

#granger causality test
grangertest(pct_ed_visits~max_temp_degF, order = 2, data = rhino_2025_adams)
# p-value is 0.18 with 2 lags, try adding more data multiple years

rhino_all_adams <- rhino |> filter(geography == "County") |> 
  mutate(location = tolower(str_remove(location, " County")))|>
  filter(location == "adams") |> filter(hazard == "heat")
adams_all.long <-rhino_all_adams |>
  select(Date, max_temp_degF, pct_ed_visits) |>
  pivot_longer(-Date, names_to = "variable", values_to = "value")
ggplot(adams_all.long, aes(Date, value, colour = variable)) + geom_point()

grangertest(pct_ed_visits~max_temp_degF, order = 1, data = rhino_all_adams)
#granger test is significant with all years of data
grangertest(max_temp_degF~pct_ed_visits, order = 1, data = rhino_all_adams)
#not surprising that high visits don't statistically predict temp. 

###Air quality same thing



