
#Initial RHINO dataset load
library(tidyverse)
library(rio)
library(ggplot2)
library(maps)
#load washington county map
wa_county <- map_data("county") |> filter(region == "washington")
plot_1 <- 
  ggplot() + 
  geom_polygon(data = wa_county, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) 
coord_fixed(1.3)
#other ways to map with usmap
library(usmap) # `countypop` data and the `plot_usmap()` function

wa_dat <- countypop |> filter(abbr == "WA")

plot_3 <-
  plot_usmap(data = wa_dat, values = "pop_2022", include = c("WA")) +
  scale_fill_continuous() +
  theme(legend.position = "right")
#more ways to merge data for map use this

# Get county boundaries
wa_county <- map_data("county") |> filter(region == "washington")

# Get county-level ("subregion") population
wa_dat <- countypop |> filter(abbr == "WA") |>
  mutate(subregion = tolower(str_remove(county, " County"))) |>
  group_by(subregion) |> summarize(pop_2022 = sum(pop_2022))

# Combine the data
wa_complete <- wa_county |> inner_join(wa_dat)

#Step 2: create the plot
plot_4 <-
  ggplot() + 
  geom_polygon(data = wa_complete, 
               aes(x = long, y = lat, group = group, fill = pop_2022)) +
  geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington State Population and Cities, 2022",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)

# Get WA cities and their coordinates
wa_cities <- us.cities |> filter(country.etc == "WA")
 
rhino <- import("C:/Users/mkett/Desktop/DaSEH project/rhino_sh_summer_hazards_downloadable.csv")
glimpse(rhino)
#county only multiple years
rhino_county <- rhino |> filter(geography == "County") |> 
  mutate(location = tolower(str_remove(location, " County")))|>
  inner_join(wa_county, by = join_by(location==subregion))
#summary map

 #2006 county data
rhino_2026_co <- rhino_county |> filter(year==2026)

#map 2026 sum of heat hospitalizations
rhino_2026_sum_heat <- rhino_2026_co |>
  filter(hazard == "heat") |>
  group_by(location) |> summarize(max_pct_ed_visits = max(pct_ed_visits)) |>
  inner_join(wa_county, by = join_by(location==subregion))
#2026 sum of smoke hospitalizations
rhino_2026_sum_smoke <- rhino_2026_co |>
  filter(hazard == "smoke") |>
  group_by(location) |> summarize(max_pct_ed_visits = max(pct_ed_visits)) |>
  inner_join(wa_county, by = join_by(location==subregion))
#2026 sum of asthma hospitalizations
rhino_2026_sum_asthma <- rhino_2026_co |>
  filter(hazard == "asthma") |>
  group_by(location) |> summarize(max_pct_ed_visits = max(pct_ed_visits)) |>
  inner_join(wa_county, by = join_by(location==subregion))

rhino_2026_sum_heat <- rhino_2026_co |> 
  filter(hazard == "heat") |>
  group_by(location) |> summarize(pct_ed_visits = sum(pct_ed_visits)) |>
  
  inner_join(wa_county, by = join_by(location==subregion))

#plot this
plot_2026_heat <-
  ggplot() + 
  geom_polygon(data = rhino_2026_sum_heat, 
               aes(x = long, y = lat, group = group, fill = max_pct_ed_visits)) +
  #geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington max heat related emergency visits, 2026",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)
  
plot_2026_smoke <- 
  ggplot() + 
  geom_polygon(data = rhino_2026_sum_smoke, 
               aes(x = long, y = lat, group = group, fill = max_pct_ed_visits)) +
  #geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington max smoke related emergency visits, 2026",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)

plot_2026_asthma <- 
  ggplot() + 
  geom_polygon(data = rhino_2026_sum_asthma, 
               aes(x = long, y = lat, group = group, fill = max_pct_ed_visits)) +
  #geom_point(data = wa_cities, aes(x = long, y = lat), color = "red") +
  labs(
    title = "Washington max asthma related emergency visits, 2026",
    x = "longitude", y = "latitude") +
  coord_fixed(1.3)
                            
