migrant_stock_changes <- migrant  |>
  filter(aggregation == "no") |> 
  filter(area_of_destination %in% country_list | 
           area_of_origin %in% country_list) |> 
  filter(value != 0 & sex == "total") |> 
  group_by(area_of_origin, area_of_destination, year) |> 
  summarise(migrants = sum(value, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = "year", 
              values_from = "migrants", 
              values_fill = 0, 
              names_prefix = "migrants_") |> 
  mutate(increase_1990 = migrants_2020 - migrants_1990, 
         migrants_increase_1990_pc = increase_1990 / migrants_1990, 
         increase_2000 = migrants_2000 - migrants_1990, 
         migrants_increase_2000_pc = increase_2000 / migrants_2000, 
         increase_2010 = migrants_2020 - migrants_2010, 
         migrants_increase_2010_pc = increase_2010 / `migrants_2010`, 
         increase_2015 = `migrants_2020` - `migrants_2015`, 
         migrants_increase_2015_pc = increase_2015 / `migrants_2015`) %>% 
  mutate_at(vars(migrants_increase_1990_pc, 
                 migrants_increase_2000_pc, 
                 migrants_increase_2010_pc, 
                 migrants_increase_2015_pc), 
            .funs = ~ifelse(is.infinite(.), 1, .)) %>%
  # select(-c(`1990`:`2015`)) |> 
  arrange(desc(`migrants_2020`)) |> 
  mutate(origin_iso = countrycode(area_of_origin,
                                  origin = "country.name", 
                                  destination = "iso3c"), 
         destination_iso = countrycode(area_of_destination, 
                                       origin = "country.name", 
                                       destination = "iso3c")) |> 
  left_join(
    population_estimates |> 
      filter(year %in% c(2000, 2010, 2015, 2020)) |>
      pivot_wider(names_from = year, 
                  values_from = population, 
                  names_prefix = "origin_population_") |> 
      select(-country), 
    by = c("origin_iso" = "country_iso")) |> 
  left_join(
    population_estimates |> 
      filter(year %in% c(2000, 2010, 2015, 2020)) |>
      pivot_wider(names_from = year, 
                  values_from = population, 
                  names_prefix = "destination_population_") |> 
      select(-country), 
    by = c("destination_iso" = "country_iso")) |>  
  # Filtered small origin-destination pairs with small island territories without population data
  # 9 rows total, minimal
  filter(!is.na(origin_population_2020) & !is.na(destination_population_2020)) |> 
  mutate(migrants_origin_100k = migrants_2020 / origin_population_2020 * 100000, 
         migrants_destination_100k = migrants_2020 / destination_population_2020 * 100000) %>%
  mutate_at(vars(migrants_increase_1990_pc, 
                 migrants_increase_2000_pc, 
                 migrants_increase_2010_pc, 
                 migrants_increase_2015_pc, 
                 migrants_origin_100k, 
                 migrants_destination_100k), 
            .funs = ~ifelse(is.nan(.), 0, .))


migrant_stock_changes |> write_csv(here("data", "migrant_stock_changes.csv"))