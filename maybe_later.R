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


require(tidyverse)
require(scales)
require(viridis)
theme_set(theme_light())

country_list <- c("Afghanistan", "Bangladesh", "Bhutan", "Cambodia", 
                  "China", "Fiji", "Hong Kong", "India", "Indonesia", 
                  "Iran", "North Korea", "South Korea", "Laos", 
                  "Malaysia", "Maldives", "Mongolia", "Myanmar", 
                  "Nepal", "Pakistan", "Papua New Guinea", "Philippines", 
                  "Solomon Islands", "Sri Lanka", "Thailand", "Timor-Leste", 
                  "Vanuatu", "Vietnam")

dataset |> 
  group_by(area_of_origin) |> 
  summarise(origin_population_2020 = mean(origin_population_2020), 
            migrants_2020 = sum(migrants_2020)) |> 
  mutate(migrants_pc = migrants_2020 / origin_population_2020 * 100) |> 
  arrange(desc(migrants_pc)) |> 
  filter(area_of_origin %in% country_list) |>
  mutate(area_of_origin = fct_reorder(area_of_origin, migrants_pc)) |> 
  ggplot(aes(x = migrants_2020, y = area_of_origin)) + 
  geom_col(aes(fill = migrants_pc)) +
  geom_text(aes(label = comma(migrants_2020)), 
            size = 3, 
            hjust = "inward") + 
  scale_fill_viridis(direction = -1, end= .95) +
  scale_x_continuous(labels = scales::comma) + 
  labs(title = "Number of persons who migrated\nby country of origin, 2020", 
       subtitle = "Colour shows % of country population have migrated", 
       fill = "% Migrated", 
       y = "", 
       x = "Number of migrants 2020", 
       caption = "Source: International Migrant Stock 2020") + 
  theme(legend.position = c(.85,.85))

require(tidyverse)
require(scales)
require(viridis)
require(ggrepel)
theme_set(theme_light())

country_list <- c("Afghanistan", "Bangladesh", "Bhutan", "Cambodia", 
                  "China", "Fiji", "Hong Kong", "India", "Indonesia", 
                  "Iran", "North Korea", "South Korea", "Laos", 
                  "Malaysia", "Maldives", "Mongolia", "Myanmar", 
                  "Nepal", "Pakistan", "Papua New Guinea", "Philippines", 
                  "Solomon Islands", "Sri Lanka", "Thailand", "Timor-Leste", 
                  "Vanuatu", "Vietnam")

dataset |> 
  filter(area_of_origin %in% country_list) |> 
  group_by(area_of_origin) |> 
  summarise(migrants_1990 = sum(migrants_1990), 
            migrants_1995 = sum(migrants_1995),
            migrants_2000 = sum(migrants_2000), 
            migrants_2010 = sum(migrants_2010), 
            migrants_2015 = sum(migrants_2015), 
            migrants_2020 = sum(migrants_2020)) |> 
  pivot_longer(cols = migrants_1990:migrants_2020, 
               names_to = "year", 
               values_to = "value") |> 
  mutate(year = str_remove_all(year, "migrants_"), 
         year = as.double(year), 
         label = ifelse(year %in% c(2020, 1990), area_of_origin, "")) |> 
  left_join(
    dataset |>
      group_by(area_of_origin) |>
      summarise(migrants_2020 = sum(migrants_2020)) |>
      arrange(desc(migrants_2020)) |>
      mutate(
        category = case_when(
          area_of_origin %in% c("India", "China", "Bangladesh", "Pakistan", "Philippines", 
                                "Afghanistan","Indonesia", "Myanmar", "Vietnam") ~ 
            ">3 million migrants", 
          area_of_origin %in% c("Nepal", "South Korea", "Sri Lanka", "Malaysia", 
                                "Iran", "Laos", "Cambodia", "Thailand", "Hong Kong") ~
            "300,000-3 million", 
          area_of_origin %in% c("Fiji", "North Korea", "Mongolia", "Bhutan", 
                                "Timor-Leste", "Vanuatu", "Papua New Guinea", 
                                "Solomon Islands", "Maldives") ~ 
            "<=300,000 migrants")) |>
      select(area_of_origin, category), 
    by = "area_of_origin"
  ) |> 
  ggplot(aes(x = year, y = value)) + 
  geom_line(aes(colour = area_of_origin), 
            size = .6) + 
  ggrepel::geom_text_repel(aes(label = label), 
                           size = 1.5) +
  scale_y_log10(labels = comma) + 
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "black")) +
  facet_wrap(~ fct_relevel(category, 
                           c("<=300,000 migrants",
                             "300,000-3 million", 
                             ">3 million migrants in 2020")), 
             ncol = 1, scales = "free_y") + 
  labs(title = "Changes in number of persons\nmigrated by area of origin, 1990-2020", 
       subtitle = "Source: International Migrant Stock", 
       y = "",
       x = "")


require(tidyverse)
require(ggraph)
require(igraph)

theme_set(theme_light())

set.seed(7)

dataset |> 
  select(area_of_origin, area_of_destination, migrants_2020, origin_population_2020) |> 
  mutate(n = 1, 
         origin_population_pc = migrants_2020 / origin_population_2020) |> 
  filter(migrants_2020 > 100000 | origin_population_pc > .005) |> 
  igraph::graph_from_data_frame() |> 
  ggraph(layout = "circle") + 
  geom_edge_link(aes(edge_width = migrants_2020, 
                     alpha = origin_population_pc),
                 lineend = "round",
                 colour = "#F58C46FF", 
                 check_overlap = TRUE) + 
  scale_edge_width_continuous(range = c(.1, 7)) + 
  scale_alpha_continuous(range = c(0.1, .9)) +
  geom_node_point(colour = "#F58C46FF", alpha = 0.2) +
  geom_node_text(aes(label = name), size = 2.8) + 
  theme(legend.position = "none") + 
  labs(title = "Origins and destinations of Asia-Pacific migrants in 2020", 
       subtitle = "Thickness of line indicates number of migrants, trasparency indicates % of origin population migrated\nOnly displaying the top origin-destination pairs", 
       caption = "Source: International Migrant Stock 2020")


# Input load. Please do not change #
`dataset` = read.csv('C:/Users/seany/REditorWrapper_f4bcdc5a-9abe-43a1-98b1-14b4ed13a9c8/input_df_f6ad1eeb-38a5-4544-bdf8-df63ac6dba40.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# The following code to create a dataframe and remove duplicated rows is always executed and acts as a preamble for your script: 

# dataset <- data.frame(Country_Name_Short, Indicator_SubType, Year, Value)
# dataset <- unique(dataset)

# Paste or type your script code here:

library(tidyverse)
theme_set(theme_light())

range_wna <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

dataset |>
  group_by(Country_Name_Short, Indicator_SubType) |> 
  mutate(rescaled_value = range_wna(Value)) |>
  ungroup() |> 
  mutate(indicator_label = 
           case_when(
             str_detect(Indicator_SubType, "Consumer price index") ~ "Consumer price index", 
             str_detect(Indicator_SubType, "GDP per capita") ~ "GDP per capita", 
             str_detect(Indicator_SubType, "Total debt service") ~ "Total debt service", 
             str_detect(Indicator_SubType, "Inflation") ~ "Inflation annual", 
             TRUE ~ Indicator_SubType
           )) |>
  filter(indicator_label != "Consumer price index") |>
  ggplot(aes(x = Year, y = rescaled_value)) + 
  geom_line(aes(colour = indicator_label)) + 
  scale_colour_manual(values = c("#35B779FF", "#440154FF", "#EDD03AFF")) + 
  facet_wrap(~ Country_Name_Short, ncol = 6) + 
  labs(title = "Macroeconomic indicator trends", 
       subtitle = "All values rescaled by their 10-year maximum: Consumer Price Index, base 2010; GDP per capita 2017 constant $; Total debt service as a % of GNI", 
       colour = "", 
       y = "Rescaled value", 
       x = "", 
       caption = "Source: World Bank") + 
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 40, hjust = .8, vjust = 1, size = 6), 
        axis.text.y = element_text(size = 6), 
        plot.caption = element_text(hjust = .5), 
        strip.background = element_rect(fill = "black"))


ggsave(here::here("plots", "macreconomic_indicator_trends.png"), 
       height = 320,
       width = 420, 
       units = "mm")

maldives_normalised <- maldives |>
  # Crime data
  filter(indicator %out% exclude_indicators & 
           # Filter rows with no island
           !is.na(island) & 
           # Only administrative islands
           administration == "Administrative Islands" & 
           # Remove resident Maldivians 2014
           population != "Resident Maldivians 2014") |> 
  # filter empty rows with no indicator
  filter(!is.na(indicator)) |> 
  group_by(indicator, population) %>% 
  mutate(range01 = range_wna(value)) |> 
  ungroup() 

hrd |> 
  mutate(country_iso = countrycode(country,
                                   origin = "country.name", 
                                   destination = "iso3c")) |>
  filter(str_detect(date_added, "2023")) |>
  group_by(country, country_iso) |> 
  summarise(count = n_distinct(serial_number), 
            impact_sum = sum(impact_of_event, na.rm = TRUE), 
            .groups = "drop") |> 
  filter(!is.na(impact_sum)) |> 
  write_csv(here("data", "hrd_text_and_map.csv"))

set.seed(4777)


mutate(rights_count = 
         ifelse(!is.na(rights_long), 1, 0), 
       violation_count = 
         ifelse(!is.na(violation_long), 1, 0), 
       hrd_count = 
         ifelse(!is.na(hrd_long), 1, 0)) |> 
  group_by(serial_number) |> 
  summarise(
    rights_count = sum(rights_count), 
    violation_count = sum(violation_count), 
    hrd_count = sum(hrd_count)
  ) |> 
  mutate(total_tags = rights_count + violation_count + hrd_count, 
         types_of_tags = case_when(
           rights_count > 0 & violation_count == 0 & hrd_count == 0 ~ "only_rights", 
           rights_count  == 0 & violation_count > 0 & hrd_count == 0 ~ "only_violations",
           rights_count == 0 & violation_count == 0 & hrd_count > 0 ~ "only_hrd",
           rights_count > 0 & violation_count > 0 & hrd_count == 0 ~ "rights_and_violations",
           rights_count > 0 & violation_count == 0 & hrd_count > 0 ~ "rights_and_hrd",
           rights_count == 0 & violation_count > 0 & hrd_count > 0 ~ "violations_and_hrd",
           rights_count > 0 & violation_count > 0 & hrd_count > 0 ~ "rights_violations_hrd",
           TRUE ~ NA_character_
         )) |> 
  arrange(total_tags) |> 
  filter(str_detect(types_of_tags, "only"))

hrd_long |> 
  filter(violation_long == "(Arbitrary) Arrest and Detention") |> 
  sample_n(5) |> 
  pull(summary_for_publications)