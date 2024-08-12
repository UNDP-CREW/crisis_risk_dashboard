# Input load. Please do not change #
`dataset` = read.csv('C:/Users/seany/REditorWrapper_3bc954ae-f603-4edf-a5e8-aee31db81b71/input_df_c0850bdf-5927-4258-9f34-f013183ac6fd.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# The following code to create a dataframe and remove duplicated rows is always executed and acts as a preamble for your script: 

# dataset <- data.frame(Country_Name_Short, Indicator_SubType, Year, Value)
# dataset <- unique(dataset)

# Paste or type your script code here:

library(tidyverse)
theme_set(theme_light())

dataset |> write_csv(here("data", "economic_stability_data.csv"))

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

