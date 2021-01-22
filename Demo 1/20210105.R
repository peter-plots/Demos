## Transit costs tidy tuesday

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-01-05')
df <- tuesdata$transit_cost


df <- df %>% 
  janitor::clean_names() %>% 
  filter(real_cost != "MAX")

df %>%
  view()

df %>%
  group_by(country) %>%
  summarize(max_cost = max(cost_km_millions)) %>%
  view()

install.packages("countrycode")
library(countrycode)



df <- codelist %>%
  select(iso2c, country.name.en, continent) %>%
  inner_join(df, by = c("iso2c" = "country"))


df$country <- df$country.name.en

p1 <- df %>%
  filter(rr == 1) %>%
  group_by(city, country, continent) %>%
  summarize(avg_cost_km = mean(cost_km_millions)) %>%
  arrange(desc(avg_cost_km)) %>%
  ggplot() +
  geom_col(aes(x = reorder(city, avg_cost_km), y = avg_cost_km, fill = continent),
           color = "black") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 3050)) +
  coord_flip() +
  labs(title = "Railroad Projects by City and Continent",
       subtitle = "Average Cost per KM",
       y = "Average Cost (in Millions of USD)",
       x = "",
       fill = "",
       caption = "Tidy Tuesday Week 2 2021") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia"),
    panel.grid.major.x = element_line(linetype = "dotted",
                                      color = "darkgrey"),
    panel.background = element_rect(fill = "beige"),
    plot.background = element_rect(fill = "beige"),
    legend.background = element_rect(fill = "beige"),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

ggsave("rr_cost.png",
       plot = p1,
       width = 5.81,
       height = 4.77,
       unit = "in")



df %>%
  filter(city == "New York" & rr == 1) %>%
  view()









