# importing necesary packages
library(tidyverse)
library(unhcrthemes)

# Installinng the packages
install.packages("refugees",  repos = "http://cran.us.r-project.org")
library(refugees)

# importing the data only in ASEAN between 2010 and 2022
population <- refugees::population %>% filter(coo_iso == c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "VNM") & between(year, 2010, 2022))

glimpse(population)

unique(population$coo_name)

# recode the name of Laos
population$coo_name <- recode(population$coo_name,
                              "Lao People's Dem. Rep." = "Laos",
                              .default = population$coo_name)

# annual trends of refugee in south east asia
population %>% 
  group_by(year) %>% 
  summarise(total_refugees = sum(c(refugees, oip), na.rm = TRUE)) %>% 
  print(n=13)

population %>%
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  summarise(refugees = sum(c(refugees, oip), na.rm = TRUE)) %>% 
  mutate(year = factor(year, levels = unique(year))) %>%
  ggplot(aes(x = refugees, y = year)) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue"), width = 0.8) +
  geom_text(aes(label = scales::label_comma()(refugees)),
            hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Refugee and other people in need of international protection in ASEAN",
       subtitle = "Annual trends in ASEAN (2010 - 2022)",
       caption = "Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 20,
              grid = FALSE,
              axis = FALSE,
              axis_title = FALSE,
              axis_text = "y")

# visualizing total regfugee per country from 2010-2022
population %>% 
  arrange(desc(refugees)) %>% 
  summarise(refugees = scales::comma(sum(c(refugees, oip), na.rm = TRUE)),
            .by = coo_name) 

population %>% 
  summarise(refugees = sum(c(refugees, oip), na.rm = TRUE), .by = coo_name) %>% 
  slice_max(order_by = refugees, n = 10) %>% 
  ggplot(aes(refugees, reorder(coo_name, refugees))) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue"),
           width = 0.8) +
  geom_text(aes(label = scales::label_comma()(refugees)),
            hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Refugee and other people in need of international protection in ASEAN",
       subtitle = "By country of origin in ASEAN (2010 - 2022)",
       caption = "Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 19,
              grid = FALSE,
              axis = FALSE,
              axis_title = FALSE,
              axis_text = "y")

# 2017 vs 2020
population %>%
  filter(year %in% c("2017", "2020")) %>%
  group_by(coo_name, year) %>%
  summarise(refugees = sum(c(refugees, oip), na.rm = TRUE)) %>%
  pivot_wider(names_from = year, values_from = refugees, names_prefix = "year_")

population %>%
  filter(year %in% c("2017", "2020")) %>%
  group_by(coo_name, year) %>%
  summarise(refugees = sum(c(refugees, oip), na.rm = TRUE)) %>% 
  ggplot(aes(refugees, reorder(coo_name, refugees))) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue"),
           width = 0.8) +
  facet_wrap(~year, scales = "free_y", ncol = 1) +
  geom_text(aes(label = scales::label_comma()(refugees)),
            hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Refugee and other people in need of international protection in ASEAN",
       subtitle = "By country of origin in ASEAN 2017 vs 2020",
       caption = "Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 18,
              grid = FALSE,
              axis = FALSE,
              axis_title = FALSE,
              axis_text = "y")

# Myanmar
population %>% 
  filter(coo_name == "Myanmar") %>% 
  summarise(refugees = sum(c(refugees, oip), na.rm = TRUE), .by = year)

population %>% 
  filter(coo_name == "Myanmar") %>% 
  summarise(refugees = sum(c(refugees, oip), na.rm = TRUE), .by = year) %>% 
  mutate(year = factor(year, levels = unique(year))) %>%
  ggplot(aes(x = refugees, y = year)) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue"), width = 0.8) +
  geom_text(aes(label = scales::label_comma()(refugees)),
            hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Refugee Populations from Myanmar",
       subtitle = "Annual refugee trends from Myanmar (2010 - 2022)",
       caption = "Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 16,
              grid = FALSE,
              axis = FALSE,
              axis_title = FALSE,
              axis_text = "y")

unique(population$coa_name)
# recode the coa_name
population$coa_name <- recode(population$coa_name,
                              "United Kingdom of Great Britain and Northern Ireland" = "United Kindom",
                              "United States of America" = "USA",
                              "Netherlands (Kingdom of the)" = "Netherlands",
                              .default = population$coa_name)

# Country of asylum for refugees from Myanmar
population %>%
  filter(coo_name == "Myanmar") %>% 
  summarize(refugees = sum(c(refugees, oip), na.rm = TRUE),
            .by = coa_name) %>% 
  arrange(-refugees) %>% 
  print(n = 30)

population %>%
  filter(coo_name == "Myanmar") %>% 
  summarize(refugees = sum(c(refugees, oip), na.rm = TRUE), .by = coa_name) %>%
  slice_max(order_by = refugees, n = 10) %>%
  ggplot(aes(refugees, reorder(coa_name, refugees))) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue"),
           width = 0.8) +
  geom_text(aes(label = scales::label_comma()(refugees)),
            hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Refugee Populations from Myanmar",
       subtitle = "By country of asylum (2010 - 2022)",
       caption = "Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 18,
              grid = FALSE,
              axis = FALSE,
              axis_title = FALSE,
              axis_text = "y")

# Refugees that seeks asylum 
population %>%
  filter(coo_name == "Myanmar") %>% 
  summarize(asylum_seekers = sum(asylum_seekers, na.rm = TRUE),
            .by = coa_name) %>% 
  arrange(-asylum_seekers) %>% 
  print(n = 30)

population %>%
  filter(coo_name == "Myanmar") %>% 
  summarize(asylum_seekers = sum(asylum_seekers, na.rm = TRUE),
            .by = coa_name) %>% 
  slice_max(order_by = asylum_seekers, n = 10) %>% 
  ggplot(aes(asylum_seekers, reorder(coa_name, asylum_seekers))) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue"),
           width = 0.8) +
  geom_text(aes(label = scales::label_comma()(asylum_seekers)),
            hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Refugee Populations from Myanmar",
       subtitle = "Total asylum seekers country of asylum (2010 - 2022)",
       caption = "Source: UNHCR Refugee Data Finder") +
  theme_unhcr(font_size = 18,
              grid = FALSE,
              axis = FALSE,
              axis_title = FALSE,
              axis_text = "y")



