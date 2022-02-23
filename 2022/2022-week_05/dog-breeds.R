library(ggimage)
library(tidyverse)
library(stringr)
library(showtext)
library(ggtext)

# import data 
breed_traits_raw <-
   readr::read_csv(
      'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv'
   )
breed_rank_all_raw <-
   readr::read_csv(
      'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv'
   )

# dogs rank clean
dogs_rank_long <-
   breed_rank_all_raw %>%
   pivot_longer(
      cols = c(`2013 Rank`:`2020 Rank`),
      names_to = "year",
      values_to = "rank"
   ) %>%
   mutate(year = as.numeric(str_remove(year, " Rank"))) %>%
   select(Breed, year, rank, everything()) %>%
   janitor::clean_names() %>%
   mutate(breed = str_squish(breed))

# dog traits clean
dogs_trait_long <-
   breed_traits_raw %>%
   select(-`Coat Type`, -`Coat Length`) %>%
   pivot_longer(
      cols = c(`Affectionate With Family`:`Mental Stimulation Needs`),
      names_to = "attribute",
      values_to = "value"
   ) %>%
   janitor::clean_names() %>%
   mutate(breed = str_squish(breed))

# transform
top_dogs <-
   dogs_rank_long %>%
   left_join(dogs_trait_long) %>%
   filter(year == 2020) %>%
   mutate(breed = as_factor(breed)) %>%
   group_by(attribute) %>%
   mutate(
      attribute = str_remove(attribute, " Level"),
      attribute = case_when(
         attribute == "Affectionate With Family"   ~ "Affectionate",
         attribute == "Good With Young Children"   ~ "Child-Friendly",
         attribute == "Good With Other Dogs"       ~ "Combativeness",
         attribute == "Openness To Strangers"      ~ "Openness",
         attribute == "Watchdog/Protective Nature" ~ "Protective",
         attribute == "Coat Grooming Frequency"    ~ "Grooming",
         attribute == "Mental Stimulation Needs"   ~ "Stimulation",
         TRUE ~ attribute
      )
   ) %>%
   mutate(attribute = factor(attribute)) %>%
   ungroup() %>%
   group_by(breed) %>%
   arrange(desc(value)) %>%
   mutate(id = row_number()) %>%
   ungroup()

#### add fonts ####
font_add_google(name = "Indie Flower")
font_add_google(name = "Luckiest Guy")
font_add_google(name = "Acme")
showtext_auto()

#### Plot aesthetics ####
title_color <- "#2c2c2a"
subtitle_color <- "#484644"
caption_color  <- "#836a55"

#### annotations ####
title_annotation <- "Dat Hound is a dachsund!"
subtitle_annotation <-
   "Traits are rated <b style = 'color:#2c2c2a'>0 (Low)</b> to <b style ='color:#ac7948'>5 (High)</b>"

#### plot ####
top_dogs %>%
   filter(breed == "Dachshunds") %>%
   ggplot() +
   geom_segment(data = data.frame(y = seq(0, 5, 1)),
      aes(x = -0.5, xend = 15, y = y, yend = y),
      linetype = "ff",
      color = "grey90"
   ) +
   geom_text(data = data.frame(y = seq(0, 5, 1)),
      aes(x = -0.15 , y = y + 0.5, label = y, family = "Luckiest Guy", color = stat(y)),
      size = 6,
      show.legend = FALSE
   ) +
   geom_col(aes(id, value, fill = stat(y)), show.legend = FALSE) +
   ggimage::geom_image(aes(x = -0.5, y = -5.5, image = image), size = 0.24) +
   geom_text(aes(x = id, y = 7, label = attribute),
             size = 6,
             family = "Indie Flower") +
   geom_text(
      aes(label = str_wrap(breed, 20)),
      x = -0.5,
      y = -1.7,
      size = 6,
      family = "Luckiest Guy",
   ) +
   scale_fill_gradientn(colours = c("#2c2c2a", "#ac7948")) +
   scale_color_gradientn(colours = c("#2c2c2a", "#ac7948")) +
   scale_y_continuous(limits = c(-5.5, 7), breaks = seq(0, 5, 1)) +
   scale_x_continuous(limits = c(-0.5, max(top_dogs$id) + 1)) +
   coord_polar(clip = "off") +
   theme_void() +
   # theme elements
   theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      plot.title.position = "plot",
      plot.title = element_markdown(
         family = "Luckiest Guy",
         size = 20,
         colour = title_color
      ),
      plot.subtitle = element_markdown(
         family = "Acme",
         colour = subtitle_color,
         size = 16
      ),
      plot.caption = element_markdown(colour = subtitle_color, family = "Acme")
   ) +
   # labels
   labs(
      title = title_annotation,
      subtitle = subtitle_annotation,
      caption = "Twitter: **@Scott_Hannah_v2**<br>
        Github: **Scott-S-Hannah**<br>
        Source: **American Kennel Club**"
   )

ggsave("2022/2022-week_05/plots/dog-breeds.png", dpi = 320, width = 8, height = 8)
