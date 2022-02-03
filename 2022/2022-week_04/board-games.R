library(tidyverse)
library(tidytuesdayR)
library(Redmonder)
library(ggtext)
library(png)
library(cowplot)
library(showtext)
font_add_google(name = "Bangers")
font_add_google(name = "Lato")
showtext_auto()

# load data and subset df
df <- tt_load("2022-01-25")

# join data frames
df_board_games <-
   df$ratings

joint_df <-
   left_join(df$ratings, df$details, by = "id")
# plot --------------------------------------------------------------------

# count total games in each category
df_total_cat <- joint_df %>%
   filter(yearpublished >= 1970) %>%
   pull(boardgamecategory) %>%
   str_remove_all(paste(c("\\[", "\\]", '\\"', "\\'"), collapse = "|")) %>%
   str_split(pattern = ",") %>%
   unlist() %>%
   str_trim() %>%
   tibble(boardgamecategory = .) %>%
   group_by(boardgamecategory) %>%
   add_count(name = "count_category", sort = TRUE) %>%
   ungroup() %>%
   distinct() %>%
   filter(!is.na(boardgamecategory)) %>%
   top_n(10)

# top 20 game count by category
plot_total_cat <-
   df_total_cat %>%
   ggplot(aes(
      x = fct_reorder(boardgamecategory, count_category, .desc = TRUE),
      y = count_category
   )) +
   geom_bar(stat = "identity", fill = alpha("#05F2DB", 1.0)) +
   scale_y_continuous(limits = c(-1000, 6500)) +
   coord_polar(start = 0)

# separate category string
df_yearly_cat <- joint_df %>%
   select(id, boardgamecategory) %>%
   mutate(boardgamecategory = str_remove_all(boardgamecategory, paste(c(
      "\\[", "\\]", '\\"', "\\'"
   ), collapse = "|"))) %>%
   separate_rows(boardgamecategory, sep = ",") %>%
   mutate(boardgamecategory = str_trim(boardgamecategory)) %>%
   distinct() %>%
   left_join(joint_df %>% select(id, primary, year, yearpublished, owned),
             by = "id") %>%
   select(yearpublished, boardgamecategory) %>%
   group_by(yearpublished, boardgamecategory) %>%
   add_count(name = "category_yearly_count", sort = TRUE) %>%
   ungroup() %>%
   distinct() %>%
   filter(!is.na(boardgamecategory)) %>%
   filter(yearpublished >= 1970 & yearpublished <= 2022) %>%
   filter(boardgamecategory %in% df_total_cat$boardgamecategory) %>%
   mutate(
      boardgamecategory = factor(boardgamecategory),
      boardgamecategory = fct_relevel(boardgamecategory, df_total_cat$boardgamecategory)
   )

# create label data frame
df_labels <-
   tibble(
      boardgamecategory = df_total_cat$boardgamecategory,
      x = 2030,
      y = c(1550, 1050, 900, 700, 550, 420, 305, 215, 125, 35)
   )

#### Plot aesthetics ####
background  <- c("#F2F2F2")
lines_color <- c("grey48")
title_color <- c("#F25C05")
subtitle_color <- c("grey42")
text_color  <- c("grey32")
caption_color  <- c("grey48")
pallete_color  <- c(redmonder.pal(name = "qPBI", n = 8), "#387AA8", "#C75153")

# area plot over time
plot <- df_yearly_cat %>%
   ggplot(
      aes(
         x = yearpublished,
         y = category_yearly_count,
         group = boardgamecategory,
         colour = boardgamecategory,
         fill = boardgamecategory
      )
   ) +
   geom_area() +
   scale_x_continuous(breaks = seq(1970, 2020, 10), limits = c(1970, 2035)) +
   geom_text(
      data = df_labels,
      aes(
         x = x,
         y = y,
         label = boardgamecategory,
         colour = boardgamecategory
      ),
      size = 6,
      family = "Bangers",
      fontface = "bold",
      nudge_x = .5
   ) +
   theme(legend.position = "none") +
   scale_fill_manual(values = pallete_color, ) +
   scale_color_manual(values = pallete_color) +
   coord_cartesian(clip = "off")



#### Annotation ####
annotation_title_text <- "Most popular board game catergories!"
annotation_subtitle_text <- "Number of board game releases over the past 50 years<br>
for the top 10 most popular catergories"

plot <- 
   plot +
   theme_minimal_hgrid() +
   theme(
      ## Panel Grid ##
      ## Plot Aesthetic ##
      panel.background = element_rect(fill = background, color = NA),
      plot.background = element_rect(fill = background, color = NA),
      legend.background = element_rect(fill = background, color = NA),
      legend.key = element_rect(fill = background, color = NA),
      axis.ticks = element_line(colour = lines_color),
      ## Legend ##
      legend.position = "none",
      ## Text ##
      text = element_text(
         face = "plain",
         family = "Bangers",
         hjust = 0.5,
         vjust = 0.5,
         angle = 0,
         colour = text_color
      ),
      axis.line.x = element_line(colour = "transparent"),
      axis.line.y = element_line(colour = "transparent"),
      axis.text = element_text(colour = text_color),
      ## Titles & Caption ##
      plot.title.position = "plot",
      plot.title = element_markdown(
         color = title_color,
         family = "Bangers",
         face = "bold",
         size = 20
      ),
      plot.subtitle = element_markdown(
         color = subtitle_color,
         family = "Bangers",
         face = "bold",
         size = 18
      ),
      plot.caption = element_markdown(
         color = caption_color,
         family = "Lato"
      ),
   ) +
   ### Labels ###
   labs(
      title = annotation_title_text,
      subtitle = annotation_subtitle_text,
      x = "Year",
      y = "Number of games published",
      caption = "Twitter: **@Scott_Hannah_v2** | Github: **Scott-S-Hannah**<br>
      Source: **Kaggle, Board Games Geek**")

ggsave(
   "2022/2022-week_04/plots/board-games.png",
   plot = plot,
   dpi = 320,
   width = 8,
   height = 6
)

