# load packages
library(tidyverse)
library(tidytuesdayR)
library(geomtextpath)
library(ggmap)
libraray(ggtext)

# load data and subset df
df <- tt_load("2022-01-11")

df_colony <- df$colony

df_stress <- df$stressor

# calculate yearly loss and renovation 
df_colony_yearly_sum <- 
   df_colony %>% 
   filter(state == "United States") %>% 
   group_by(year) %>% 
   summarise(colony_lost = (sum(colony_lost, na.rm = TRUE) * -1),
             colony_reno = sum(colony_reno, na.rm = TRUE)) %>% 
   pivot_longer(-year)

# add colour scheme
mycolors <- c("#010600", "#E9B033")
   
# basic plot
plot <- 
   df_colony_yearly_sum %>% 
   ggplot(aes(year, value, fill = name, colour = name, group = name)) +
   geom_col() +
   scale_y_continuous(labels = scales::number_format(scale = 0.000001, suffix = "m"), breaks = seq(-2e6, 2e6, by = 250000)) +
   scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
   coord_flip() +
   scale_fill_manual(values = mycolors,
                     labels = c("Colony Loss", "Colony Renovated")) +
   scale_color_manual(values = mycolors,
                      labels = c("Colony Loss", "Colony Renovated")) +
   theme_minimal()


# add bee images to tibble
bees <-  tibble(img = c("2022/2022-week_02/images/bee_left.png","2022/2022-week_02/images/bee_right.png"),
                name = c("colony_lost","colony_reno"),
                label = c("Lost\nColonies","Renovated\nColonies"),
                x = c(2021, 2021),
                y = c(-1.25e6, 1.25e6),
                x_position = x - if_else(label == "Lost\nColonies", 1.5e6, -1.54e6),
                y_position = 2021)

# add bees to plot
bee_plot <- 
   plot +
   geom_image(data = bees, 
              aes(x = x, y = y, image = img, color = name),
              size = 0.10,
              asp = 1,
              inherit.aes = FALSE) +
   geom_text(data = bees,
             aes(
                x = y_position,
                y = x_position,
                label = label,
                color = name
             ))

# final plot theming
bee_plot <- 
   bee_plot +
   theme_minimal() +
   theme(
      axis.title = element_blank(),
      plot.background = element_rect(color = NA, fill = "white"),
      text = element_text(color = "grey48"),
      plot.title = element_markdown(family = "Lato", color = "grey2", face = "bold"),
      plot.subtitle = element_markdown(),
      plot.caption = element_markdown(),
      plot.title.position = "plot",
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "grey87", size = 0.1),
      legend.position = "none") +
   labs(title = "To bee, or not to bee",
        subtitle = "Annual bee colonies <b style ='color:#010600'>lost</b> and <b style ='color:#E9B033'>renovated</b> in the United States over the last 7 years",
        caption = "Source: **U.S. Department of Agriculture** |
       Visualisation: **@Scott_Hannah_v2**",
       )

ggsave("2022/2022-week_02/plots/bees.png", dpi = 320, width = 8, height = 6)


