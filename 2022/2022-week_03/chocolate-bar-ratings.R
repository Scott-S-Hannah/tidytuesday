library(tidyverse)
library(tidytuesdayR)
library(ggridges)
library(scico)
library(ggtext)

# load data and subset df
df <- tt_load("2022-01-18")

df_choc <- df$chocolate

# data mutate and subset venezuela
df_choc <- df_choc %>%
   mutate(cocoa_percent = as.numeric(sub("%", "", cocoa_percent)),
          country_of_bean_origin = as_factor(country_of_bean_origin)) %>% 
   filter(country_of_bean_origin == "Venezuela")

# rank top 5 importers
ranking_imports <- df_choc %>% 
   count(company_location) %>% 
   arrange(-n) %>% 
   top_n(10)

top_imports <- ranking_imports$company_location

# subset and mutate comapany location for rating
df_choc_location <- 
   df_choc  %>% 
   filter(company_location %in% top_imports) %>%
   group_by(company_location) %>% 
   mutate(
      company_n = n(),
      rating_avg = round(mean(rating), 1)
   ) %>% 
   ungroup()

worst <-  df_choc_location %>% 
   filter(rating == min(rating)) 
best <-  df_choc_location %>% 
   filter(rating == max(rating))

# plot average rating for top three bean origins
plot <-
   ggplot(df_choc_location) +
   aes(
      y = fct_reorder(company_location, rating, .fun = mean),
      x = rating,
      fill = stat(x),
      label = company_location
   ) +
   stat_density_ridges(
      jittered_points = TRUE,
      color = 'white',
      aes(point_color = stat(x)),
      position = position_raincloud(height = 0.2, width = 0.05),
      alpha = 0.5,
      scale = 0.9,
      point_size = 1,
      rel_min_height = 0.001,
      show.legend = FALSE,
      geom = "density_ridges_gradient",
      quantile_lines = TRUE,
      quantiles = 2,
      quantile_fun = mean
   ) +
   geom_text(
      aes(x = rating_avg - 1.4, color = rating_avg),
      nudge_y = 0.3,
      show.legend = FALSE,
      check_overlap = TRUE,
      size = 8
   ) +
   scale_fill_scico(
      palette = 'vik',
      begin = 0.3,
      aesthetics = c("point_color", "fill", "color")
   ) +
   scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
   scale_x_continuous(
      position = 'top',
      expand = expansion(add = c(0.2, 0.5)),
      limits = c(0.5, 5)
   ) + # venezuela arrow
   geom_curve(
      aes(
         x = 1.2,
         xend = 0.9,
         y = 3.2,
         yend = 4.5
      ),
      ncp = 300,
      angle = 120,
      size = 0.08,
      curvature = -0.3,
      color = scico(
         1,
         direction = -1,
         end = 0.5,
         palette = "bilbao"
      ),
      arrow = arrow(length = unit(2, "mm"),
                    type = "closed")
   ) + #switzerland arrow
   geom_curve(
      aes(
         x = 4.5,
         xend = 4.7,
         y = 10,
         yend = 9
      ),
      ncp = 300,
      angle = 120,
      size = 0.08,
      curvature = -0.3,
      color = scico(
         1,
         direction = -1,
         end = 0.5,
         palette = "bilbao"
      ),
      arrow = arrow(length = unit(2, "mm"),
                    type = "closed")
   )

# themeing
plot <- 
   plot +
   theme_ridges() +
   labs(
      title = "Which country produces the best chocolate <br>
      with Venezuelan cocoa beans?",
      subtitle = "<b style ='color:#8d6852'>Average chocolate</b> rating for the top ten countires",
      caption = "Source: **Flavours of Cacao** |
      Visualisation: **@Scott_Hannah_v2**",
      x = NULL,
      y = NULL
   ) +
   theme(text = element_text(color = "grey48"),
         axis.title = element_blank(),
         plot.background = element_rect(color = NA, fill = "#fbefdf"),
         plot.title = element_markdown(family = "Lato", color = "#462e25",
                                       face = "bold", size = 24),
         panel.grid.major.y = element_blank(),
         plot.subtitle = element_markdown(),
         plot.caption = element_markdown(),
         plot.title.position = "plot",
         axis.text.y = element_blank()) +
   #venezuela annotation
   annotate(
      geom = "richtext",
      x = c(0.85),
      y = c("Germany"),
      label = c(
         "Despite using homegrown <br>
         beans, Venezuela produces <br> 
         the 8th best rated chocolate"
      ),
      color = "#4f1f10",
      fill = NA,
      label.color = NA,
      size = 3
   ) +
   # switzerland annotation
   annotate(
      geom = "richtext",
      x = c(5),
      y = c("France"),
      label = c(
         "Of course, the Swiss<br>
         produce some of <br>
         the best chocolate <br>
         with Venezuelan beans"
      ),
      color = "#4f1f10",
      fill = NA,
      label.color = NA,
      size = 3
   )

# save plot
ggsave("2022/2022-week_03/plots/chocolate-bar-ratings.png", plot = plot, dpi = 320, width = 8, height = 6)


