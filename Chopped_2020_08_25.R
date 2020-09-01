

library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2020-08-25')
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)

chopped <- tuesdata$chopped

rating <- chopped %>%
  group_by(season)%>%
  count(episode_rating)%>%
  arrange(season)

  
chopped %>%
  filter (season <= 40) %>%
  mutate(mean_all = mean(episode_rating,na.rm=TRUE)) %>%
  group_by(season) %>%
  mutate(mean_rating = mean(episode_rating,na.rm=TRUE),
         fct_season = as.factor(season)) %>%
  ungroup() %>%
  ggplot() +
  ggridges::geom_density_ridges(
    aes(
      x = episode_rating,
      y = fct_season,
      fill = mean_rating )) +
  geom_vline(xintercept = 8.38, linetype="dashed", color = "red") +
   geom_text(data=data.frame(x = 8.38,y=40), aes(x, y), label= "Overall Mean Rating", vjust=-1) +
  annotate("text", x = 6.2, y = 33, label = "Lowest Rating: Celebrity Contestants", color= "red") + 
  labs(
    title = "Chopped: IMDB Ratings of Each Epesode",
    x = "Episode Rating",
    y = "Season",
    caption = "Source: IMDB-Kaggle | Jeffrey Braun"
  ) + 
 theme(plot.background = element_rect(fill = "light grey"))
   
  







chopped_for_plot <- chopped %>%
  mutate(season_as_fct = season_as_character %>%
           fct_inorder() %>%
           fct_rev()) %>%
  group_by(season_as_fct) %>%
  mutate(avr_rating_per_season = mean(
    episode_rating,
    na.rm = TRUE)) %>%
  ungroup()

chopped_plot_lvls <- levels(chopped_for_plot[["season_as_fct"]])

plot <- chopped_for_plot %>%
  ggplot() +
  ggridges::geom_density_ridges(
    aes(
      x = episode_rating,
      y = season_as_fct,
      fill = avr_rating_per_season),
    colour = "darkgrey") +
  theme_minimal() +
  labs(
    x = "Episode rating",
    y = "Season",
    title = "Episode rating distributions, by season") +
  # Code from here: https://stackoverflow.com/questions/32278215/show-every-nth-row-name-in-ggplot2
  scale_y_discrete(breaks = chopped_plot_lvls[seq(1,
                                                  length(chopped_plot_lvls),
                                                  by = 4)]) +
  scale_fill_viridis() +
  xlim(5, 10) +
  theme(
    legend.position="none",
    axis.line = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size=16))




 
  arrange(episode_rating, desc())