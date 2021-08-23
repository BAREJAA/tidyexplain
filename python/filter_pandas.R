# filter() df

# numeric
library(tidyverse)
library(gganimate)

filter_df_before_num <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  4, "col_A", "col A", 1, "#E41A1C",
  1, "col_A", "a3", 1, "#E41A1C",
  4, "col_B", "col B", 2, "#377EB8",
  1, "col_B", "b3", 2, "#377EB8",
  4, "col_C", "col C", 3, "#4DAF4A",
  1, "col_C", "3", 3, "#4DAF4A",
  3, "col_A", "a1", 1, "#E41A1C",
  3, "col_B", "b1", 2, "#377EB8",
  3, "col_C", "1", 3, "#4DAF4A",
  2, "col_A", "a2", 1, "#E41A1C",
  2, "col_B", "b2", 2, "#377EB8",
  2, "col_C", "2", 3, "#4DAF4A",
)

filter_df_before_num$frame <- 1

filter_df_after_num <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  4, "col_A", "col A", 1, "#E41A1C",
  3, "col_A", "a3", 1, "#E41A1C",
  4, "col_B", "col B", 2, "#377EB8",
  3, "col_B", "b3", 2, "#377EB8",
  4, "col_C", "col C", 3, "#4DAF4A",
  3, "col_C", "3", 3, "#4DAF4A",
)

filter_df_after_num$frame <- 2

# testing
# test for both before and after dfs
filter_df_before_num %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity()

filter_df_num <- bind_rows(filter_df_before_num, filter_df_after_num)

filter_df_num %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity() +
  ggtitle("df.loc[df['colC'] > 2]") +
  theme_void() +
  theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> filter_anim_num

filter_anim_num <- animate(filter_anim_num, nframes = 100)
anim_save(here::here("images", "filter_numeric-pandas.gif"), filter_anim_num)

#######################
# categorical
library(tidyverse)
library(gganimate)

filter_df_before_cat <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  4, "col_A", "col A", 1, "#E41A1C",
  2, "col_A", "a2", 1, "#E41A1C",
  4, "col_B", "col B", 2, "#377EB8",
  2, "col_B", "b2", 2, "#377EB8",
  4, "col_C", "col C", 3, "#4DAF4A",
  2, "col_C", "2", 3, "#4DAF4A",
  3, "col_A", "a1", 1, "#E41A1C",
  3, "col_B", "b1", 2, "#377EB8",
  3, "col_C", "1", 3, "#4DAF4A",
  1, "col_A", "a3", 1, "#E41A1C",
  1, "col_B", "b3", 2, "#377EB8",
  1, "col_C", "3", 3, "#4DAF4A",
)

filter_df_before_cat$frame <- 1

filter_df_after_cat <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  4, "col_A", "col A", 1, "#E41A1C",
  3, "col_A", "a2", 1, "#E41A1C",
  4, "col_B", "col B", 2, "#377EB8",
  3, "col_B", "b2", 2, "#377EB8",
  4, "col_C", "col C", 3, "#4DAF4A",
  3, "col_C", "2", 3, "#4DAF4A",
)

filter_df_after_cat$frame <- 2

# testing
# test for both before and after dfs
filter_df_after_cat %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity()

filter_df_cat <- bind_rows(filter_df_before_cat, filter_df_after_cat)

filter_df_cat %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity() +
  ggtitle("df.loc[df['colA'] == 'a2']") +
  theme_void() +
  theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> filter_anim_cat

filter_anim_cat <- animate(filter_anim_cat, nframes = 100)
anim_save(here::here("images", "filter_categorical-pandas.gif"), filter_anim_cat)
