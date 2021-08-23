# select() df
library(tidyverse)
library(gganimate)

select_df_before <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  -1, "col_A", "col A", 1, "#E41A1C",
  -2, "col_A", "a1", 1, "#E41A1C",
  -3, "col_A", "a2", 1, "#E41A1C",
  -4, "col_A", "a3", 1, "#E41A1C",
  -1, "col_B", "col B", 2, "#377EB8",
  -2, "col_B", "b1", 2, "#377EB8",
  -3, "col_B", "b2", 2, "#377EB8",
  -4, "col_B", "b3", 2, "#377EB8",
  -1, "col_C", "col C", 3, "#4DAF4A",
  -2, "col_C", "c1", 3, "#4DAF4A",
  -3, "col_C", "c2", 3, "#4DAF4A",
  -4, "col_C", "c3", 3, "#4DAF4A"
)

select_df_before$frame <- 1

select_df_after <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  -1, "col_A", "col A", 1, "#E41A1C",
  -2, "col_A", "a1", 1, "#E41A1C",
  -3, "col_A", "a2", 1, "#E41A1C",
  -4, "col_A", "a3", 1, "#E41A1C",
  -1, "col_B", "col B", 3, "#ffffff",
  -2, "col_B", "b1", 3, "#ffffff",
  -3, "col_B", "b2", 3, "#ffffff",
  -4, "col_B", "b3", 3, "#ffffff",
  -1, "col_C", "col C", 2, "#4DAF4A",
  -2, "col_C", "c1", 2, "#4DAF4A",
  -3, "col_C", "c2", 2, "#4DAF4A",
  -4, "col_C", "c3", 2, "#4DAF4A"
)

select_df_after$frame <- 2

# testing
# test for both before and after dfs
select_df_after %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity()

select_df <- bind_rows(select_df_before, select_df_after)

select_df %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity() +
  ggtitle("df[['colA', 'colC']]") +
  theme_void() +
  theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> select_anim

select_anim <- animate(select_anim)
anim_save(here::here("images", "select-multiple-pandas.gif"), select_anim)
