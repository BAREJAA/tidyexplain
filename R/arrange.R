# arrange() df

library(tidyverse)
library(gganimate)

# ascend

arrange_df_before <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  -1, "col_A", "col A", 1, "#E41A1C",
  -4, "col_A", "a1", 1, "#E41A1C",
  -3, "col_A", "a2", 1, "#E41A1C",
  -2, "col_A", "a3", 1, "#E41A1C",
  -1, "col_B", "col B", 2, "#377EB8",
  -4, "col_B", "b1", 2, "#377EB8",
  -3, "col_B", "b2", 2, "#377EB8",
  -2, "col_B", "b3", 2, "#377EB8",
  -1, "col_C", "col C", 3, "#4DAF4A",
  -4, "col_C", "c1", 3, "#4DAF4A",
  -3, "col_C", "c2", 3, "#4DAF4A",
  -2, "col_C", "c3", 3, "#4DAF4A"
)

arrange_df_before$frame <- 1


arrange_df_after <- tribble(
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

arrange_df_after$frame <- 2

# testing
# test for both before and after dfs
arrange_df_before %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity()

arrange_df <- bind_rows(arrange_df_before, arrange_df_after)

arrange_df %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity() +
  ggtitle("arrange(colA)") +
  theme_void() +
  theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> arrange_anim

arrange_anim <- animate(arrange_anim)
anim_save(here::here("images", "arrange.gif"), arrange_anim)

###########################3

# descend

arrange_df_before_desc <- tribble(
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

arrange_df_before_desc$frame <- 1

arrange_df_after_desc <- tribble(
  ~.y, ~label, ~value, ~.x, ~color,
  -1, "col_A", "col A", 1, "#E41A1C",
  -4, "col_A", "a1", 1, "#E41A1C",
  -3, "col_A", "a2", 1, "#E41A1C",
  -2, "col_A", "a3", 1, "#E41A1C",
  -1, "col_B", "col B", 2, "#377EB8",
  -4, "col_B", "b1", 2, "#377EB8",
  -3, "col_B", "b2", 2, "#377EB8",
  -2, "col_B", "b3", 2, "#377EB8",
  -1, "col_C", "col C", 3, "#4DAF4A",
  -4, "col_C", "c1", 3, "#4DAF4A",
  -3, "col_C", "c2", 3, "#4DAF4A",
  -2, "col_C", "c3", 3, "#4DAF4A"
)

arrange_df_after_desc$frame <- 2

# testing
# test for both before and after dfs
arrange_df_after_desc %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity()

arrange_df_desc <- bind_rows(arrange_df_before_desc, arrange_df_after_desc)

arrange_df_desc %>%
  ggplot() +
  aes(.x, .y, fill = color, label = value) +
  geom_tile(aes(alpha = 1), width = 0.9, height = 0.9) +
  geom_text(aes(x = .x, color = "white", size = 12), hjust = 0.5, family = "Fira Sans") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_size_identity() +
  ggtitle("arrange(desc(colA))") +
  theme_void() +
  theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
  transition_states(frame, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> arrange_desc_anim

arrange_desc_anim <- animate(arrange_desc_anim)
anim_save(here::here("images", "arrange_desc.gif"), arrange_desc_anim)
