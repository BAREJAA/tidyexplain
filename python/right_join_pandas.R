source(here::here("R/00_base_join.R"))

rj_joined_dfs <- right_join(x, y, "id") %>%
  proc_data("y") %>%
  mutate(frame = 2, .x = .x + 1)

rj_extra_blocks <- inner_join(x, y, "id") %>%
  select(id) %>%
  proc_data("x") %>%
  mutate(frame = 2, .x = .x + 1)

rj <- bind_rows(
  initial_join_dfs,
  rj_joined_dfs,
  rj_extra_blocks
) %>%
  filter(!is.na(value)) %>%
  mutate(
    .id = ifelse(label == "x", label, .id),
    removed = as.integer(grepl("3", value))
  ) %>%
  arrange(removed, value, .id, frame) %>%
  plot_data_small_title("pd.merge(x, y, how = 'right', on = 'num_col')") %>%
  animate_plot()

rj <- animate(rj)
anim_save(here::here("images", "right-join_pandas.gif"), rj)

rj_g <- plot_data_join(rj_joined_dfs, "pd.merge(x, y, how = 'right', on = 'num_col')")
save_static_plot(rj_g, "right-join")
