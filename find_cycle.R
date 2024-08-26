
# Function to normalize cycle
normalize_cycle <- function(cycle) {
  # Split cycle into components, sort and put it back
  components <- str_split(cycle, "-", simplify = TRUE) |> sort() |> paste(collapse = "-")
  return(components)
}



# find cycles

cycles_high <- high_net |> 
  map_depth(2, ~.x |> transmute(id = map_chr(id, normalize_cycle))) |> map(~unlist(.x) |> table() |> prop.table() |>as.data.frame()) |>
  map_dfr(~.x , .id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), .by = Var1) 

cycles_low <- low_net |> 
  map_depth(2, ~.x |> transmute(id = map_chr(id, normalize_cycle))) |> map(~unlist(.x) |> table() |> prop.table() |>as.data.frame()) |>
  map_dfr(~.x , .id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), .by = Var1) 

  # map_depth(2, ~ .x$id) |> map(~unlist(.x) |> table() |> prop.table() |>as.data.frame()) |>
  #  map_dfr(~.x , .id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), .by = Var1)

# table for checking the ids of cycle
tab <- high_net |> 
  map_depth(2, ~.x |> mutate(idnorm = map_chr(id, normalize_cycle)) |> select(id, idnorm)) |> bind_rows() |>
  group_by(idnorm) %>%
  summarize(ids = paste(unique(id), collapse = ", ")) 



freq_cycle <- cycles_high |> full_join(cycles_low, by = "Var1") |>
  set_names(c("cycle", "mean_high","sd_high", "mean_low", "sd_low")) |>
  # replace NA with 0
  replace_na(list(mean_high = 0, sd_high = 0)) %>% 
  mutate(id = paste("loop", 1:nrow(.)),
         diff = mean_high - mean_low) |>
  pivot_longer(!c(cycle, id, diff), names_to = c(".value", "condition"), names_sep = "_") |>
  mutate(high_diff = diff %in% sort(diff, decreasing = TRUE)[1:10],
         low_diff = diff %in% sort(diff)[1:10]) |>
  group_by(condition) |>   
  # total 32 chocies, half top == 16
  mutate(top10_freq = rank(-mean) <= 10) |>
  ungroup()



# just to check the order
freq_cycle |> filter(condition == "high") |> arrange(desc(mean)) |> print(n = 20)
freq_cycle |> filter(condition == "low") |> arrange(desc(mean)) |> print(n =15)


# overlapping edge (mot -> sui)
freq_cycle$high_diff[c(97, 98)] <- TRUE # loop 49 not overlapping hence TRUE
freq_cycle$high_diff[c(95, 96, 47, 48, 5, 6)] <- FALSE # loop 24, 48, 3: not high cycles hence FALSE




# Step 1: Calculate the mean frequency for each edge in the "highnet" group
cycle_order <- freq_cycle %>%
  filter(condition == "high") %>%
  group_by(id) %>%
  arrange(desc(mean)) %>%
  pull(id)

# Step 2: Reorder the `edge` factor based on the calculated means
freq_cycle <- freq_cycle |>
  mutate(id = factor(id, levels = cycle_order))


mainplot_cycle <- freq_cycle |>
  ggplot() +
           geom_point(aes(x = id, y = mean, color = condition), alpha = 0.3) +
           geom_line(aes(x = id, y = mean, color = condition, group = condition), alpha = 0.5) +
  geom_errorbar(aes(x = id, ymin = mean - sd, ymax = mean + sd, color = condition,), width = 0.3, alpha = 0.5) +
  scale_color_manual(values = c("high" = "coral", "low" = "darkseagreen")) +
  geom_point(data = freq_cycle %>% filter(condition == "high", top10_freq), aes(x = id, y = mean), color = "coral", size = 3, alpha = 0.5) +
  geom_point(data = freq_cycle %>% filter(condition == "low", top10_freq), aes(x = id, y = mean), color = "darkseagreen", size = 3,  alpha = 0.5) +
  labs(y = "Proportion of cycle frequency", x = "", color = "") +
  theme_pubr() +
  theme(legend.position = "none",  # Remove legend
        text = element_text(size = 23, family = "Palatino"),
        axis.text.x = element_text(angle = 90, vjust= 0.45),
        axis.title.y = element_text(vjust = +4, size = 26),
        axis.title.x = element_blank(), # Remove x-axis title to share with diff plot
        plot.margin = margin(t = 2, r = 2, b = 0, l = 2, "cm"))


diffplot_cycle <-   ggplot(data = freq_cycle) +
  geom_line(aes(x = id, y = diff, group = condition), color = "gray", alpha = 0.7, linetype = "dashed") +
  geom_point(data = freq_cycle %>% filter(high_diff), aes(x = id, y = diff, color = "coral4"), size = 3.5, shape = 8, alpha = 0.3) +
  geom_point(data = freq_cycle %>% filter(low_diff), aes(x = id, y = diff), color = "darkseagreen4", size = 3.5, shape = 8, alpha = 0.3) +
  #scale_color_manual(values = c("High diff. points" = "coral4")) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 3), limits = c(0.03, -0.035)) +
  scale_x_discrete(position = "top", labels = custom_labels) +
  labs(y = "Difference", x = "Cycles", color = "") +
  theme_pubr() +
  theme(legend.position = "none",  # Remove legend
        text = element_text(size = 23, family = "Palatino"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(vjust = +1.5, size = 26),
        axis.title.x = element_blank(), # Remove x-axis title to share with main plot
        plot.margin = margin(t = 0, r = 2, b = 1.5, l = 2, "cm"))


# Extract legend from one of the plots
legend_cycle <- cowplot::get_plot_component(
  ggplot(data = freq_cycle) +
    geom_point(aes(x = cycle, y = mean, col = condition), alpha = 0.8, shape = 1, size = 3) +
    geom_line(aes(x = cycle, y = mean, col = condition, group = condition), alpha = 0.6) +
    geom_errorbar(aes(x = cycle, ymin = mean - sd, ymax = mean + sd, color = condition), width = 0.3, alpha = 0.5) +
    geom_point(data = freq_cycle %>% filter(condition == "high", top10_freq), aes(x = cycle, y = mean), color = "black", size = 2, alpha = 0.5) +
    geom_point(data = freq_cycle %>% filter(condition == "low", top10_freq), aes(x = cycle, y = mean), color = "black", size = 2) +
    geom_line(aes(x = cycle, y = diff, col = "diff", linetype = "diff"), group = 1,alpha = 0.5) +
    geom_point(data = freq_cycle %>% filter(high_diff), aes(x = cycle, y = diff, shape = "Large diff. points"), color = "coral4", size = 3.5, alpha = 0.3) +
    geom_point(data = freq_cycle %>% filter(low_diff), aes(x = cycle, y = diff, shape = "Large diff. points"), color = "black", size = 3.5) +
    scale_color_manual(values = c("high" = "coral", "lown" = "darkseagreen"), 
                       labels = c("High symptom level network", "Low symptom level network")) +
    scale_shape_manual(values = c("Large diff. points" = 8), labels = c("Sig. high-frequency points")) +
    scale_linetype_manual(values = c("diff" = 2), labels = c("Difference (high-low)"))+
    labs(y = "Proportion of cycle frequency", x = "Cycles", color = "", linetype = "", shape = "") +
    theme_pubr() +
    guides(
      color = guide_legend(title = "", order = 1),
      linetype = guide_legend(title = "", order = 2),
      shape = guide_legend(title = "", order = 3, color = "black", byrow = TRUE)
    ) +
    theme(
      text = element_text(size = 23, family="Palatino"),
      legend.text=element_text(size=20),
      legend.key.size = unit(2,"line"),
      legend.position = "bottom",
      legend.spacing.y = unit(-0.5, "cm"),
      legend.spacing.x = unit(-0.25, "cm")
    ),
  'guide-box-bottom', return_all = TRUE)


# Combine plots with shared x-axis
combined_plot_cycle <- cowplot::plot_grid(mainplot_cycle, diffplot_cycle, ncol = 1, align = "v", axis = "lr", rel_heights = c(3, 1.3))



# Combine the plot and the legend
final_plot_cycle <- cowplot::plot_grid(combined_plot_cycle, legend, NULL, ncol = 1, rel_heights = c(8, 1, 0.5))

# ggsave("figure/cycle_plot.pdf", plot = final_plot_cycle, width = 50, height = 30, units = "cm", dpi = 300)
