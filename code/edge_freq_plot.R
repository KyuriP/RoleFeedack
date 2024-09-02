## =========================================================
## Edge Frequency Plot
##
## This script refines the edge frequency plot and generates the 
## final figure used in the paper (e.g., Figure 7: Frequency Proportion 
## of Edges Involved in Feedback Loops).
## =========================================================


# Step 1: Calculate the mean frequency for each edge in the "highnet" group
edge_order <- plotdf %>%
  filter(id == "highnet") %>%
  group_by(edge) %>%
  summarize(mean_m = mean(m)) %>%
  arrange(desc(mean_m)) %>%
  pull(edge)

# Step 2: Reorder the `edge` factor based on the calculated means
plotdf <- plotdf %>%
  mutate(edge = factor(edge, levels = edge_order))

# Main plot without legend
main_plot <- ggplot(data = plotdf) +
  geom_point(aes(x = edge, y = m, col = id), alpha = 0.8, shape = 1, size = 1) +
  geom_line(aes(x = edge, y = m, col = id, group = id), alpha = 0.5) +
  geom_errorbar(aes(x = edge, ymin = m_min, ymax = m_max, color = id), width = 0.3, alpha = 0.5) +
  geom_point(data = plotdf %>% filter(id == "highnet", top10_freq), aes(x = edge, y = m), color = "coral", size = 2, alpha = 0.5) +
  geom_point(data = plotdf %>% filter(id == "lownet", top10_freq), aes(x = edge, y = m), color = "darkseagreen", size = 2,  alpha = 0.5) +
  scale_x_discrete(labels = custom_labels) +
  scale_color_manual(values = c("highnet" = "coral", "lownet" = "darkseagreen")) +
  labs(y = "Proportion of edge frequency", x = "Edges", color = "") +
  theme_pubr() +
  theme(legend.position = "none",  # Remove legend
        text = element_text(size = 23, family = "Palatino"),
        axis.text.x = element_text(angle = 90, vjust= 0.45),
        axis.title.y = element_text(vjust = +4, size = 26),
        axis.title.x = element_blank(), # Remove x-axis title to share with diff plot
        plot.margin = margin(t = 2, r = 2, b = 0, l = 2, "cm"))

# Diff plot with x-axis on top
diff_plot <- ggplot(data = plotdf) +
  geom_line(aes(x = edge, y = diff), group = 1, linetype = "dashed", color = "gray", alpha = 0.7) +
  geom_point(data = plotdf %>% filter(high_diff), aes(x = edge, y = diff, color = "coral4"), size = 3.5, shape = 8, alpha = 0.3) +
  geom_point(data = plotdf %>% filter(low_diff), aes(x = edge, y = diff), color = "darkseagreen4", size = 3.5, shape = 8, alpha = 0.3) +
  #scale_color_manual(values = c("High diff. points" = "coral4")) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 3), limits = c(0.028, -0.028)) +
  scale_x_discrete(position = "top", labels = custom_labels) +
  labs(y = "Difference", x = "Edges", color = "") +
  theme_pubr() +
  theme(legend.position = "none",  # Remove legend
        text = element_text(size = 23, family = "Palatino"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(vjust = +1.5, size = 26),
        axis.title.x = element_blank(), # Remove x-axis title to share with main plot
        plot.margin = margin(t = 0, r = 2, b = 1.5, l = 2, "cm"))


# Extract legend from one of the plots
legend <- cowplot::get_plot_component(
  ggplot(data = plotdf) +
    geom_point(aes(x = edge, y = m, col = id), alpha = 0.8, shape = 1, size = 3) +
    geom_line(aes(x = edge, y = m, col = id, group = id), alpha = 0.6) +
    geom_errorbar(aes(x = edge, ymin = m_min, ymax = m_max, color = id), width = 0.3, alpha = 0.5) +
    geom_point(data = plotdf %>% filter(id == "highnet", top10_freq), aes(x = edge, y = m), color = "black", size = 2, alpha = 0.5) +
    geom_point(data = plotdf %>% filter(id == "lownet", top10_freq), aes(x = edge, y = m), color = "black", size = 2) +
    geom_line(aes(x = edge, y = diff, col = "diff", linetype = "diff"), group = 1,alpha = 0.5) +
    geom_point(data = plotdf %>% filter(high_diff), aes(x = edge, y = diff, shape = "Large diff. points"), color = "coral4", size = 3.5, alpha = 0.3) +
    geom_point(data = plotdf %>% filter(low_diff), aes(x = edge, y = diff, shape = "Large diff. points"), color = "black", size = 3.5) +
    scale_x_discrete(labels = custom_labels) +
    scale_color_manual(values = c("highnet" = "coral", "lownet" = "darkseagreen"), 
                       labels = c("High symptom level network", "Low symptom level network")) +
    scale_shape_manual(values = c("Large diff. points" = 8), labels = c("Sig. high-frequency points")) +
    scale_linetype_manual(values = c("diff" = 2), labels = c("Difference (high-low)"))+
    labs(y = "Proportion of edge frequency", x = "Edges", color = "", linetype = "", shape = "") +
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



# Extract combined legend
#combined_legend <- get_legend(legend_plot)
# legend = cowplot::get_plot_component(edge_plot, 'guide-box-bottom', return_all = TRUE)

# Combine plots with shared x-axis
combined_plot <- cowplot::plot_grid(main_plot, diff_plot, ncol = 1, align = "v", axis = "lr", rel_heights = c(3, 1.3))

# Combine the plot and the legend
final_plot <- cowplot::plot_grid(combined_plot, legend, NULL, ncol = 1, rel_heights = c(8, 1, 0.5))

# ggsave("figure/edge_plot4.pdf", plot = final_plot, width = 40, height = 30, units = "cm", dpi = 300)

comb_final <-final_plot + patchwork::inset_element(p_bidirect, left = 0.70, bottom = 0.83, right = 0.96, top = 1.00)
# ggsave("figure/edge_plot_comb.pdf", plot = comb_final, width = 45, height = 33, units = "cm", dpi = 300)
