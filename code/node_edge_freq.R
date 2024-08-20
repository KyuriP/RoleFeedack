

# time points to use
ts <- c(800, 1200, 1600, 2000)

# high level networks
highnet <- ts |> map(function(x) {
  comb_avg_res |>
    filter(t == x, nloop!=0) |>
    slice_max(order_by = avg, n = 1000) 
}) |> set_names(paste0("t", ts)) #|>
  bind_rows(.id = "id")

# low level networks
lownet <- ts |> map(function(x) {
  comb_avg_res |>
    filter(t == x, nloop!=0) |>
    slice_min(order_by = avg, n = 1000) 
}) |> set_names(paste0("t", ts)) #|>
 bind_rows(.id = "id")


# for low net, we exclude the acyclic ones as they are not informative 
# lownet <-comb_avg_res|> filter(t == 2000, nloop!=0) |>
#   # slice the bottom 100 
#   slice_min(order_by = avg, n = 100)
high_idx <- highnet |> map(~ transmute(.x, matr = as.numeric(matr) + 1) |> unlist())
low_idx <- lownet |> map(~ transmute(.x, matr = as.numeric(matr) + 1) |> unlist())


# high_idx <-  highnet$matr |> as.numeric() + 1
# low_idx <- lownet$matr |> as.numeric() + 1

# for cycles
high_net <- high_idx |> map(\(x) loop_info[x])
low_net <- low_idx |> map(\(x) loop_info[x])

# high_net <- loop_info[high_idx]
# low_net <- loop_info[low_idx]

high_node <- high_net |> purrr::map_depth(2, \(x) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
                          table(exclude = "") |> as.data.frame()) |> 
                            map(~.x |> list_rbind() |>
  summarize(total = sum(Freq), .by = Var1) |>
  mutate(Totalsum = sum(total),
  total = total / Totalsum)
  )

low_node <- low_net |> purrr::map_depth(2, \(x) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
                                          table(exclude = "") |> as.data.frame()) |> 
  map(~.x |> list_rbind() |>
        summarize(total = sum(Freq), .by = Var1) |>
        mutate(Totalsum = sum(total),
               total = total / Totalsum)
  )

# low_node <- low_net |>  purrr::map(\(x) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
#                                        table(exclude = "") |> as.data.frame()) |> list_rbind() |>
#   summarize(total = sum(Freq), .by = Var1) |>
#   mutate(total = total/sum(total))
  
# check the node frequency 
df_hnode <- high_node |> map_dfr(~.x, .id = "t") 
df_lnode <- low_node |> map_dfr(~.x, .id = "t") 

bind_rows(df_hnode, df_lnode, .id = "id") |>
  ggplot(aes(x = factor(Var1, level = c(1:9)), y = total, col = id, group = id)) +
  geom_point()+ geom_line() +
  facet_wrap(~t)

# combined across diff t
comb_hnode <- df_hnode |> summarize(m = mean(total), sd = sd(total), me = 1.96 * sd / sqrt(4),  .by = Var1)
comb_lnode <- df_lnode |> summarize(m = mean(total), sd = sd(total),  me = 1.96 * sd / sqrt(4), .by = Var1)

node_plot <- bind_rows(comb_hnode, comb_lnode, .id = "id") |>
  mutate(ymin = m - me, ymax = m + me) |>
  ggplot(aes(x = factor(Var1, level = c(1:9), labels = c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")), y = m, col = id, group = id)) +
  geom_point()+ geom_line() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax, color = id), width = 0.2, alpha = 0.5)  +
  scale_color_manual(values=c("1" = "coral", "2" = "darkseagreen"), labels= c("High symptom level networks", "Low symptom level networks")) +
  labs(y = "Proportion of node frequency", x = "Node", color = "") +
  theme_pubr() +
  theme(legend.position = "bottom",
        # space between legend and plot
        text = element_text(size = 23, family="Palatino"),
        legend.text=element_text(size=22),
        legend.key.size = unit(2,"line"),
        legend.box = "vertical",  # Ensure legends are arranged horizontally
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(t = 3, r = 1, b = 1, l = 1, "cm"))

# ggsave("figure/node_plot.pdf", plot = node_plot, width = 32, height = 17, units = "cm", dpi = 300)

  
# check the edges
# Sample vector of cycles

# Function to extract edges from a cycle
extract_edges <- function(cycle) {
  edges <- list()
  for (i in 1:length(cycle)){
    node <- unlist(strsplit(cycle[i], "-"))
    edge <- paste(node, c(node[-1], node[1]), sep = "-")
    # Exclude the last element to avoid repeating the first edge
    edges[[i]] <- edge[-length(edge)]}  
  return(unlist(edges))
}


# get the edge freq proportion for the cycles
high_edges <- high_net |>  purrr::map_depth(2, \(x) x$id |> extract_edges()) |> map(~.x |> unlist() |> table() |> prop.table() |> as.data.frame())
low_edges <- low_net |>  purrr::map_depth(2, \(x) x$id |> extract_edges()) |> map(~.x |> unlist() |> table() |> prop.table() |> as.data.frame())
# low_edges <- low_net |>  purrr::map(\(x) x$id |> extract_edges()) |> unlist() |> table()  |> prop.table() |> as.data.frame()


top10_he <- high_edges |> map_dfr(~.x ,id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), freq_me = 1.96 * freq_sd / sqrt(4), .by = Var1)
top10_le <- low_edges |>  map_dfr(~.x ,id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), freq_me = 1.96 * freq_sd / sqrt(4), .by = Var1)

top10_he |> arrange(desc(freq_m))# |> filter(freq_m >= 0.035)
top10_le |> arrange(desc(freq_m)) #|> filter(freq_m >= 0.035)


plotdf <- top10_he |> full_join(top10_le, by = join_by(Var1)) |>
  set_names(c("edge", "highnet_m","highnet_sd", "highnet_me", "lownet_m", "lownet_sd", "lownet_me")) |>
  mutate(diff = highnet_m - lownet_m) |>
  pivot_longer(!c(edge, diff), names_to = c("id", ".value"), names_sep = "_") |>
  # by 2 (becuz of id) --> if want 7, then 14
  mutate(high_diff = diff %in% sort(diff, decreasing = TRUE)[1:14],
         low_diff = diff %in% sort(diff)[1:14],
         m_min = m - sd,
         m_max = m + sd) |>
  group_by(id) |>   
  # total 32 chocies, half top == 16
  mutate(top10_freq = rank(-m) <= 13) |>
  ungroup()

# overlapping edge (mot -> sui)
plotdf$high_diff[c(11, 12, 37, 38, 63,64)] <- TRUE #sad -> glt, glt -> sad, sui -> motor : not overlapping hence TRUE
plotdf$low_diff[c(29,30,9,10)] <- TRUE # ene -> con, sad -> ene: not overlapping

plotdf$low_diff[c(43, 44, 1,2, 21, 22, 17,18)] <- FALSE # glt -> mot, anh ->sad, ene -> anh : they are not freq points. slp -> ene overlapping hence FALSE


### get the edge freq for the whole network
high_net_whole <- high_idx |> map(\(x) all_networks[x] |>
                                    reshape2::melt() |>
                                    filter(value != 0 & Var1 != Var2) |>
                                    transmute(edge_list = paste0(Var1, "-", Var2)) |>
                                    table() |> prop.table() |> as.data.frame()) 
low_net_whole <- low_idx |> map(\(x) all_networks[x] |>
                                  reshape2::melt() |>
                                  filter(value != 0 & Var1 != Var2) |>
                                  transmute(edge_list = paste0(Var1, "-", Var2)) |>
                                  table()|> prop.table() |> as.data.frame()) 

top10_high_whole <- high_net_whole |> map_dfr(~.x ,id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), freq_me = 1.96 * freq_sd / sqrt(4), .by = edge_list)
top10_low_whole <- low_net_whole |>  map_dfr(~.x ,id = "t") |> summarize(freq_m = mean(Freq), freq_sd = sd(Freq), freq_me = 1.96 * freq_sd / sqrt(4), .by = edge_list)

top10_high_whole |> arrange(desc(freq_m))# |> filter(freq_m >= 0.035)
top10_low_whole |> arrange(desc(freq_m)) #|> filter(freq_m >= 0.035)


plotdf_whole <- top10_high_whole |> full_join(top10_low_whole, by = join_by(edge_list)) |>
  set_names(c("edge", "highnet_m","highnet_sd", "highnet_me", "lownet_m", "lownet_sd", "lownet_me")) |>
  mutate(diff = highnet_m - lownet_m) |>
  pivot_longer(!c(edge, diff), names_to = c("id", ".value"), names_sep = "_") |>
  # by 2 (becuz of id) --> if want 7, then 14
  mutate(high_diff = diff %in% sort(diff, decreasing = TRUE)[1:14],
         low_diff = diff %in% sort(diff)[1:14],
         m_min = m - sd,
         m_max = m + sd) |>
  group_by(id) |>   
  # total 32 chocies, half top == 16
  mutate(top10_freq = rank(-m) <= 13) |>
  ungroup()





# Custom x-axis labels
# Mapping of numbers to characters
number_to_char <- c("1" = "anh", "2" = "sad", "3" = "slp", "4" = "ene", "5" = "app", 
                    "6" = "glt", "7" = "con", "8" = "mot", "9" = "sui")

# Replace numbers with corresponding characters using str_replace_all
plotdf$edge <- str_replace_all(plotdf$edge, number_to_char) |> as.factor()

plotdf_whole$edge <- str_replace_all(plotdf_whole$edge, number_to_char) |> as.factor()

# Create x-labels for edges with arrows
custom_labels <- sapply(levels(plotdf$edge), function(x) {
  # Split the edge into two parts
  parts <- strsplit(x, "-")[[1]]
  # Create the expression with the correct format
  as.expression(bquote(.(parts[1]) %->% .(parts[2])))
})


df <- plotdf # plotdf_whole
#edge_plot <- 
ggplot(data = df) +
  # Points and lines for freq
  geom_point(aes(x = edge, y = m, col = id), alpha = 0.8, shape=1, size = 1) +
  geom_line(aes(x = edge, y = m, col = id, group = id), alpha = 0.6) +
  geom_errorbar(aes(x = edge,ymin = m_min, ymax = m_max, color = id), width = 0.2, alpha = 0.5) +
  # Line for diff values
  # Points for low_top TRUE

  geom_point(data = df %>% filter(id == "highnet", top10_freq), aes(x = edge, y = m), color = "coral", size = 2) +
  geom_point(data = df %>% filter(id == "lownet", top10_freq), aes(x = edge, y = m), color = "darkseagreen", size = 2) +
  # Points for high_top TRUE
  geom_point(data = df %>% filter(high_diff), aes(x = edge, y = diff, shape="Large diff. points"),color = "coral4", size = 3.5, alpha = 0.3) +
  geom_point(data = df %>% filter(low_diff), aes(x = edge, y = diff, shape = "Large diff. points"),color = "darkseagreen4", size = 3.5) +
  geom_line(aes(x = edge, y = diff, col = "diff", linetype="dashed"), group=1, linetype = 2, alpha = 0.5) +
  
  # Custom x-axis labels
  scale_x_discrete(labels = custom_labels) +
  scale_color_manual(values = c("highnet" = "coral", "lownet" = "darkseagreen", diff = "gray"), labels = c("Difference (high-low)", "High symptom level networks", "Low symptom level networks"))+
  scale_shape_manual(values = 8)+
  # scale_linetype_manual(values = c("Difference (high-low)" = "dashed")) +
  labs(y = "Proportion of edge frequency", x = "Edges", color = "", linetype = "", shape = "") +
  theme_pubr() +
  # Customize guides to combine color and linetype
  # guides(color = guide_legend(title = "", override.aes = list(linetype = c(2,1,1), size = 3), nrow=2,ncol=2, byrow=TRUE)) +
  guides(shape = guide_legend(title = "", override.aes = list(color = 1), nrow = 2, ncol = 2, byrow = TRUE, order =1),
    color = guide_legend(title = "", override.aes = list(shape = c(NA, 1, 1), linetype = c(2, 1, 1), size = 3), nrow=1, byrow=TRUE)
  ) +
  
  theme(legend.position = "bottom",
        # space between legend and plot
        text = element_text(size = 23, family="Palatino"),
        axis.text.x = element_text(angle = 80, vjust = 0.5),
        legend.text=element_text(size=22),
        legend.key.size = unit(4,"line"),
        # legend.box = "vertical",  # Ensure legends are arranged horizontally
        legend.margin = margin(0, 0, 0, 0), # Adjust margin inside the legend box
        legend.key.spacing.y = unit(-0.7, "cm"),
        legend.key.spacing.x = unit(0.7, "cm"),
        axis.title.y = element_text(vjust = +3, size =26),
        axis.title.x = element_text(vjust = -0.75, size =26),
        plot.margin = margin(t = 2, r = 1, b = 1, l = 1, "cm"))
    

# ggsave("figure/edge_plot.pdf", plot = edge_plot, width = 40, height = 20, units = "cm", dpi = 300)


## plot the networks
high_A <- matrix(c(.30, 0.00, 0, 0.21, 0, 0, 0, 0, 0,
                   0.33, .30, 0, 0, 0, .13, 0, 0, 0,
                   0,  0.14, .30, 0, 0.23, 0, 0, 0, 0,
                   0, 0.15, 0.22, .30, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, .30, 0.15, 0, 0, 0,
                   0, .13, 0, 0, 0, .30, 0, 0, 0.22,
                   0, 0, 0, 0.12, 0, 0, .30, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, .30, 0,
                   0, 0.15, 0, 0, 0, 0, 0, 0.30, .30), 9, 9, byrow = T)
rownames(high_A) <- colnames(high_A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

manual_layout <- matrix(c( -1.00000000,-0.3697451,
                           -0.25362943,-0.4206165,
                           -0.86442347, 0.4941126,
                           -0.08080896, 0.3245454,
                           0.49177041, 0.9000000,
                           0.43942364,-0.1656119,
                           0.99799343, 0.2837986,
                           1.00000000,-0.7135021,
                           0.41570786,-1.0000000), 9, 2, byrow=T)
high_edge <- matrix(c(0, 0, 0, 1, 0, 0, 0, 0, 0,
                       1, 0, 0, 0, 0, 1, 0, 0, 0,
                       0,  0, 0, 0, 1, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 1, 0, 0, 0,
                       0, 1, 0, 0, 0, 0, 0, 0, 1,
                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 1, 0, 0, 0, 0, 0, 1, 0), 9, 9, byrow = T)
high_edgecol <- ifelse(high_edge == 1, "brown", "coral")
high_edgelty <- ifelse(high_edge == 1, 2, 1)

pdf(file = "figure/highnet.pdf", width=5, height=5, bg = 'transparent', family="Palatino")

qgraph(high_A != 0, layout = manual_layout, edge.color = high_edgecol, lty = high_edgelty, label.color = "black", asize= 5, fade =F, vsize=10, esize = 2)

dev.off()

# low level network

low_A <- matrix(c(.30, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, .30, 0, .15, 0, .13, 0, 0, .15,
                   0,  .14, .30, 0, 0, 0, 0, 0, 0,
                   0, .15, 0, .30, .17, 0, .12, 0, 0,
                   0, 0, 0, 0, .30, 0, 0, 0, 0,
                   0, .13, 0, 0, .15, .30, .20, 0, 0,
                   0, 0, 0, .12, 0, .20, .30, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, .30, 0,
                   0, 0, 0, 0, 0, .22, 0, 0, .30), 9, 9, byrow = T)
rownames(low_A) <- colnames(low_A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

low_edge <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 1, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 1, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 1, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 1, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 1, 0, 0, 0), 9, 9, byrow = T)
low_edgecol <- ifelse(low_edge == 1, "steelblue4", "darkseagreen")
low_edgelty <- ifelse(low_edge == 1, 2, 1)

#edgelty <- ifelse(edgecolors == 1, 2, 1)

# pdf(file = "figure/lownet.pdf", width=5, height=5, bg = 'transparent', family="Palatino")

qgraph(low_A!=0, layout = manual_layout, edge.color = low_edgecol, lty =low_edgelty, label.color = "black", asize= 5, fade = F, vsize=10, esize = 2)

# dev.off()




# edge freq change by time 
high_edges$t800
low_edges$t800

edgedf <- high_edges$t800|> full_join(low_edges$t800, by = join_by(Var1)) |>
  set_names(c("edge", "highnet", "lownet")) |>
  reshape2::melt(value.name = "freq", variable.name = "id")
  

edgedf_high <- high_edges |> list_rbind(names_to = "t") |>
  set_names(c("time", "edge", "freq"))

edgedf_low <- low_edges |> list_rbind(names_to = "t") |>
  set_names(c("time", "edge", "freq"))

# Mapping of numbers to characters
number_to_char <- c("1" = "anh", "2" = "sad", "3" = "slp", "4" = "ene", "5" = "app", 
                    "6" = "glt", "7" = "con", "8" = "mot", "9" = "sui")

# Replace numbers with corresponding characters using str_replace_all
edgedf$edge <- str_replace_all(edgedf$edge, number_to_char) |> as.factor()
edgedf_high$edge <- str_replace_all(edgedf$edge, number_to_char) |> as.factor()
edgedf_low$edge <- str_replace_all(edgedf$edge, number_to_char) |> as.factor()

common_theme <-   theme(legend.position = "bottom",
                        # space between legend and plot
                        text = element_text(size = 23, family="Palatino"),
                        axis.text.x = element_text(angle = 80, vjust = 0.5),
                        legend.text=element_text(size=22),
                        legend.key.size = unit(4,"line"),
                        # legend.box = "vertical",  # Ensure legends are arranged horizontally
                        legend.margin = margin(0, 0, 0, 0), # Adjust margin inside the legend box
                        legend.key.spacing.y = unit(-0.7, "cm"),
                        legend.key.spacing.x = unit(0.7, "cm"),
                        axis.title.y = element_text(vjust = +3, size =26),
                        axis.title.x = element_text(vjust = -0.75, size =26),
                        plot.margin = margin(t = 2, r = 1, b = 1, l = 1, "cm"))

ggplot(data = edgedf) +
  # Points and lines for freq
  geom_point(aes(x = edge, y = freq, col = id), alpha = 0.8, shape=1, size = 1) +
  geom_line(aes(x = edge, y = freq, col = id, group = id), alpha = 0.6) +
  # Custom x-axis labels
  scale_x_discrete(labels = custom_labels) +
  scale_color_manual(values = c("highnet" = "coral", "lownet" = "darkseagreen"), labels = c("High symptom level networks", "Low symptom level networks"))+
  labs(y = "Proportion of edge frequency", x = "Edges", color = "", linetype = "", shape = "") +
  theme_pubr() +
  common_theme

ggplot(data = edgedf_low) +
  # Points and lines for freq
  geom_point(aes(x = edge, y = freq, col = time), alpha = 0.8, shape=1, size = 1) +
  geom_line(aes(x = edge, y = freq, col = time, group = time), alpha = 0.6) +
  # Custom x-axis labels
  scale_x_discrete(labels = custom_labels) +
  # scale_color_manual(values = c("highnet" = "coral", "lownet" = "darkseagreen"), labels = c("High symptom level networks", "Low symptom level networks"))+
  labs(y = "Proportion of edge frequency", x = "Edges", color = "") +
  theme_pubr() +
  common_theme


## =================================================================
## check whether bidirectional edges also happening in low networks
## between sad -- glt

# Define the positions to check
positions_to_check <- list(c(2, 6), c(6, 2))

# Use purrr::map to check if the elements at the specified positions are not zero in each matrix
bidirect_high <- high_idx |> map(\(x) all_networks[x] |>
                              imap(~ all(map_lgl(positions_to_check, function(pos) .x[pos[1], pos[2]] != 0))) |> unlist() |> sum()
) |> unlist() |> as_tibble() |> summarize(Mean = mean(value/1000), SD = sd(value/1000))

bidirect_low <- low_idx |> map(\(x) all_networks[x] |>
                                   imap(~ all(map_lgl(positions_to_check, function(pos) .x[pos[1], pos[2]] != 0))) |> unlist() |> sum()
) |> unlist() |> as_tibble() |> summarize(Mean = mean(value/1000), SD = sd(value/1000))

# visualize it in barplot
p_bidirect <- bidirect_high |> bind_rows(bidirect_low, .id = "id") |>
ggplot() +
  geom_bar(aes(x= id, y = Mean, fill = id), stat = "identity", alpha = 0.3) +
  geom_errorbar(aes(x=id, ymin = Mean -SD, ymax= Mean +SD, color = id), width = 0.5, linewidth =0.5) +
  geom_text(aes(x = id, y = Mean, label = round(Mean, 2)), 
            position = position_stack(vjust = 0.5), 
            colour = "white", size = 5.5, family="Palatino") +
  scale_color_manual(values = c("coral", "darkseagreen")) +
  scale_fill_manual(values = c("coral", "darkseagreen")) +
  scale_x_discrete(labels = c("High net", "Low net")) +
  labs(x = "", y = "", color = "", fill = "", title = expression("Prop. of feedback loop glt" %<->% "sad")) +
  theme_bw() +
  coord_flip()+
  theme(legend.position = "none",
        text = element_text(family="Palatino"),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        # remove ticks
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "lightgray", fill = NA))

