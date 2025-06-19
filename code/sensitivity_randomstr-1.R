library(furrr); library(purrr); library(dplyr); library(qgraph)

plan(multisession, workers = 12)  # or try 2

source("code/mod_specification.R")
source("code/euler_stochastic2.R")
source("code/helper_func.R")


# set seed for reproducibility
set.seed(123)
## seed1 with 500 nets

# function to generate random network with same density (number of edges)
generate_random_weighted_network <- function(n_nodes = 9, n_edges = 16, 
                                             weight_range = c(0.1, 0.4), 
                                             self_loop_value = 0.3, 
                                             seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Initialize matrix
  A <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  diag(A) <- self_loop_value  # set constant self-loop weights
  
  # Identify off-diagonal positions
  off_diag_indices <- which(row(A) != col(A), arr.ind = TRUE)
  
  # Randomly choose which edges to keep
  selected_edges <- off_diag_indices[sample(nrow(off_diag_indices), n_edges, replace = FALSE), ]
  
  # Assign random weights to selected edges
  rand_weights <- runif(n_edges, weight_range[1], weight_range[2])
  for (k in seq_len(n_edges)) {
    i <- selected_edges[k, "row"]
    j <- selected_edges[k, "col"]
    A[i, j] <- rand_weights[k]
  }
  
  # Optional: assign row and column names
  var_names <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")
  rownames(A) <- colnames(A) <- var_names
  
  return(A)
}

# Step 1: Generate a new random base network (structure + weights)
A_random <- generate_random_weighted_network(
  n_nodes = 9,
  n_edges = 16,                  # same as in A_orig
  weight_range = c(0.1, 0.4),
  self_loop_value = 0.3,
  seed = 1 # change seed as you wish
)

# Step 2: Identify modifiable edges (edges with non-zero and off-diagonal)
modifiable_edges <- which(A_random > 0 & row(A_random) != col(A_random), arr.ind = TRUE)
modifiable_edges <- lapply(seq_len(nrow(modifiable_edges)), function(i) modifiable_edges[i, ])

# Step 3: Generate all directional configurations
all_configs <- generate_configurations(A_random, modifiable_edges)

# Step 4: Count loops (excluding self-loops)
loop_numbers <- purrr::map_dbl(all_configs, \(network) {
  find_loops(create_adjacency_list(network), network) |> length() - 9
})

# Optional: inspect loop distribution
print(table(loop_numbers))
loop_counts <- loop_numbers

# Setup
target_loops <- c(0,3,6,9,12,15,18) #c(0,3,6,9,12,15)
max_per_group <- 500 #100
sim_per_net <- 30
deltaT <- 0.5 #0.2
timelength <- 2000

# Sample equal number of configurations per loop group (0, 5, 10, 15)

withr::with_seed(123, {
  selected_configs <- unlist(lapply(target_loops, function(k) {
    idx <- which(loop_counts == k)
    if (length(idx) == 0) return(integer(0))
    sample(idx, min(max_per_group, length(idx)))
  }), use.names = FALSE)
})

configs <- all_configs[selected_configs]
loop_counts <- loop_counts[selected_configs]

# Define the exact time points you want
target_times <- c(400, 800, 1200, 1600, 1999.5)
time_indices <- as.integer(target_times / deltaT + 1)  # Index positions

# Run simulation
# Run simulation and directly summarize by config and time
df_summary <- future_map_dfr(seq_along(configs), function(i) {
  A_conf <- configs[[i]]
  loop_count <- loop_counts[[i]]
  
  mod <- mod_spec("base", init_val = 0.01, mat = A_conf)
  
  sim_outputs <- replicate(sim_per_net, {
    out <- euler_stochastic2(
      Amat = mod$A,
      deterministic_rate = mod$dif_eq,
      stochastic_rate = mod$sto_eq,
      initial_condition = mod$initial_values,
      parameters1 = c(mod$Beta_bistable, mod$delta),
      parameters2 = c(mod$Beta_sick, mod$delta),
      deltaT = deltaT,
      timelength = timelength,
      D1 = 0.01,
      shock = TRUE,
      t_shock = 50,
      duration = 300
    ) |> as.data.frame()
    
    out[time_indices, ] |>
      dplyr::mutate(total = rowSums(dplyr::across(dplyr::starts_with("S_")))) |>
      dplyr::select(t, total)
  }, simplify = FALSE)
  
  # Summarize across reps per time point
  bind_rows(sim_outputs, .id = "rep") |>
    dplyr::mutate(rep = as.integer(rep)) |>
    group_by(t) |>
    dplyr::summarise(
      mean_symptom = mean(total),
      loops = loop_count,
      sigma = cal_sd(A_conf)$sumsdStr,
      config_id = i,
      .groups = "keep"
    )
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

# Save results
saveRDS(df_summary, "results/sensitivity_randstr_seed1_net500.rds")
# df_summary <- readRDS("results/sensitivity_randstr_seed1_net500.rds")


df_summary |>
  group_by(loops) |>
  dplyr::summarize(mean = mean(mean_symptom))

## recreate result 
p1 <- df_summary |> 
  ggplot(aes(x = factor(loops), y = mean_symptom, fill= factor(t), color = factor(t))) +
  geom_boxplot(width = .7,
               outlier.alpha = 0.1,
               outlier.size = 0.6,
               outlier.shape = 21,
               outlier.color = NA,
               outlier.fill = NULL,
               alpha = 0.1,
               position = position_dodge(width = 0.8))  +
  scale_fill_manual(values = c("cyan3", "#E7B000", "salmon", "palegreen3", "slateblue3")) +
  scale_colour_manual(values = c("cyan4", "#E7A809", "salmon2", "palegreen4", "slateblue4"), labels = c("t = 400", "t = 800", "t = 1200", "t = 1600", "t = 2000")) +
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level", col = "")  +
  theme_pubr() +
  guides(fill = "none") + 
  theme(legend.position = "bottom",
        # space between legend and plot
        legend.box.spacing = unit(1.3, "cm"),
        text = element_text(size = 17, family="Palatino"),
        legend.text=element_text(size=rel(0.8)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(t = 3, r = 4, b = 1, l = 1, "cm"))

# ggsave("figure/sensitivity_feedbackloop.pdf", plot = p1, width = 23, height = 18, units = "cm", dpi = 300)


## Fig2
# Compute nos1 and nos2 for each configuration in configs
nos_scores <- purrr::map(configs, function(mat) {
  graph <- create_adjacency_list(mat)
  loops <- find_loops(graph, mat)
  loops <- purrr::discard(loops, ~ .x$loop_length == 1)
  
  if (length(loops) == 0) return(tibble::tibble(nos1 = 0, nos2 = 0))
  
  node_counts <- str_extract_all(names(loops), "\\d", simplify = TRUE)[, -1]
  node_table <- table(node_counts)
  
  common_score <- sum(node_table^2)
  n_loop <- length(loops)
  nos1 <- common_score / (n_loop^2)
  nos2 <- sum(as.numeric(node_table) - 1) / n_loop
  
  tibble::tibble(nos1 = nos1, nos2 = nos2)
})

nos_df <- purrr::list_rbind(nos_scores) 

# Add config_id column to nos_df
nos_df <- nos_df |> 
  dplyr::mutate(config_id = seq_len(nrow(nos_df)))

# Join to df_summary by config_id
df_summary2 <- df_summary |>
  dplyr::left_join(nos_df, by = "config_id")


# install.packages("devtools")
library(devtools)
devtools::install_github("johannesbjork/LaCroixColoR")

# 
# # gradient color
# # pal <- wes_palette("Zissou1", 100, type = "continuous")
# pal <- LaCroixColoR::lacroix_palette("PeachPear", n = 10, type = "continuous") |> rev()

fig2 <- df_summary2 |> filter(near(t, 1999.5), loops != 0) |> # decide time points later 
  ggplot(aes(x = sigma , #(1/relstr) * 3,
             y = mean_symptom)) +
  geom_point(aes(col = nos1),#nos1), #jaccard*50), size = 1,
             alpha = 0.7, size = 1.3, shape=20) +
  geom_smooth(method = "loess", linewidth = 0.5, col = alpha("hotpink4", 0.7), se = F, span = 1) +
  
  # scale_color_gradientn(colours = pal) +
  # scale_color_gradientn(colours = alpha(rainbow(10), 0.5)) +
  scale_color_gradientn(colors = c("#172869", "#0A5396", "#037DB9", "#12A0B3", "#48C0AD", "#48C0AD", "#48C0AD", "#BBD9A8", "#E9CD98", "#E9A880", "#F26F44", "#FF3200")) +
  # scale_color_gradient(low = "navy", high = "green") +
  facet_wrap(~factor(loops)) +
  labs(x = expression("Weighted degree variability ("~sigma[tot]~")"), y = "Average aggregated symptom level", color = "Feedback loop\noverlap level") +
  theme_bw() +
  guides(colour=guide_colourbar(barwidth=30,label.position="bottom"))+
  theme(legend.position = "bottom",
        # space between legend and plot
        legend.box.spacing = unit(1.3, "cm"),
        text = element_text(size = 23, family="Palatino"),
        legend.text = element_text(size=rel(0.9)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(1, 2, 1, 1, "cm")) 



## adding overarching label for the facets
# labels 
labelT = "Number of feedback loop"

# get the ggplot grob
z <- ggplotGrob(fig2)

# get the positions of the strips in the gtable: t = top, l = left, ...
posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

# add a new column to the right of current right strips, 
# and a new row on top of current top strips
height <- z$heights[min(posT$t)]  # height of current top strips

z <- gtable_add_rows(z, height, min(posT$t)-1)

# construct the new strip grobs
stripT <- gTree(name = "Strip_top", children = gList(
  rectGrob(gp = gpar(col = "black", fill = "grey85")),
  textGrob(labelT, gp = gpar(fontsize = 22, col = "black", fontfamily = "Palatino"))))

# position the grobs in the gtable
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# add small gaps between strips
z <- gtable_add_rows(z, unit(1/5, "line"), min(posT$t))

# draw it
grid.newpage()
grid.draw(z)


# ggsave("figure/sensitivityanal_var_overlap.pdf", plot = z, width = 35, height = 20, units = "cm", dpi = 300)


## example random network
# pdf("figure/example_random_network.pdf", width = 6, height = 5)
qgraph(A_random, theme = 'colorblind', border.color = 'darkgray', border.width = 2,
       edge.color = "darkgray", edge.width = 0.8, curve = 0.3, curveAll = TRUE,
       label.color = "black", legend.cex = 1.2, asize = 4)
# dev.off()
