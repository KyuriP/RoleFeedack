# ===================================================
# Sensitivity Analysis: Structural Robustness via Random Networks
# ===================================================
# This script performs a structural sensitivity analysis to assess how 
# the relationship between feedback loops and symptom persistence 
# generalizes across diverse network topologies.


# ---------------------------------------------------
# Load functions and parallel backend
# ---------------------------------------------------
source("code/mod_specification.R")
source("code/euler_stochastic2.R")
source("code/helper_func.R")
source("code/libraries.R")

plan(multisession, workers = 12)  # enable parallelism


# ---------------------------------------------------
# Parameters
# ---------------------------------------------------
n_random_networks <- 5        # Number of random base networks
n_nodes <- 9                  # Number of nodes (symptoms)
n_edges <- 16                 # Number of directed edges
max_per_group <- 500          # Max configs per loop count
sim_per_net <- 30             # Simulations per configuration
deltaT <- 0.5                 # Time step
timelength <- 2000            # Simulation length
target_loops <- c(0, 3, 6, 9, 12, 15, 18)  # Loop count groups
target_times <- c(400, 800, 1200, 1600, 1999.5)
time_indices <- as.integer(target_times / deltaT + 1)


# set seed for reproducibility
set.seed(123)


# ---------------------------------------------------
# Function: Generate a Random Weighted Network
# ---------------------------------------------------
generate_random_weighted_network <- function(n_nodes = 9, n_edges = 16, 
                                             weight_range = c(0.1, 0.4), 
                                             self_loop_value = 0.3, 
                                             seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  A <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  diag(A) <- self_loop_value  # Constant self-loop on each node
  
  off_diag_indices <- which(row(A) != col(A), arr.ind = TRUE)
  selected_edges <- off_diag_indices[sample(nrow(off_diag_indices), n_edges), ]
  rand_weights <- runif(n_edges, weight_range[1], weight_range[2])
  
  for (k in seq_len(n_edges)) {
    i <- selected_edges[k, "row"]
    j <- selected_edges[k, "col"]
    A[i, j] <- rand_weights[k]
  }
  
  var_names <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")
  rownames(A) <- colnames(A) <- var_names
  
  return(A)
}


# ---------------------------------------------------
# Function: Run Full Analysis for One Base Network
# ---------------------------------------------------
# Generate random weighted network
run_one_random_network_analysis <- function(seed_id) {
  A_random <- generate_random_weighted_network(n_nodes, n_edges, c(0.1, 0.4), self_loop_value = 0.3, seed = seed_id)
  modifiable_edges <- which(A_random > 0 & row(A_random) != col(A_random), arr.ind = TRUE)
  modifiable_edges <- lapply(seq_len(nrow(modifiable_edges)), function(i) modifiable_edges[i, ])
  # Generate all possible directional configurations
  all_configs <- generate_configurations(A_random, modifiable_edges)
  loop_numbers <- map_dbl(all_configs, \(net) find_loops(create_adjacency_list(net), net) |> length() - 9)
  # Subsample configurations by feedback loop count
  selected_configs <- withr::with_seed(seed_id, {
    unlist(lapply(target_loops, function(k) {
      idx <- which(loop_numbers == k)
      if (length(idx) == 0) return(integer(0))
      sample(idx, min(max_per_group, length(idx)))
    }), use.names = FALSE)
  })
  
  configs <- all_configs[selected_configs]
  loop_counts <- loop_numbers[selected_configs]
  
  # Run SDE simulations and aggregate symptom levels
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
        mutate(total = rowSums(across(starts_with("S_")))) |>
        select(t, total)
    }, simplify = FALSE)
    
    bind_rows(sim_outputs, .id = "rep") |>
      mutate(rep = as.integer(rep)) |>
      group_by(t) |>
      summarise(
        mean_symptom = mean(total),
        loops = loop_count,
        sigma = cal_sd(A_conf)$sumsdStr,
        config_id = i,
        seed = seed_id,
        .groups = "keep"
      )
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  # Calculate loop overlap scores
  nos_scores <- map(configs, function(mat) {
    graph <- create_adjacency_list(mat)
    loops <- find_loops(graph, mat)
    loops <- discard(loops, ~ .x$loop_length == 1)
    
    if (length(loops) == 0) return(tibble(nos1 = 0, nos2 = 0))
    
    node_counts <- str_extract_all(names(loops), "\\d", simplify = TRUE)[, -1]
    node_table <- table(node_counts)
    
    common_score <- sum(node_table^2)
    n_loop <- length(loops)
    tibble(nos1 = common_score / (n_loop^2), nos2 = sum(as.numeric(node_table) - 1) / n_loop)
  })
  
  # Combine simulation and loop metrics
  nos_df <- list_rbind(nos_scores)
  nos_df <- mutate(nos_df, config_id = seq_len(nrow(nos_df)))
  
  # Return joined result
  df_out <- left_join(df_summary, nos_df, by = "config_id")
  return(df_out)
}


# ---------------------------------------------------
# Run analysis across multiple random base networks
# ---------------------------------------------------
set.seed(123)  # top-level seed for reproducibility
seeds <- sample(1:1e6, n_random_networks)  # fixed list of unique seeds

df_summary_all <- map_dfr(seeds, run_one_random_network_analysis)

# Save combined results
# saveRDS(df_summary_all, "results/sensitivity_multiple_random_networks.rds")

# ---------------------------------------------------
# Plot 1: Symptom levels by feedback loop count
# ---------------------------------------------------
p1 <- df_summary_all |> 
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

# ggsave("figure/sensitivity_feedbackloop2.pdf", plot = p1, width = 23, height = 18, units = "cm", dpi = 300)



# ---------------------------------------------------
# Plot 2: Effect of overlap and connectivity variability (sigma)
# ---------------------------------------------------
p2 <-df_summary_all |> filter(near(t, 1600), loops != 0) |> 
  ggplot(aes(x = sigma , 
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
z <- ggplotGrob(p2)

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

# ggsave("figure/sensitivityanal_var_overlap2.pdf", plot = z, width = 35, height = 20, units = "cm", dpi = 300)




# ---------------------------------------------------
# Example base network visualization (Appendix Fig D1)
# ---------------------------------------------------
A_random <- generate_random_weighted_network(
  n_nodes = 9,
  n_edges = 16,                  # same as in A_orig
  weight_range = c(0.1, 0.4),
  self_loop_value = 0.3,
  seed = 1 # change seed as you wish
)

# pdf("figure/example_random_network.pdf", width = 6, height = 5)
qgraph(A_random, theme = 'colorblind', border.color = 'darkgray', border.width = 2,
       edge.color = "darkgray", edge.width = 0.8, curve = 0.3, curveAll = TRUE,
       label.color = "black", legend.cex = 1.2, asize = 4)
# dev.off()