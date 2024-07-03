
library(dplyr)
library(ggplot2)
library(ggridges) # geom_ridges
library(PupillometryR) # geom_flat_violin
library(ggthemes)
library(ggpubr) 
library(plot3D) # for 3d regression plane
library(ggExtra) # for adding marginal distributions
library(grid) # for adding the overarching facet title
library(gtable) # for adding the overarching facet title

# load data
res <- readRDS("data/res.rds")


## aggregate symptom level
n_sims <- 100

# run simulation n_sims times

original_res <- map(1:n_sims, ~ euler_stochastic2(
    Amat = A, 
    deterministic_rate = mod$dif_eq,
    stochastic_rate = mod$sto_eq,
    initial_condition = mod$initial_values,
    parameters1 = parms1,
    parameters2 = parms2, 
    deltaT = deltaT,
    timelength = timelength,
    D1 = D_stoeq1,
    shock = TRUE,
    t_shock = t_shock, 
    duration = shock_duration)  |>
      # due to tiny differences in the underlying floating point representation of the numbers in R
      dplyr::filter(round(t,1) == 1000.0 | round(t,1) == 2000.0 | round(t,1) == 2999.9) |>
      dplyr::mutate(total = rowSums(pick(S_anh:S_sui)), .keep = "none")
    ) |> 
  purrr::list_cbind() |>
  dplyr::mutate(mat = paste0(rep(0, each = 3),"-", c(1000, 2000, 3000)))


# get the number of loops 
loop_numbers <- c()
for (i in 1:length(all_networks)){
  loop_numbers[i] <- find_loops(create_adjacency_list(all_networks[[i]]), all_networks[[i]]) |> length()
}

nloop_A <- find_loops(create_adjacency_list(A), A) |> length()

# add it to the result
comb_res <- rbind(original_res, res) |>
  mutate(nloop = rep(c(nloop_A, loop_numbers), each =3)) |>
  pivot_longer(!c(nloop, mat), names_to = "sim", values_to = "value") |>
  mutate(matr = stringr::str_extract_all(mat, "\\d+", simplify = T)[,1], 
         t = stringr::str_extract_all(mat, "\\d+", simplify = T)[,2],
         sim = stringr::str_extract_all(sim, "\\d+", simplify = T),
         group = case_when(
           nloop < 18 ~ "[15, 18)",
           nloop < 21 ~ "[18, 21)",
           nloop < 24 ~ "[21, 24)",
           nloop < 27 ~ "[24, 27)",
           nloop < 30 ~ "[27, 30)",
           nloop < 33 ~ "[30, 33)",
           nloop < 36 ~ "[33, 36)",
           nloop < 39 ~ "[36, 39)",
           nloop < 42 ~ "[39, 42)",
           nloop < 45 ~ "[42, 45)",
           nloop < 48 ~ "[45, 48)",
           nloop < 52 ~ "[48, 52)",
           nloop < 55 ~ "[52, 55)",
           nloop < 58 ~ "[55, 58)",
           nloop <= 61 ~ "[58, 61]"))


# boxplot _ number feedback loop
comb_res |> 
  ggplot() +
  aes(x = factor(nloop), 
      y = value, fill = t) +
  # geom_point(aes(color = t),
  #            position = position_jitter(w = .2),
  #            size = 1,
  #            alpha = 0.2,
  #            show.legend = F) +
  geom_boxplot(width = .2, 
               outlier.alpha = 0.1,
               #outlier.size = 0.5,
               outlier.size = 0,
               alpha = 0.2) +
  # geom_flat_violin(position = position_nudge(x = .2),
  #                  alpha = 0.4,
  #                  adjust = 1.1,
  #                  trim = T) +
  facet_wrap(~t)
  # facet_wrap(~t, labeller = labeller(t = c("t1500" = "t = 1500",
  #                                          "t2000" = "t = 2000",
  #                                          "t2500" = "t = 2500",
  #                                          "t3000" = "t = 3500"))) +
  # labs(x = "Number of feedback loop", y = "Average aggregated symptom level") +
  # # scale_fill_brewer(palette = "Set3") +
  # # scale_color_brewer(palette = "Set3") +
  # theme_bw() +
  # theme(legend.position = "none",
  #       text = element_text(size = 20, family="Palatino"),
  #       axis.title.y = element_text(vjust = +3),
  #       axis.title.x = element_text(vjust = -0.75),
  #       plot.margin = margin(1, 2, 1, 1, "cm"))




p1 <- comb_res |> 
  ggplot(aes(x = group, y = value, fill= t, color = t)) +
  geom_boxplot(width = .4,
               outlier.alpha = 0.1,
               outlier.size = 0.7,
               outlier.shape = 21,
               outlier.color = NA,
               outlier.fill = NULL,
               alpha = 0.1,
               position = position_dodge(width = 0.6))  +
  #geom_point(alpha = .1, position = position_jitterdodge(jitter.width = 0.2)) +
  
  # stat_summary(fun.y = median,
  #               geom = 'line',
  #               aes(group = t, color = t),
  #              alpha = 0.2, linewidth = 1, linetype = 2,
  #              show.legend = F) +
  
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2", labels = c("t = 1000", "t = 2000", "t = 3000")) +
  # scale_fill_manual(values = c("darkgreen", "goldenrod3", "#00b5e7", "brown4")) +
  # scale_colour_manual(values = c("darkolivegreen", "goldenrod3", "lightskyblue4", "brown4"), labels = c("t = 1500", "t = 2000", "t = 2500", "t = 3000")) +
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level", col = "") +
  theme_pubr() +
  guides(fill = "none") + 
  theme(legend.position = "bottom",
        # space between legend and plot
        legend.box.spacing = unit(1.3, "cm"),
        text = element_text(size = 23, family="Palatino"),
        legend.text=element_text(size=rel(0.9)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(t = -0.1, r = -0.1, b = 1, l = 1, "cm"))


# set up marginal histograms
loop <- comb_res |>
  ggplot(aes(x = nloop, y = after_stat(density))) +
  geom_histogram(position = "identity", bins = 47, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(size = 0.6,  color = alpha("lightsteelblue4", 0.6), bw = 1) +
  guides(fill = FALSE) +
  theme_void() 

symptom <- comb_res |>
  ggplot(aes(x = value, y = after_stat(density))) +
  geom_histogram(position = "identity", bins = 19, color = alpha("seashell3", 0.5), fill = alpha("seashell", 0.5)) +
  geom_density(size = 0.6,  color = alpha("wheat3", 0.8), bw = 1) +
  guides(fill = FALSE) +
  theme_void() +
  coord_flip()


# align histograms with scatterplot
aligned_x_hist <- cowplot::align_plots(loop, p1, align = "v", axis = "t")[[1]]
aligned_y_hist <- cowplot::align_plots(symptom, p1, align = "h", axis = "b")[[1]]


# arrange plots
p_comb_KP <- cowplot::plot_grid(
  aligned_x_hist
  , NULL
  , p1
  , aligned_y_hist
  , ncol = 2
  , nrow = 2
  , rel_heights = c(0.2, 1)
  , rel_widths = c(1, 0.2)
)

# ggsave("figure/feedback_density_KP.pdf", plot = p_comb_KP, width = 54, height = 25, units = "cm", dpi = 300)
