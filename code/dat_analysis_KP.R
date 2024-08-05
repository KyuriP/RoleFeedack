
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

source("code/libraries.R")

## source necessary functions
source("code/euler_stochastic2.R")
source("code/mod_specification.R")
source("code/gen_network.R")

# load data
# res <- readRDS("data/res.rds")

res16384 <- readRDS("data/res_16384.rds")
# "mat order" has to be manually edited (previously error)
res32768 <- readRDS("data/res_32768.rds") |> dplyr::mutate(mat = paste0(rep(16385:32768, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res49152 <- readRDS("data/res_49152.rds") |> dplyr::mutate(mat = paste0(rep(32769:49152, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res65536 <- readRDS("data/res_65536.rds") |> dplyr::mutate(mat = paste0(rep(49153:65536, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res81920 <- readRDS("data/res_81920.rds") |> dplyr::mutate(mat = paste0(rep(65537:81920, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res98304 <- readRDS("data/res_98304.rds") |> dplyr::mutate(mat = paste0(rep(81921:98304, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res114688 <- readRDS("data/res_114688.rds") |> dplyr::mutate(mat = paste0(rep(98305:114688, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res131071 <- readRDS("data/res_131071.rds") |> dplyr::mutate(mat = paste0(rep(114689:131071, each = 5),"-", c(400, 800, 1200, 1600, 2000)))

res2 <- res16384 |> bind_rows(res32768, res49152, res65536, res81920, res98304, res114688, res131071)

rm(res16384, res32768, res49152, res65536, res81920, res98304, res114688, res131071)

avg_res <- res2 |>
  rowwise() |>
  mutate(avg = mean(total...1:total...50)) |>
  select(avg, mat) |>
  ungroup() # cancel rowwise

## aggregate symptom level
n_sims <- 50

# run simulation n_sims times
# original sim from Xinhai
# original_res <- map(1:n_sims, ~ euler_stochastic2(
#     Amat = A, 
#     deterministic_rate = mod$dif_eq,
#     stochastic_rate = mod$sto_eq,
#     initial_condition = mod$initial_values,
#     parameters1 = parms1,
#     parameters2 = parms2, 
#     deltaT = deltaT,
#     timelength = timelength,
#     D1 = D_stoeq1,
#     shock = TRUE,
#     t_shock = t_shock, 
#     duration = shock_duration)  |>
#       # due to tiny differences in the underlying floating point representation of the numbers in R
#       dplyr::filter(round(t,1) == 1000.0 | round(t,1) == 2000.0 | round(t,1) == 2999.9) |>
#       dplyr::mutate(total = rowSums(pick(S_anh:S_sui)), .keep = "none")
#     ) |> 
#   purrr::list_cbind() |>
#   dplyr::mutate(mat = paste0(rep(0, each = 3),"-", c(1000, 2000, 3000)))

# 2nd simulation with diff A mat
original_res2 <- purrr::map(1:n_sims, ~ euler_stochastic2(
  Amat = mod$A, 
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
    dplyr::filter(round(t,1) == 400.0 |round(t,1) == 800.0 |round(t,1) == 1200.0 |round(t,1) == 1600.0 | round(t,1) == 1999.9) |>
    dplyr::mutate(total = rowSums(pick(S_anh:S_sui)), .keep = "none")
) |> 
  purrr::list_cbind() |> 
  dplyr::mutate(mat = paste0(rep(0, each = 5),"-", c(400, 800, 1200, 1600, 2000)))

ori_avg_res <- original_res2 |>
  rowwise() |>
  mutate(avg = mean(total...1:total...50)) |>
  select(avg, mat) |>
  ungroup() # cancel rowwise

# extract all networks
all_networks <- generate_configurations(A, modifiable_edges)

# extract all networks
# all_networks_copy <- all_networks

all_networks <- append(list(A), all_networks)

# compute spectral radius
spec_rad <- lapply(all_networks, sparsevar::spectralRadius) |> unlist()

# get the number of loops 
loop_numbers <- c()
for (i in 1:length(all_networks)){
  loop_numbers[i] <- find_loops(create_adjacency_list(all_networks[[i]]), all_networks[[i]]) |> length()
}
loop_numbers <- loop_numbers - 9

 # Function to calculate Jaccard Similarity Index
calculate_jaccard_similarity <- function(cycles) {
  if (length(cycles) <= 1) {
    return(0) # Return zero if there are 0 or 1 cycles
  }
  
  combinations <- utils::combn(seq_along(cycles), 2, simplify = FALSE)
  
  similarities <- purrr::map_dbl(combinations, ~ {
    i <- .x[1]
    j <- .x[2]
    intersection <- length(base::intersect(cycles[[i]], cycles[[j]]))
    union <- length(base::union(cycles[[i]], cycles[[j]]))
    intersection / union
  })
  mean(similarities, na.rm = TRUE)
}


### common nodes in cycle
# get the number of loops 
loop_info <- all_networks |> 
  purrr::map(\(x) find_loops(create_adjacency_list(x), x) |> 
               purrr::list_rbind(names_to = "id") |>
               filter(loop_length != 1) )

sd_info <- all_networks |>
  purrr::map(\(x) cal_sd(x)) |> list_rbind()

cycles <- loop_info |>  
  purrr::map(\(x) str_extract_all(x$id, "\\d") |>
               map(\(x) as.numeric(x)[-1] |> unlist() )
             )

cyclelength <- loop_info |>
  purrr::map("loop_length") |> purrr::map(\(x) data.frame(Mean = mean(x), SD = sd(x), max = max(x), min = min(x))) |> 
  list_rbind() |>
  mutate(ratio = Mean/SD) |>
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) |>
  mutate_all(function(x) ifelse(is.na(x), 0, x)) |>
  mutate_all(function(x) ifelse(is.nan(x), 0, x)) 


rel_strength <- loop_info |>  
  purrr::map(~ {
    if (length(.x$rel_weighted_length) > 0) {
      data <- .x$rel_weighted_length
      tibble(
        Mean = mean(data, na.rm = TRUE),
        SD = sd(data, na.rm = TRUE),
        Max = max(data, na.rm = TRUE),
        Min = min(data, na.rm = TRUE)
      )
    } else {
      tibble(Mean = NA, SD = NA, Max = NA, Min = NA)
    }
  }) |> list_rbind() |>
  mutate_all(function(x) ifelse(is.na(x), 0, x)) 

# rel_strength <- loop_info |>  
#   purrr::map(\(x) x$rel_weighted_length |> as_tibble() |> summarize(Mean = mean(value), SD = sd(value), Max = max(value), Min = min(value))) #|> purrr::map(\(x) data.frame(Mean = mean(x), SD = sd(x), max = max(x), min = min(x))) |> 
#   list_rbind() |>
#   mutate(ratio = Mean/SD) |>
#   # tidyr::replace_na(list(MEAN = 0 , SD = 0)) |>
#   mutate_all(function(x) ifelse(is.infinite(x), 0, x)) |>
#   mutate_all(function(x) ifelse(is.na(x), 0, x)) |>
#   mutate_all(function(x) ifelse(is.nan(x), 0, x)) 
# # rel_strength[is.na(rel_strength)] <- 0  # replace na & nan with 0
# 
# str_ratio <- rel_strength$ratio

max_str_sd <- 1/(rel_strength$Mean * sd_info$sumsdStr)
# max_str_sd <- rel_strength$Mean / sd_info$sumsdStr

deg_sd <- sd_info$sumsdStr

length <- cyclelength$min

spec_rad_sd <- spec_rad / sd_info$sumsdStr

jaccard <- cycles |>
  purrr::map_dbl(~.x |> calculate_jaccard_similarity())


freq_node <- loop_info |> 
  # extract node number without the first node that repeats
  purrr::imap(\(x,y) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
               table(exclude = "") |> as.data.frame() |>
               mutate(nloop = loop_numbers[y])) 
             

freq_node <- loop_info |> 
  purrr::imap(\(x, y) {
    nodes <- str_extract_all(x$id, "\\d", simplify = TRUE)[, -1]
    node_table <- table(nodes)
    if (length(node_table) > 0) {
      node_df <- as.data.frame(node_table)
      colnames(node_df) <- c("Node", "Frequency")
      node_df <- node_df |>
        mutate(nloop = loop_numbers[y])
    } else {
      node_df <- data.frame(Node = character(0), Frequency = numeric(0), nloop = numeric(0))
    }
    return(node_df)
  })


common_cycle <- loop_info |> 
  # extract node number without the first node that repeats
  purrr::map(\(x) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
               table(exclude = "") |> as.data.frame() %>% 
               dplyr::mutate(n_loop =  nrow(x),
                          # normalized overlap score1 = sum(cycle_count-1)^2) / node_num * (cycle_num)^2
                           common_score = sum((Freq)^2) ,
                           nos1 = common_score / ((n_loop)^2),
                          # normalized overlap score 2 = sum(cycle_count - 1) / node_num * (cycle_num)
                           nos2 = sum(Freq - 1) / (n_loop))
  ) 


nos1 <- common_cycle |> purrr::map_dbl(list("nos1", 1), .default = 0)
nos2 <- common_cycle |> purrr::map_dbl(list("nos2", 1), .default = 0)



# add it to the result
# comb_res <- rbind(original_res, res) |>
# mutate(nloop = rep(c(nloop_A, loop_numbers), each =3)) |>
# comb_res <- rbind(original_res2, res2) |>
#   dplyr::mutate(nloop = rep(loop_numbers, each =5),
#          nos1 = rep(nos1, each = 5),
#          nos2 = rep(nos2, each = 5),
#          jaccard = rep(jaccard, each = 5),
#          specrad = rep(spec_rad, each = 5),
#          max_str_var = rep(max_str_var, each = 5)) |>
#   pivot_longer(!c(nloop, mat, common_score), names_to = "sim", values_to = "value") |>
#   dplyr::mutate(matr = stringr::str_extract_all(mat, "\\d+", simplify = T)[,1], 
#          t = factor(stringr::str_extract_all(mat, "\\d+", simplify = T)[,2], levels = c("400", "800", "1200", "1600", "2000")),
#          sim = stringr::str_extract_all(sim, "\\d+", simplify = T),
#          # if loop number is higher than 20, then 20
#          nloop = ifelse(nloop >= 20, 20, nloop)
#          # group = case_when(
#          #   common_score == 0 ~ "0",
#          #   common_score <2 ~ "[1,2)",
#          #   common_score <3 ~ "[2,3)",
#          #   common_score <4 ~ "[3,4)",
#          #   common_score <5 ~ "[4,5)",
#          #   common_score <7 ~ "[5,6.5]")
#   )
#          # group = case_when(
#          #   nloop == 0 ~ "0",
#          #   nloop <= 5 ~ "[1, 5]",
#          #   nloop <= 10 ~ "[6, 10]",
#          #   nloop <= 15 ~ "[11, 15]",
#          #   nloop >= 16 ~ "[16, 20+]"))


comb_avg_res <- rbind(ori_avg_res, avg_res) |>
  dplyr::mutate(nloop = rep(loop_numbers, each =5),
                nos1 = rep(nos1, each = 5),
                nos2 = rep(nos2, each = 5),
                jaccard = rep(jaccard, each = 5),
                specrad = rep(spec_rad, each = 5),
                max_str_sd = rep(max_str_sd, each = 5),
                specrad_sd = rep(spec_rad_sd, each = 5),
                length = rep(length, each = 5),
                deg_sd = rep(deg_sd, each = 5)) |>
  # pivot_longer(!c(nloop, mat, common_score), names_to = "sim", values_to = "value") |>
  dplyr::mutate(matr = stringr::str_extract_all(mat, "\\d+", simplify = T)[,1], 
                t = factor(stringr::str_extract_all(mat, "\\d+", simplify = T)[,2], levels = c("400", "800", "1200", "1600", "2000")),
                # if loop number is higher than 20, then 20
                nloop = ifelse(nloop >= 20, "20+", nloop),
                # make sure nos1 == 0 when nloop == 1
                nos1 = ifelse(nloop == 1, 0, nos1),
                group = case_when(
                  nos1 < 0.05 ~ "<0.05",
                  nos1 <0.10 ~ "<0.10",
                  nos1 <0.15 ~ "<0.15",
                  nos1 <0.20 ~ "<0.20",
                  nos1 >= 0.20 ~ "0.20+",
                )
  )
                # group = case_when(
                #   nloop == 0 ~ "0",
                #   nloop <= 5 ~ "[1, 5]",
                #   nloop <= 10 ~ "[6, 10]",
                #   nloop <= 15 ~ "[11, 15]",
                #   nloop >= 16 ~ "[16, 20+]"))

plot(rel_strength$Mean, sd_info$sumsdStr)
plot(jaccard, deg_sd)
plot(deg_sd, nos2)
plot(comb_avg_res$max_str_sd, comb_avg_res$avg)

comb_avg_res |> filter(t == 2000)
lm(avg ~ nloop + jaccard + max_str_sd, data = comb_avg_res |> filter(t == 2000)) |> summary()
## nos1, relstr!!  --> lrgst effect.


# more color
# library(wesanderson)

# install.packages("devtools")
devtools::install_github("johannesbjork/LaCroixColoR", force = TRUE)

# Gradient color
# pal <- wes_palette("Zissou1", 100, type = "continuous")
pal <- LaCroixColoR::lacroix_palette("PeachPear", n = 10, type = "continuous") |> rev()

# boxplot _ number feedback loop
p2 <- comb_avg_res |> filter(t==1200, nloop!=0) |> # decide time points later 
  ggplot(aes(x = deg_sd , #(1/relstr) * 3,
             y = avg)) +
  geom_point(aes(col = nos1),#nos1), #jaccard*50), size = 1,
             alpha = 0.7, size = 1.3, shape=20) +
  geom_smooth(method = "loess", linewidth = 0.5, col = alpha("hotpink4", 0.7), se = F, span = 1) +
  
  scale_color_gradientn(colours = pal) +
  # scale_color_gradientn(colours = alpha(rainbow(10), 0.5)) +
  # scale_color_gradientn(colors = c("#172869", "#0A5396", "#037DB9", "#12A0B3", "#48C0AD", "#48C0AD", "#48C0AD", "#BBD9A8", "#E9CD98", "#E9A880", "#F26F44", "#FF3200")) +
  # scale_color_gradient(low = "navy", high = "green") +
  facet_wrap(~factor(nloop, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20+")), nrow = 5) +
  labs(x = "Total variability in connection strength", y = "Average aggregated symptom level", color = "Feedback loop\noverlap level") +
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


## Adding overarching label for the facets
# Labels 
labelT = "Number of feedback loop"

# Get the ggplot grob
z <- ggplotGrob(p2)

# Get the positions of the strips in the gtable: t = top, l = left, ...
posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

# Add a new column to the right of current right strips, 
# and a new row on top of current top strips
height <- z$heights[min(posT$t)]  # height of current top strips

z <- gtable_add_rows(z, height, min(posT$t)-1)

# Construct the new strip grobs
stripT <- gTree(name = "Strip_top", children = gList(
  rectGrob(gp = gpar(col = "black", fill = "grey85")),
  textGrob(labelT, gp = gpar(fontsize = 22, col = "black", fontfamily = "Palatino"))))

# Position the grobs in the gtable
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# Add small gaps between strips
z <- gtable_add_rows(z, unit(1/5, "line"), min(posT$t))

# Draw it
grid.newpage()
grid.draw(z)

# ggsave("figure/var_overlap.pdf", plot = z, width = 35, height = 30, units = "cm", dpi = 300)





pmain <- comb_res |> 
  ggplot(aes(x = factor(nloop, labels = c(0:19, "20+")), y = value, fill= t, color = t)) +
  geom_boxplot(width = .7,
               outlier.alpha = 0.1,
               outlier.size = 0.6,
               outlier.shape = 21,
               outlier.color = NA,
               outlier.fill = NULL,
               alpha = 0.1,
               position = position_dodge(width = 0.8))  +
  #geom_point(alpha = .1, position = position_jitterdodge(jitter.width = 0.2)) +
  
  # stat_summary(fun.y = median,
  #               geom = 'line',
  #               aes(group = t, color = t),
  #              alpha = 0.2, linewidth = 1, linetype = 2,
  #              show.legend = F) +
  
  # scale_fill_brewer(palette = "Accent") +
  # scale_color_brewer(palette = "Accent", labels = c("t = 400", "t = 800", "t = 1200", "t = 1600", "t = 2000")) +
  scale_fill_manual(values = c("cyan3", "#E7B000", "salmon", "palegreen3", "slateblue3")) +
  scale_colour_manual(values = c("cyan4", "#E7A809", "salmon2", "palegreen4", "slateblue4"), labels = c("t = 400", "t = 800", "t = 1200", "t = 1600", "t = 2000")) +
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level", col = "")  +
  theme_pubr() +
  guides(fill = "none") + 
  theme(legend.position = "bottom",
        # space between legend and plot
        legend.box.spacing = unit(1.3, "cm"),
        text = element_text(size = 23, family="Palatino"),
        legend.text=element_text(size=rel(0.9)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(t = 3, r = 4, b = 1, l = 1, "cm"))


# set up marginal histograms
loop <- cowplot::axis_canvas(p1, axis = "x")+
  #ggplot() +
  geom_histogram(data = comb_res, aes(x = nloop, y = after_stat(density)), position = "identity", bins = 23, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(size = 0.6,  color = alpha("lightsteelblue4", 0.6), bw = 1) +
  guides(fill = FALSE) +
  theme_void() 

xdens <- cowplot::axis_canvas(pmain, axis = "x")+
  #ggplot() +
  # geom_histogram(data = comb_res, aes(x = nloop, y = after_stat(density)), position = "identity", bins = 23, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(data = comb_res, aes(x = nloop), color = alpha("steelblue4", 0.5), fill = alpha("steelblue4", 0.2), bw = 0.9) +
  theme(plot.margin = margin(t = 2, r = 0, b = 0, l = 0, "cm"))


# q <- loop_numbers |> as.data.frame() |>
#   group_by(loop_numbers) |>
#   dplyr::summarize(n = n())
# 
# loop <- loop_numbers |> as.data.frame() |>
#   ggplot(aes(x = loop_numbers, y = after_stat(density))) +
#   geom_histogram(position = "identity", bins = 28, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
#   geom_density(size = 0.6,  color = alpha("lightsteelblue4", 0.6), bw = 1) +
#   guides(fill = FALSE) +
#   theme_void() 

symptom <- cowplot::axis_canvas(p1, axis = "y", coord_flip = TRUE)+
  # ggplot() +
  geom_histogram(data = comb_res, aes(x = value, y = after_stat(density)), position = "identity", bins = 19, color = alpha("seashell3", 0.5), fill = alpha("seashell", 0.5)) +
  geom_density(size = 0.6,  color = alpha("wheat3", 0.8), bw = 1) +
  guides(fill = FALSE) +
  theme_void() +
  coord_flip()

ydens <- cowplot::axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  #ggplot() +
  # geom_histogram(data = comb_res, aes(x = nloop, y = after_stat(density)), position = "identity", bins = 23, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(data = comb_res, aes(x = nloop), color = "seashell3", fill = alpha("seashell", 0.5), bw = 0.8) +
  coord_flip() +
  theme(plot.margin = margin(t = 0, r = 4, b = 0, l = 0, "cm"))


# # align histograms with scatterplot
# aligned_x_hist <- cowplot::align_plots(loop, p1, align = "v", axis = "t")[[1]]
# aligned_y_hist <- cowplot::align_plots(symptom, p1, align = "h", axis = "b")[[1]]
# 
# 
# # arrange plots
# p_comb_KP <- cowplot::plot_grid(
#   aligned_x_hist
#   , NULL
#   , p1
#   , aligned_y_hist
#   , ncol = 2
#   , nrow = 2
#   , rel_heights = c(0.2, 1)
#   , rel_widths = c(1, 0.2)
# )

p2 <- cowplot::insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p3 <- cowplot::insert_yaxis_grob(p2, ydens, grid::unit(.05, "null"), position = "right")
cowplot::ggdraw(p3) 

# ggsave("figure/feedback_density_KP.pdf", plot = p3, width = 40, height = 22, units = "cm", dpi = 300)




  
  
  
  ## 3d regression plane
  # set the x, y, and z variables
  x <- loop_numbers #comb_avg_res$nloop
  y <- nos1#comb_avg_res$nos1
  z <- comb_avg_res |> filter(t == 1200) |> select(avg) |> unlist()

    # compute the linear regression 
  fit <- lm(z ~ x * y)
  summary(fit)
  
  # loess fit
  fit.loess <- loess(z ~ x * y, span = 0.85, degree = 2)
  summary(fit.loess)
  
  # create a grid from the x and y values (min to max) and predict values for every point
  # this will become the regression plane
  grid.lines = 50
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  y.pred <- seq(min(y), max(y), length.out = grid.lines)
  xy <- expand.grid(x = x.pred, y = y.pred)
  z.pred <- matrix(predict(fit, newdata = xy), 
                   nrow = grid.lines, ncol = grid.lines)
  
  z.pred.loess <- matrix(predict(fit.loess, newdata = xy), 
                         nrow = grid.lines, ncol = grid.lines)
  
  # create the fitted points for droplines to the surface
  # fitpoints <- predict(fit)
  dat.sam <- dat |> sample_frac(0.1)
  x.sam <- dat.sam$loop_count 
  y.sam <- dat.sam$var_in_degree 
  z.sam <- dat.sam$average_sum 
  
  
  
  
  # pdf(file = "figure/3dplot.pdf", bg = 'transparent', family="Palatino", width = 13, height = 7)
  
  par(mfrow=c(1,2), mar=c(0, 2, 3 ,1.2), oma=c(0,0,1,0))

    # 3dplot-1
  scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
            theta = 60, phi = 25, bty="b",
            xlab = "Number of feedback loop", ylab = "Var(C)", zlab = "Avg. aggregated symptom level",
            cex.lab = 1.5,  cex.main = 1.7, main = "(a)",
            surf = list(x = x.pred, y = y.pred, z = z.pred.loess, facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.5), 
                        border=alpha("darkgray", .1)))
  
  scatter3D(x, y, z, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1, colvar = 
              theta = -40, phi = 25, bty="b", add =T, cex.symbols = 5,  cex.axis = 5)
  # scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1, colvar = 
  #           theta = -40, phi = 25, bty="b", add =T, cex.symbols = 5,  cex.axis = 5)
  
  # 3dplot-2
  scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
            theta = 30, phi = 25, bty="b",
            xlab = "Number of feedback loop", ylab = "Var(C)", zlab = "Avg. aggregated symptom level",
            cex.lab = 1.55,  cex.main = 1.7, main = "(b)",
            surf = list(x = x.pred, y = y.pred, z = z.pred.loess, facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.5), 
                        border=alpha("darkgray", .1)))
  
  scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1,
            theta = 30, phi = 25, bty="b", add =T)
  
  # dev.off()
  
  