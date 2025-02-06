## =========================================================
## Data Analysis - Part 1
##
## This script analyzes the simulated data, focusing on topological features
## such as the number of feedback loops, weighted degree variability, and feedback
## loop overlap level. Based on these analyses, the script generates figures (e.g., 
## Figure 1, Figure 2, Figure 3) to visualize the findings.
## =========================================================


## install packages
source("code/libraries.R")

## source necessary functions
source("code/euler_stochastic2.R")
source("code/mod_specification.R")
source("code/helper_func.R")



# ================================
# Calculate number of feedback loops
# ================================
loop_numbers <- purrr::map_dbl(all_networks, \(network) {
  find_loops(create_adjacency_list(network), network) |> length() - 9 # subtract self-loops
})


# ================================
# Extract loop information and statistics
# ================================
# get loop length and relative weighted length
loop_info <- all_networks |> 
  purrr::map(\(x) find_loops(create_adjacency_list(x), x) |> 
               purrr::list_rbind(names_to = "id") |>
               filter(loop_length != 1) )#  exclude self-loops


# get weighted degree variability
sd_info <- all_networks |>
  purrr::map(\(x) cal_sd(x)) |> list_rbind()

# get cycle path
cycles <- loop_info |>
  purrr::map(\(x) str_extract_all(x$id, "\\d") |>
               map(\(x) as.numeric(x)[-1] |> unlist() )
             )

# weighted degree variability
deg_sd <- sd_info$sumsdStr


# get frequency of node
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


# ================================
# Feedback loop overlap calculations
# ================================
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


# ================================
# Combine results into data frames
# ================================
# original result df
comb_res <- rbind(original_res2, res2) |>
  dplyr::mutate(nloop = rep(loop_numbers, each =5),
                nos1 = rep(nos1, each = 5),
                nos2 = rep(nos2, each = 5)) |>
  pivot_longer(!c(nloop, mat), names_to = "sim", values_to = "value") |>
  dplyr::mutate(matr = stringr::str_extract_all(mat, "\\d+", simplify = T)[,1],
                t = factor(stringr::str_extract_all(mat, "\\d+", simplify = T)[,2], levels = c("400", "800", "1200", "1600", "2000")),
                sim = stringr::str_extract_all(sim, "\\d+", simplify = T),
                # if loop number is higher than 20, then 20
                nloop = ifelse(nloop >= 20, 20, nloop)
  )

# averaged result df
comb_avg_res <- rbind(ori_avg_res, avg_res) |>
  dplyr::mutate(loop_numbers = rep(loop_numbers, each =5),
                nos1 = rep(nos1, each = 5),
                nos2 = rep(nos2, each = 5),
                deg_sd = rep(deg_sd, each = 5)) |>
  # pivot_longer(!c(nloop, mat, common_score), names_to = "sim", values_to = "value") |>
  dplyr::mutate(matr = stringr::str_extract_all(mat, "\\d+", simplify = T)[,1], 
                t = factor(stringr::str_extract_all(mat, "\\d+", simplify = T)[,2], levels = c("400", "800", "1200", "1600", "2000")),
                # if loop number is higher than 20, then 20
                nloop = ifelse(loop_numbers >= 20, "20+", loop_numbers),
                # make sure nos1 == 0 when nloop == 1
                nos1 = ifelse(nloop == 1, 0, nos1),
                group = case_when(
                  nos1 < 0.05 ~ "<0.05",
                  nos1 <0.10 ~ "<0.10",
                  nos1 <0.15 ~ "<0.15",
                  nos1 <0.20 ~ "<0.20",
                  nos1 >= 0.20 ~ "0.20+",
                )
  ) |>
  # remove the same matrices (due to the bidirectional loop: 2^15 * 3 = 98304)
  filter(!as.numeric(matr) %in% dup_ind)

# saveRDS(comb_res, file = "comb_res.rds")
# saveRDS(comb_avg_res, file = "comb_avg_res.rds")

## ================================
## Figure1: number of feedback loop
## ================================
fig1 <- comb_res |> 
  ggplot(aes(x = factor(nloop, labels = c(0:19, "20+")), y = value, fill= t, color = t)) +
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
        text = element_text(size = 23, family="Palatino"),
        legend.text=element_text(size=rel(0.9)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(t = 3, r = 4, b = 1, l = 1, "cm"))


## set up marginal histograms
xdens <-  cowplot::axis_canvas(fig1, axis = "x", ylim = c(0,0.135)) +
  #ggplot() +
  # geom_histogram(data = comb_res, aes(x = nloop, y = after_stat(density)), position = "identity", bins = 23, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(data = comb_res, aes(x = nloop), color = alpha("steelblue4", 0.5), fill = alpha("steelblue4", 0.2), bw = 0.9) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "cm"))

ydens <- cowplot::axis_canvas(fig1, axis = "y", coord_flip = TRUE, xlim = c(0, 0.23))+
  #ggplot() +
  # geom_histogram(data = comb_res, aes(x = nloop, y = after_stat(density)), position = "identity", bins = 23, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(data = comb_res, aes(x = value), color = "seashell3", fill = alpha("seashell", 0.5), bw = 0.9) +
  coord_flip() +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "cm"))

# combine the plots
p2 <- cowplot::insert_xaxis_grob(fig1, xdens, grid::unit(0.2, "null"), position = "top")
p3 <- cowplot::insert_yaxis_grob(p2, ydens, grid::unit(.05, "null"), position = "right")
cowplot::ggdraw(p3) 

# ggsave("figure/feedback_density2.pdf", plot = p3, width = 40, height = 22, units = "cm", dpi = 300)


## ================================
## Figure2: weighted degree variability
## ================================

# get the color palette
# install.packages("devtools")
devtools::install_github("johannesbjork/LaCroixColoR", force = TRUE)

# gradient color
# pal <- wes_palette("Zissou1", 100, type = "continuous")
pal <- LaCroixColoR::lacroix_palette("PeachPear", n = 10, type = "continuous") |> rev()

# boxplot _ number feedback loop
fig2 <- comb_avg_res |> filter(t==1200, nloop!=0) |> # decide time points later 
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

# ggsave("figure/var_overlap.pdf", plot = z, width = 35, height = 30, units = "cm", dpi = 300)


  
## ================================
## Figure3: 3d regression plane 
## ================================

# select relevant columns
selected_data <- comb_avg_res |>
  filter(t == 1200, loop_numbers != 0) |>
  # filter(t == 1200, loop_numbers != 0, loop_numbers <= 20, loop_numbers > 1) |>
  select(deg_sd, nos1, loop_numbers, max_str_sd, avg) 


# set the x, y, and z variables
x <- selected_data$loop_numbers #comb_avg_res$nloop
y <- selected_data$deg_sd #comb_avg_res$nos1
z <- selected_data$avg #comb_avg_res |> filter(t == 1200) |> select(avg) |> unlist()

# compute the linear regression 
fit <- lm(z ~ x * y)
summary(fit)

# loess fit
fit.loess <- loess(z ~ x * y, degree = 2)
summary(fit.loess)

## create a grid from the x and y values (min to max) and predict values for every point
## this will become the regression plane
# create a grid for x and y
grid.lines = 100
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

z.pred.loess <- matrix(predict(fit.loess, newdata = xy), 
                       nrow = grid.lines, ncol = grid.lines)



# create the fitted points for droplines to the surface
# fitpoints <- predict(fit)
dat.sam <- selected_data |> sample_frac(0.05) #balanced_df |> sample_frac(0.3)
x.sam <- dat.sam$loop_numbers
y.sam <- dat.sam$deg_sd 
z.sam <- dat.sam$avg
  
# pdf(file = "figure/3dplot_KP4.pdf", bg = 'transparent', family="Palatino", width = 13, height = 7)

par(mfrow=c(1,2), mar=c(2, 2, 3 ,1.2), oma=c(0,0,1,0))
## 3dplot-1
scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
          theta = -45, phi = 20, bty="b",
          xlab = "Number of feedback loop", ylab = "Weighted degree variability", zlab = "Avg. symptom level",
          cex.lab = 1.55,  cex.main = 1.7, main = "(a)",
          len = 3,
          surf = list(x = x.pred, y = y.pred, z = z.pred.loess, facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.5), 
                      border=alpha("darkgray", .1)))

scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1,
          theta = -45, phi = 20, bty="b", add =T)


## 3dplot-2
scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
          theta = 125, phi = 20, bty="b",
          xlab = "Number of feedback loop", ylab = "Weighted degree variability", zlab = "Avg. symptom level",
          cex.lab = 1.5,  cex.main = 1.7, main = "(b)",
          surf = list(x = x.pred, y = y.pred, z = z.pred.loess, facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.5), 
                      border=alpha("darkgray", .1)))

scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1, 
          theta = 125, phi = 20, bty="b", add =T, cex.symbols = 5,  cex.axis = 5)
# scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1, colvar = 
#           theta = -40, phi = 25, bty="b", add =T, cex.symbols = 5,  cex.axis = 5)

# dev.off()




  
##############
## Try-outs ##
##############

# compute spectral radius
spec_rad <- lapply(all_networks, sparsevar::spectralRadius) |> unlist()

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


# cyclelength <- loop_info |>
#   purrr::map("loop_length") |> purrr::map(\(x) data.frame(Mean = mean(x), SD = sd(x), max = max(x), min = min(x))) |> 
#   list_rbind() |>
#   mutate(ratio = Mean/SD) |>
#   mutate_all(function(x) ifelse(is.infinite(x), 0, x)) |>
#   mutate_all(function(x) ifelse(is.na(x), 0, x)) |>
#   mutate_all(function(x) ifelse(is.nan(x), 0, x)) 


# rel_strength <- loop_info |>  
#   purrr::map(~ {
#     if (length(.x$rel_weighted_length) > 0) {
#       data <- .x$rel_weighted_length
#       tibble(
#         Mean = mean(data, na.rm = TRUE),
#         SD = sd(data, na.rm = TRUE),
#         Max = max(data, na.rm = TRUE),
#         Min = min(data, na.rm = TRUE)
#       )
#     } else {
#       tibble(Mean = NA, SD = NA, Max = NA, Min = NA)
#     }
#   }) |> list_rbind() |>
#   mutate_all(function(x) ifelse(is.na(x), 0, x)) 