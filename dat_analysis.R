
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
t1500 <- read.csv("data/simulation_result_t1500.csv") |> select(loop_count, average_sum, var_in_degree)
t2000 <- read.csv("data/simulation_result_t2000.csv") |> select(loop_count, average_sum, var_in_degree)
t2500 <- read.csv("data/simulation_result_t2500.csv") |> select(loop_count, average_sum, var_in_degree)
t3000 <- read.csv("data/simulation_result_t3000.csv") |> select(loop_count, average_sum, var_in_degree)

dat <- bind_rows(list(t1500 = t1500, t2000 = t2000, t2500 = t2500, t3000 = t3000), .id = "t") |>
  mutate(group = case_when(
    loop_count < 20 ~ "[15, 20)",
    loop_count < 25 ~ "[20, 25)",
    loop_count < 30 ~ "[25, 30)",
    loop_count < 35 ~ "[30, 35)",
    loop_count < 40 ~ "[35, 40)",
    loop_count < 45 ~ "[40, 45)",
    loop_count < 50 ~ "[45, 50)",
    loop_count < 55 ~ "[50, 55)",
    loop_count <= 61 ~ "[55, 61)"))
    

# boxplot _ number feedback loop
p1 <- dat |> 
ggplot() +
  aes(x = group, 
      y = average_sum, fill = t) +
  geom_point(aes(color = t),
             position = position_jitter(w = .2),
             size = 1,
             alpha = 0.2,
             show.legend = F) +
  geom_boxplot(width = .2, 
               outlier.alpha = 0.1,
               #outlier.size = 0.5,
               outlier.size = 0,
               alpha = 0.2) +
  geom_flat_violin(position = position_nudge(x = .2),
                   alpha = 0.4,
                   adjust = 1.1,
                   trim = T) +
  facet_wrap(~t, labeller = labeller(t = c("t1500" = "t = 1500",
                                           "t2000" = "t = 2000",
                                           "t2500" = "t = 2500",
                                           "t3000" = "t = 3500"))) +
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level") +
  # scale_fill_brewer(palette = "Set3") +
  # scale_color_brewer(palette = "Set3") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20, family="Palatino"),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(1, 2, 1, 1, "cm"))





p1 <- dat |> #filter(t != "t1500") |>
  ggplot(aes(x = group, y = average_sum, fill= t, color = t)) +
  geom_boxplot(width = .4,
               outlier.alpha = 0.1,
               outlier.size = 2,
               outlier.shape = 21,
               outlier.color = NA,
               outlier.fill = NULL,
               alpha = 0.1,
               position = position_dodge(width = 0.6)) +
  #geom_point(alpha = .1, position = position_jitterdodge(jitter.width = 0.2)) +
  
  # stat_summary(fun.y = median,
  #               geom = 'line',
  #               aes(group = t, color = t),
  #              alpha = 0.2, linewidth = 1, linetype = 2,
  #              show.legend = F) +

  # scale_fill_brewer(palette = "Set2") +
  # scale_color_brewer(palette = "Set2") +
  scale_fill_manual(values = c("#97c225", "#E7B800", "#00b5e7", "indianred3")) +
  scale_colour_manual(values = c("darkolivegreen", "orange2", "lightskyblue4", "indianred3"), labels = c("t = 1500", "t = 2000", "t = 2500", "t = 3000")) +
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
loop <- dat |>
  ggplot(aes(x = loop_count, y = after_stat(density))) +
  geom_histogram(position = "identity", bins = 47, color = alpha("azure3", 0.5), fill = alpha("lightskyblue3", 0.2)) +
  geom_density(size = 0.6,  color = alpha("lightsteelblue4", 0.6)) +
  guides(fill = FALSE) +
  theme_void() 

symptom <- dat |>
  ggplot(aes(x = average_sum, y = after_stat(density))) +
  geom_histogram(position = "identity", bins = 25, color = alpha("seashell3", 0.5), fill = alpha("seashell", 0.5)) +
  geom_density(size = 0.6,  color = alpha("wheat3", 0.8)) +
  guides(fill = FALSE) +
  theme_void() +
  coord_flip()


# align histograms with scatterplot
aligned_x_hist <- cowplot::align_plots(loop, p1, align = "v", axis = "t")[[1]]
aligned_y_hist <- cowplot::align_plots(symptom, p1, align = "h", axis = "b")[[1]]


# arrange plots
p_comb <- cowplot::plot_grid(
  aligned_x_hist
  , NULL
  , p1
  , aligned_y_hist
  , ncol = 2
  , nrow = 2
  , rel_heights = c(0.2, 1)
  , rel_widths = c(1, 0.2)
)

# ggsave("figure/feedback_density.pdf", plot = p_comb, width = 35, height = 20, units = "cm", dpi = 300)


## variance in-degree plot
dat |> 
  ggplot() +
  aes(x = var_in_degree, 
      y = average_sum, fill = t) +
  geom_point(aes(color = t),
             position = position_jitter(w = .2),
             size = 1,
             alpha = 0.2,
             show.legend = F) +
  # geom_boxplot(#width = .2,
  #              outlier.alpha = 0.1,
  #              #outlier.size = 0.5,
  #              outlier.size = 0,
  #              alpha = 0.2) +
  geom_flat_violin(position = position_nudge(x = .2),
                   alpha = 0.4,
                   adjust = 1.1,
                   trim = T) +
  # facet_wrap(~t, labeller = labeller(t = c("t1500" = "t = 1500",
  #                                          "t2000" = "t = 2000",
  #                                          "t2500" = "t = 2500",
  #                                          "t3000" = "t = 3500"))) +
  facet_wrap(~group + t, ncol = 4, labeller = labeller(.multi_line = F)) + 
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level") +
  # scale_fill_brewer(palette = "Set3") +
  # scale_color_brewer(palette = "Set3") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 23, family="Palatino"),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(1, 2, 1, 1, "cm"))


p2 <- dat |> filter(t == "t2000" |t == "t3000") |>
 #mutate(group2 = case_when(group == "[15, 20)" ~ "Feedback number", group == "[20, 25)" ~ "Feedback number", group == "[25, 30)" ~ "Feedback number", .default =  NULL)) |>
  ggplot() +
  aes(x = var_in_degree, 
      y = average_sum, fill = t) +
  geom_point(aes(color = t),
             position = position_jitter(w = .2),
             size = 1,
             alpha = 0.2,
             show.legend = F) +
  geom_flat_violin(position = position_nudge(x = 1),
                   alpha = 0.4,
                   adjust = 1.1,
                   trim = T,
                   color = "darkgray") +
  facet_wrap(~group , ncol = 3) +
 # facet_wrap(~group , ncol = 3, nrow = 3, labeller = labeller(group = ~paste("#loop = ", .x))) +
  labs(x = "Var(C)", y = "Average aggregated symptom level") +
  scale_fill_brewer(palette = "Dark2", labels = c("t = 2000", "t = 3000")) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
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
  textGrob(labelT, gp = gpar(fontsize = 20, col = "black", fontfamily = "Palatino"))))

# Position the grobs in the gtable
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# Add small gaps between strips
z <- gtable_add_rows(z, unit(1/5, "line"), min(posT$t))

# Draw it
grid.newpage()
grid.draw(z)

# ggsave("figure/var_instrength.pdf", plot = z, width = 30, height = 30, units = "cm", dpi = 300)


plot(dat$var_in_degree, dat$average_sum)

## 3d regression plane
# set the x, y, and z variables
x <- dat$loop_count
y <- dat$var_in_degree
z <- dat$average_sum

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

# scatter plot with regression plane
# # pdf(file = "3dplot.pdf", bg = 'transparent', family="Palatino")
# scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
#           theta = -55, phi = 25, bty="b",
#           xlab = "Number of feedback loop", ylab = "Var(C)", zlab = "Average aggregated symptom level",  
#           surf = list(x = x.pred, y = y.pred, z = z.pred.loess,  
#                       facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.4), border=alpha("darkgray", .1)))
# 
# scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1,
#           theta = -55, phi = 25, bty="b",
#           xlab = "Number of feedback loop", ylab = "Var(C)", zlab = "Average aggregated symptom level", add =T)
# 
# # dev.off()




# pdf(file = "3dplot.pdf", bg = 'transparent', family="Palatino", width = 13, height = 6)

par(mfrow=c(1,2), mar=c(0, 1.2, 0 ,1.2))

# 3dplot-1
scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
          theta = -40, phi = 25, bty="b",
          xlab = "Number of feedback loop", ylab = "Var(C)", zlab = "Avg. aggregated symptom level",  cex.lab = 1.5, cex.symbols = 5,  cex.axis = 5, lwd.axis = 5,
          surf = list(x = x.pred, y = y.pred, z = z.pred.loess,  
                      facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.5), border=alpha("darkgray", .1)))

scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1,
          theta = -40, phi = 25, bty="b", add =T, cex.symbols = 5,  cex.axis = 5)

# 3dplot-2
scatter3D(x, y, z, pch = 20, cex = .8, colvar = NULL, col=NULL, alpha = 0,
          theta = 30, phi = 25, bty="b",
          xlab = "Number of feedback loop", ylab = "Var(C)", zlab = "Avg. aggregated symptom level",   cex.lab = 1.5,
          surf = list(x = x.pred, y = y.pred, z = z.pred.loess,  
                      facets = TRUE,  col=ramp.col(col = c("darkseagreen4","khaki"), n = 300, alpha=0.5), border=alpha("darkgray", .1)))

scatter3D(x.sam, y.sam, z.sam, pch = 20, cex = .9, colvar = NULL, col="dodgerblue4", alpha = 0.1,
          theta = 30, phi = 25, bty="b", add =T)

# dev.off()


