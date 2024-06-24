
library(dplyr)
library(ggplot2)
library(ggridges) # geom_ridges
library(PupillometryR) # geom_flat_violin
library(ggthemes)
library(ggpubr) 
library(plot3D) # for 3d regression plane

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
               outlier.alpha = 0.7,
               outlier.size = 2,
               outlier.shape = 21,
               outlier.color = "white",
               outlier.fill = NULL,
               alpha = 0.3,
               position = position_dodge(width = 0.6)) +
  #geom_point(alpha = .1, position = position_jitterdodge(jitter.width = 0.2)) +
  
  # stat_summary(fun.y = median,
  #               geom = 'line',
  #               aes(group = t, color = t),
  #              alpha = 0.25, linewidth = 5, linetype = 1,
  #              show.legend = F) +

  # scale_fill_brewer(palette = "Set2") +
  # scale_color_brewer(palette = "Set2") +
  scale_fill_manual(values = c("darkolivegreen", "orange2", "lightskyblue4", "indianred3"),labels = c("t = 1500", "t = 2000", "t = 2500", "t = 3000")) +
  scale_colour_manual(values = c("darkolivegreen", "orange2", "lightskyblue4", "indianred3")) +
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level", fill = "") +
  theme_pubr() +
  guides(color = "none") + 
  theme(legend.position = "top",
        text = element_text(size = 25, family="Palatino"),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(1, 2, 1, 1, "cm"))

# ggsave("figure/feedback_num.pdf", plot = p1, width = 35, height = 20, units = "cm", dpi = 300)


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
        text = element_text(size = 20, family="Palatino"),
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
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 20, family="Palatino"),
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



## 3d regression plane
# set the x, y, and z variables
x <- dat$loop_count
y <- dat$var_in_degree
z <- dat$average_sum

# Compute the linear regression 
fit <- lm(z ~ x * y)
summary(fit)
# create a grid from the x and y values (min to max) and predict values for every point
# this will become the regression plane
grid.lines = 40
# x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# create the fitted points for droplines to the surface
fitpoints <- predict(fit)


# scatter plot with regression plane
scatter3D(x, y, z, pch = 19, cex = 0.2, colvar = NULL, col="salmon", alpha = 0.1,
          theta = 30, phi = 35, bty="b",
          xlab = "loop number", ylab = "var(indeg)", zlab = "avg.symptom",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = TRUE,  col=ramp.col (col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.5), border="black"), main = "Average aggregated symptom level")



# scatterplot 3d
library(rgl)

plot3d(dat$loop_count, dat$var_in_degree, dat$average_sum,
        xlab = "loop count", ylab = "var", zlab = "symptom",
        type = "s", 
        size = 1.5,
        col = "red")
