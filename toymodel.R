# load packages
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(qgraph)
library(bootnet)
library(ggpubr)
library(magrittr)
library(haven)
library(stringr)


## weigthed adjacency matrix
A <- matrix(c( .30, 0, 0, 0, 0, 0, 0, 0, 0,
               .33, .30, .14, .15, 0, .13, 0, 0, .15,
               .13, .14, .30, .22, .23, 0, 0, 0, 0,
               .21, .15, .22, .30, 0, 0, .12, 0, 0,
               0, 0, 0, .17, .30, 0, 0, 0, 0,
               0, .13, 0, 0, .15, .30, .2, .15, .22,
               0, 0, 0, 0, 0, 0, .30, .17, 0,
               0, 0, 0, 0, 0, 0, 0, .30, 0,
               0, 0, 0, 0, 0, 0, 0, .3, 0.30), 9, 9, byrow = T)
rownames(A) <- colnames(A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

# for legend in the example network plot
Names <- c("anhedonia", "sadness", "sleep", "energy", "appetite", "guilty", "concentration", "motor", "suicidal")
# grouping str
grp <- list(`feedback loop` = 2:6, `no feedback loop` = c(1,7:9))
# layout
manual_layout <- matrix(c( -1.00000000,-0.3697451,
                           -0.25362943,-0.4206165,
                           -0.86442347, 0.4941126,
                           -0.08080896, 0.3245454,
                           0.49177041, 0.9000000,
                           0.43942364,-0.1656119,
                           0.99799343, 0.2837986,
                           1.00000000,-0.7135021,
                           0.41570786,-1.0000000), 9, 2, byrow=T)

# save plot (Fig1)
pdf(file = "toymodel-bi2.pdf", width=16, height=12, bg = 'transparent', family="Palatino")

qgraph(A, theme = 'colorblind', groups = grp, color = c("white", "white"), nodeNames = Names, border.color = c("darkgray", "#F5651C", "#F5651C", "#F5651C", "#F5651C", "#F5651C", "darkgray", "darkgray", "darkgray"),border.width = 2, edge.color = c("#A9A9A9FF", "#DDDDDDFF", "#C8C8C8FF", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#F8a365", "#E0E0E0FF", "#CBCBCBFF", "#D8D8D8FF", "#D3D3D3FF", "#B1B1B1FF", "#D8D8D8FF", "#C6C6C6FF"), edge.width = 0.8, curve = 0.3, curveAll = T, label.color = "black", legend.cex = 1.2, asize= 4, layout = manual_layout, bidirectional = TRUE)

dev.off()



# example plot for illustrating algorithm
ex1 <- matrix(c(0, 1, 1, 1, 0, 1, 0, 0, 0), byrow = T, nrow = 3, ncol =3)
ex2 <- matrix(c(0, 1, 1, 1, 0, 0, 0, 1, 0), byrow = T, nrow = 3, ncol =3)
ex3 <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1, 0), byrow = T, nrow = 3, ncol =3)
ex4 <- matrix(c(0, 1, 0, 1, 0, 1, 1, 0, 0), byrow = T, nrow = 3, ncol =3)
colnames(ex1) <- colnames(ex2) <- colnames(ex3) <- colnames(ex4) <- c("A", "B", "C")


# 1 by 4 layout
pdf(file = "example_net.pdf", width=50, height=10, bg = 'transparent', family="Palatino")

par(mfrow=c(1,4), mar=c(1,1,1,1), oma=c(10,0,0,0))
qgraph(ex1, layout = "circle", asize= 4, edge.width = 6, vsize = 20, label.cex = 1.3, title = "(a)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3))
mtext(expression(A %->% B), side = 1, line = 7, cex = 4, xpd=TRUE)
qgraph(ex2, layout = "circle", asize= 4, edge.width = 6, vsize = 20, label.cex = 1.3, title = "(b)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3))
mtext(expression(atop(A %->% B), A %->% C %->% B), side = 1, line = 7, cex = 4, xpd=TRUE)
qgraph(ex3, layout = "circle", asize= 4, edge.width = 6, vsize = 20, label.cex = 1.3, title = "(c)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3))
mtext(expression(A %->% B), side = 1, line = 7, cex = 4, xpd=TRUE)
qgraph(ex4, layout = "circle", asize= 4, edge.width = 6, vsize = 20, label.cex = 1.3, title = "(d)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3))
mtext(expression(atop(A %->% B), A %->% B %->% C), side = 1, line = 7, cex = 4, xpd=TRUE)


dev.off()


# 2 by 2 layout
pdf(file = "example_net2.pdf", width=35, height=35, bg = 'transparent', family="Palatino")

par(mfrow=c(2,2), mar=c(40,50,30,50), oma=c(10,0,0,0))
qgraph(ex1, layout = "circle", asize= 4, edge.width = 6, vsize = 10, label.cex = 1.3, title = "(a)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3), bidirectional=TRUE)
mtext(expression(A %->% B), side = 1, line = 7, cex = 5, xpd=TRUE)
qgraph(ex2, layout = "circle", asize= 4, edge.width = 6, vsize = 10, label.cex = 1.3, title = "(b)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3), bidirectional=TRUE)
mtext(expression(atop(A %->% B), A %->% C %->% B), side = 1, line = 7, cex = 5, xpd=TRUE)
qgraph(ex3, layout = "circle", asize= 4, edge.width = 6, vsize = 10, label.cex = 1.3, title = "(c)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3), bidirectional=TRUE)
mtext(expression(A %->% B), side = 1, line = 7, cex = 5, xpd=TRUE)
qgraph(ex4, layout = "circle", asize= 4, edge.width = 6, vsize = 10, label.cex = 1.3, title = "(d)", title.cex = 7, label.font = 2, color = alpha("darkgray", 0.3), bidirectional=TRUE)
mtext(expression(atop(A %->% B), A %->% B %->% C), side = 1, line = 7, cex = 5, xpd=TRUE)


dev.off()
