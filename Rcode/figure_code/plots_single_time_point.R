# single time plot
library(ggplot2)
library(reshape2)
library(plyr)

# plot the Venndiagram
setwd("~/Documents/barcode_single_end/Rcode")
library(VennDiagram)
#source("/Users/lukez/Documents/barcode_single_end/Rcode/distance_bitwise_util.R")
truth = read.table("../simulation_data/True_barcode_seq.csv", 
                   header = F, sep = ',', stringsAsFactors = F)
# 
# cluster <- read.table("../simulation_data/simulated_data_cluster_cluster.csv",
#                       header = TRUE, sep = ',', stringsAsFactors = F)
cluster <- read.table("../simulation_data/primer_test_seed_5_cluster.csv",
                      header = TRUE, sep = ',', stringsAsFactors = F)
# filter out low frequency barcode

valid_truth = truth[which(truth$V2 > 2),]
valid_estimate = cluster[which(cluster$time_point_1 > 2),]

#plot distribution
dev.off()
# Histogram Grey Color
hist(valid_truth$V2, col=rgb(1,0,0,0.5),
     xlim=c(3, 1200), ylim=c(0,40000), 
     main="Overlapping Histogram")
hist(valid_estimate$time_point_1,
     col=rgb(0,1,1,0.2),
     add=T)
dev.off()
box()
plot(density(valid_truth$V2), col = rgb(1,0,0, alpha = 0.3),
     lwd = 5, lty = 1,main = '',
     xlab = "Barcode frequency")
lines(density(valid_estimate$time_point_1), col = rgb(0,0,1,alpha = 1), lty = 2, lwd = 2)
legend("topright", legend = c("Truth", "Estimated"),
       col =c(rgb(1,0,0, alpha = 0.3), rgb(0,0,1,alpha = 1)),
       lty = c(1,2),
       lwd = c(5,2))
merged = merge(valid_truth, valid_estimate, by.x = "V1", by.y = "Center")

missing = valid_truth[!(valid_truth$V1 %in% merged$V1),c("V2")]

false_positive = valid_estimate[!(valid_estimate$Center %in% merged$V1), c("time_point_1")]


#Scatter plot and the barplot together
#Probably only works for rstudio cause the margin is hardcoded
dev.off()

# plot for the single time point
# Add boxplots to a scatterplot
plot.new()
box()

library(LSD)

par(fig = c(0.01,0.8,0.01,0.8), new = T)
break.x = c(0,seq(100, max(merged$V2), by = 100),1200)
break.y = c(0, seq(100, max(merged$time_point_1), by = 100),1200)
plot(merged$V2, merged$time_point_1, 
     main = "", pch = 19, ylab = '', xlab = '',
     col = 'blue',cex = 0.5, axes = F)
axis(3, at = break.x, col.lab = 'red', pos = 1200,  tck = -0.008)
axis(4, at = break.y, col.lab = "blue",tck = -0.008, pos = 1200)
lines(c(0,1200), c(0, 1200))
text(800, 500, 
     labels = bquote(R^2 == .(cor(merged$V2, merged$time_point_1) ^ 2)), 
     col = "blue", cex = 1.5)
text(600,1170, labels = "Truth", cex = 1.5)
text(1180, 600, labels = "Estimated", srt = -90, cex = 1.5)

par(fig=c(0.08,0.765,0.75,0.85), new=TRUE)
par(mar = c(0.5,0.5,1,0.5),mgp = c(3,1,0))

palette<- colorRampPalette(c("grey90", "dark green"))
mypalette<-palette(100)



z.np = matrix(0, nrow = max(merged$V2), ncol = 1)
for (f in missing) {
  z.np[f] = z.np[f] + 1
}
image(z.np, 1,
      col=mypalette, 
      xlab="",ylab="",axes = F)

mtext("False negative barcodes", line = 0.5, cex = 1.5, side = 3)
#library(grid)
par(fig=c(0.81,0.85,0.12,0.81),new=TRUE)
par(mar = c(1,0.3,5,0.2))

#xhist.count[c(which.min(xhist.count!=0),which.max(xhist.count!=0))]=0;
z.fp = matrix(0, nrow = max(merged$time_point_1), ncol = 1)
for (f in false_positive) {
  z.fp[f] = z.fp[f] + 1
}

image(1,1:max(merged$time_point_1), t(z.fp),
      col=mypalette, 
      xlab="",ylab="",axes = F)
mtext("False positive barcodes",side=4,cex=1.5,line = 0.5, padj = 0) #Plot your title

par(fig=c(0.8,0.91,0.80,0.83),new=TRUE)
par(mar = c(0.2,0.2,0.2,0.2))
#palette<- colorRampPalette(c("grey90", "dark green"))
#mypalette<-palette(100)
#mypalette = c("grey90", mypalette)
image(0:4,1,as.matrix(1:4),col=mypalette,xlab="",
      ylab="", axes = F)
axis(1, labels = c("0","10", "30", "40"), at = c(0.5,1.5,2.5,3.5), tick = F,cex = 0.9)

