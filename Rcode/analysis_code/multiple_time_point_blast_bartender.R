# compare the bartender and all-by-all blast result using two adjacent time points


blast_74 = read.csv("~/Downloads/multiple_time/74_BCcounts.csv", header = F)
colnames(blast_74) = c("barcode", "frequency")


blast_75 = read.csv("~/Downloads/multiple_time/75_BCcounts.csv", header = F)
colnames(blast_75) = c("barcode", "frequency")


combined_files = "~/Documents/barcode_single_end/simulation_data/combine_result_cluster.csv"

combined_result = read.csv(combined_files, header = T)

bartender_74 = combined_result[,c(1,2,3,10)]
bartender_75 = combined_result[,c(1,2,3,11)]


colnames(bartender_74) = c("ID", "barcode", "Score", "frequency")
colnames(bartender_75) = c("ID", "barcode", "Score", "frequency")


bartender_74 = bartender_74[bartender_74$frequency > 0,]
bartender_75 = bartender_75[bartender_75$frequency > 0,]

truth_74 = read.csv("~/Documents/barcode_single_end/simulation_data/Truth_before_sequence_time_point_74_seq.csv",
                    header = F)

truth_75 = read.csv("~/Documents/barcode_single_end/simulation_data/Truth_before_sequence_time_point_75_seq.csv",
                    header = F)
colnames(truth_74) = c("barcode", "frequency")
colnames(truth_75) = c("barcode", "frequency")
truth_74 = truth_74[truth_74$frequency > 0,]
truth_75 = truth_75[truth_75$frequency > 0,]




imageBlock <- function(counts, step, image_width) {
  if (length(counts) == 0) {
    return(matrix(1, nrow = max(image_width), ncol = 1))
  }
  im = matrix(0, nrow = max(image_width), ncol = 1)
  for (c in counts) {
    lower = (c %/% step) * step
    
    if (lower < c) {
      upper = (c %/% step + 1) * step
    } else {
      upper = lower
      lower = upper - step
    }
    lower = lower + 1
    im[lower:upper] = im[lower:upper] + 1
  }
  im
}

max_size = max(truth_74$frequency,blast_74$frequency, bartender_74$frequency)


overlap_blast_truth = merge(truth_74, blast_74, by = "barcode", all.x = F, all.y = F)
overlap_bartender_truth = merge(truth_74, bartender_74, by = "barcode", all.x = F, all.y = F)
limis = c(0, max_size)
dev.off()
plot.new()
library(LSD)
par(fig = c(0,0.8,0.0,0.85), new = T)
break.x = c(seq(0, max_size, by = max_size %/% 15))
break.y = break.x
plot(overlap_blast_truth$frequency.x,overlap_blast_truth$frequency.y,ylim = limis,xlim = limis,col = rgb(0,0,1,alpha = 0.5),
     xlab = "", ylab = "", pch = 19, cex = 1, axes = F)



points(overlap_bartender_truth$frequency.x, overlap_bartender_truth$frequency.y, col = rgb(1,0,0, alpha = 1),
       pch = 19, cex = 0.5)
lines(c(0,max_size), c(0, max_size))

axis(3, at = break.x, col.lab = 'red', tck = -0.008)
axis(4, at = break.y, col.lab = "blue",tck = -0.008)
legend(40000,20000, legend = c("All-by-all Blast", "Bartender"), pch = 19,
       col = c(rgb(0,0,1,alpha = 0.5),col = rgb(1,0,0, alpha = 1)),
       cex = 1, bty = 'n')
# 
# selected_clusters = overlap_bartender_truth[which(overlap_bartender_truth$Cluster.ID %in% c(72189,62735)),]
# points(selected_clusters$frequency, selected_clusters$time_point_1, col = 'red', pch = 17)
text(30000,58500, labels = "True counts", cex = 1.5)
text(58000, 28000, labels = "Estimated counts", srt = -90, cex = 1.5)

blast_color_palette = colorRampPalette(c("grey90", "dark blue"))
bartender_color_palette = colorRampPalette(c("grey90", "dark red"))
bartender_color <-bartender_color_palette(4000)[c(1, 100:4000)]
blast_color <-blast_color_palette(4000)[c(1, 100:4000)]


# plot the False negative for bartender

par(fig=c(0.072,0.76,0.80,0.88), new=TRUE)
par(mar = c(0.5,0.5,1,0.5),mgp = c(3,1,0))
missing_bartender = truth_74[!(truth_74$barcode %in% overlap_bartender_truth$barcode),c("frequency")]

z.np.img = imageBlock(missing_bartender, 1000, max_size)
# z.np = matrix(0, nrow = max(overlap_bartender_truth$frequency), ncol = 1)
# for (f in missing_bartender) {
#   z.np[f] = z.np[f] + 1
# }
image(z.np.img, 1,
      col=bartender_color, 
      xlab="",ylab="",axes = F)
# false negative for blast

par(fig=c(0.072,0.76,0.86,0.94), new=TRUE)
par(mar = c(0.5,0.5,1,0.5),mgp = c(3,1,0))
missing_blast = truth_74[!(truth_74$barcode %in% overlap_blast_truth$barcode),c("frequency")]


z.np_blast = imageBlock(missing_blast, 1000, max_size)
# for (f in missing_blast) {
#   z.np_blast[f] = z.np_blast[f] + 1
# }
image(z.np_blast, 1,
      col=blast_color, 
      xlab="",ylab="",axes = F)
mtext("False negative(missing barcodes)", line = 0.5, cex = 1, side = 3)

# Plot the false positive part.
#library(grid)

fp_bartender = bartender_74[!(bartender_74$barcode %in% overlap_bartender_truth$barcode),c("frequency")]
par(fig=c(0.80,0.84,0.11,0.85),new=TRUE)
par(mar = c(1,0.3,5,0.2))

#xhist.count[c(which.min(xhist.count!=0),which.max(xhist.count!=0))]=0;
z.bartender_fp = imageBlock(fp_bartender, 1000, max_size)

image(1,1:max_size, t(z.bartender_fp),
      col=bartender_color[1], 
      xlab="",ylab="",axes = F)

par(fig=c(0.84,0.88,0.11,0.85),new=TRUE)
par(mar = c(1,0.3,5,0.2))
fp_blast = blast_74[!(blast_74$barcode %in% overlap_blast_truth$barcode),c("frequency")]
#xhist.count[c(which.min(xhist.count!=0),which.max(xhist.count!=0))]=0;
z.blast_fp = imageBlock(fp_blast, 1000, max_size)


image(1,1:max_size, t(z.blast_fp),
      col=blast_color, 
      xlab="",ylab="",axes = F)
mtext("False positive(spurious barcodes)",side=4,cex=1,line = 0.5, padj = 0) #Plot your title


par(fig=c(0.77,0.98,0.83,0.85),new=TRUE)
par(mar = c(0.2,0.2,0.2,0.2))
max_freq = max(z.np_blast, z.blast_fp, z.bartender_fp,z.np.img)
image(seq(0,max_freq,by = 100),1,as.matrix(seq(0,max_freq,by = 100)),col=blast_color
      ,xlab="",ylab="", axes = F)
axis(1, labels = seq(0,max_freq,by = 100), at = seq(0,max_freq,by = 100), tick = F,cex = 0.3)

par(fig=c(0.77,0.98,0.90,0.92),new=TRUE)
par(mar = c(0.2,0.2,0.2,0.2))
image(seq(0,3300,by = 100),1,as.matrix(seq(1,3300,by = 100)),col=bartender_color
      ,xlab="",ylab="", axes = F)
axis(1, labels = seq(0,3300,by = 100), at = seq(0,3300,by = 100), tick = F,cex = 0.3)
mtext("Estimated counts for the color bar",side=3,cex=1,line = 0.5, padj = 0)

