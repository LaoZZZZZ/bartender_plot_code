#bartender vs blast

### Loads the data
#truth = read.table("/Users/lukez/Documents/barcode_single_end/simulation_data/True_barcode_seq.csv", 
truth = read.table("/Users/lukez/Downloads/True_barcode_freq_38.txt",
                   header = F, sep = '\t', stringsAsFactors = F)
#blast_result = read.csv("/Users/lukez/Desktop/barcode_literature/BCcounts.csv", header = F)

blast_result = read.csv("/Users/lukez/Downloads//BCcounts_2.csv", header = F)

#blast_result = read.csv("/Users/lukez/Desktop/barcode_literature/BLAST_output.csv", header = F)

#bartender_result = read.csv("/Users/lukez/Desktop/barcode_literature/primer_test_seed_5_1_cluster.csv",
bartender_result = read.csv("/Users/lukez/Downloads/simulated_38_seed_6_d3_cluster.csv",
                            header = T)

colnames(blast_result) = c("barcode", "frequency")
colnames(truth) = c("barcode", "frequency")


valid_truth = truth[which(truth$frequency > 3),]
valid_bartender = bartender_result[which(bartender_result$time_point_1 > 3),]
valid_blast = blast_result[which(blast_result$frequency > 3),]

overlap_blast_truth = merge(valid_truth, valid_blast, by = "barcode",
                            all.x = F, all.y = F)
overlap_bartender_truth = merge(valid_truth, valid_bartender, by.x = "barcode", by.y = "Center",
                                all.x = F, all.y= F)
overlap_both = merge(overlap_blast_truth, bartender_result, by.x = "barcode", by.y = "Center")
colnames(overlap_both) = c("barcode", "truth_freq", 'blast_freq',
                           'Cluster.ID', 'Cluster.Score', 'bartender_freq')
# Scatter plot

dev.off()
# scatter plot against 
attach(overlap_both)
plot(truth_freq,blast_freq, xlim = c(0,1200), ylim = c(0,1200), col = rgb(0,0,1,alpha = 0.5),
     xlab = "Truth", ylab = "Estimated", pch = 19, cex = 1.5)
abline(0,1)
points(frequency, time_point_1, col = rgb(1,0,0, alpha = 1),
       pch = 19, cex = 0.5)
legend("bottomright", legend = c("Blast", "Bartender"), pch = 19,
       col = c(rgb(0,0,1,alpha = 0.5),col = rgb(1,0,0, alpha = 1)))
detach(overlap_both)


imageBlock <- function(counts, step, image_width) {
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
dev.off()
dev.off()
pdf("/Users/lukez/Desktop/barcode_literature/barcode_blast_single_simu.pdf",
    width = 12.5, height =7.8)
plot.new()
library(LSD)
step = 5
par(fig = c(0,0.8,0.0,0.85), new = T)
break.x = c(seq(0, 1300, by = 100))
break.y = break.x
limis = c(0,max(max(valid_truth$frequency),max(max(valid_blast$frequency),max(valid_bartender$time_point_1))))
plot(overlap_blast_truth$frequency.x,
     overlap_blast_truth$frequency.y,
     ylim = limis,xlim = limis,
     col = rgb(0,0,1,alpha = 0.5),
     xlab = "", ylab = "", 
     pch = 19, cex = 1, axes = F)

axis(3, at = break.x, col.lab = 'red', tck = -0.008)
axis(4, at = break.y, col.lab = "blue",tck = -0.008)
legend(50,1000, legend = c("Blast", "Bartender"), pch = 19,
       col = c(rgb(0,0,1,alpha = 0.5),col = rgb(1,0,0, alpha = 1)),
       cex = 1, bty = 'n')
lines(c(0,1300), c(0, 1300))
points(overlap_bartender_truth$frequency, overlap_bartender_truth$time_point_1, col = rgb(1,0,0, alpha = 1),
       pch = 19, cex = 0.5)


points(348, 582, col = 'green', pch = 17, cex = 1.5)
points(348, 349, col = 'green', pch = 17, cex = 1.5)
points(239, 235, col = 'green', pch = 17, cex = 1.5)

points(348, 582, col = rgb(0,0,1,alpha = 0.5), pch = 19, cex = 1)
points(348, 349, col = rgb(1,0,0, alpha = 1), pch = 19, cex = .5)
points(239, 235, col = rgb(1,0,0, alpha = 1), pch = 19, cex = .5)

# first picked mixed cluster
# points(348,587, col = 'green', pch = 2, cex = 1.5, lty = 1.5)
# points(348,373, col = 'green', pch = 2, cex = 1.5, lty = 1.5)
# points(239,214, col = 'green', pch = 2, cex = 1.5, lty = 1.5)
# 
# # second picked mixed cluster
# points(67,520, col = 'black', pch = 0, cex = 1.5, lty = 1.5)
# points(237,235, col = 'black', pch = 0, cex = 1.5, lty = 1.5)
# points(216,216, col = 'black', pch = 0, cex = 1.5, lty = 1.5)
# points(67,69, col = 'black', pch = 0, cex = 1.5,lty = 1.5)

# 
# selected_clusters = overlap_bartender_truth[which(overlap_bartender_truth$Cluster.ID %in% c(72189,62735)),]
# points(selected_clusters$frequency, selected_clusters$time_point_1, col = 'red', pch = 17)
#text(600,1170, labels = "True counts", cex = 1)
#text(1180, 600, labels = "Estimated counts", srt = -90, cex = 1)

blast_color_palette = colorRampPalette(c("grey90", "dark blue"))
bartender_color_palette = colorRampPalette(c("grey90", "dark red"))
bartender_color <-bartender_color_palette(1000)[c(1, 100:1000)]
blast_color <-blast_color_palette(1000)[c(1, 100:1000)]


# plot the False negative for bartender
#overlap_bartender_truth = merge(valid_truth, valid_bartender, by.x = "barcode", by.y = "Center")

par(fig=c(0.078,0.76,0.80,0.88), new=TRUE)
par(mar = c(0.5,0.5,1,0.5),mgp = c(3,1,0))
missing_bartender = valid_truth[!(valid_truth$barcode %in% overlap_bartender_truth$barcode),c("frequency")]


z.np.img = imageBlock(missing_bartender, step, 1200)
# z.np = matrix(0, nrow = max(overlap_bartender_truth$frequency), ncol = 1)
# for (f in missing_bartender) {
#   z.np[f] = z.np[f] + 1
# }
z.np.img[z.np.img >=902] = 902

image(z.np.img, 1,
      col=bartender_color, 
      xlab="",ylab="",axes = F)
# false negative for blast
#overlap_blast_truth = merge(valid_truth, valid_blast, by = "barcode")

par(fig=c(0.078,0.76,0.86,0.94), new=TRUE)
par(mar = c(0.5,0.5,1,0.5),mgp = c(3,1,0))
missing_blast = valid_truth[!(valid_truth$barcode %in% overlap_blast_truth$barcode),c("frequency")]


z.np_blast = imageBlock(missing_blast, step, 1300)
z.np_blast[z.np_blast >=902] = 902
# for (f in missing_blast) {
#   z.np_blast[f] = z.np_blast[f] + 1
# }
image(z.np_blast, 1,
      col=blast_color, 
      xlab="",ylab="",axes = F)
#mtext("False negative (missing barcodes)", line = 0.5, cex = 1, side = 3)

# Plot the false positive part.
#library(grid)

fp_bartender = valid_bartender[!(valid_bartender$Center %in% overlap_bartender_truth$barcode),c("time_point_1")]
par(fig=c(0.80,0.84,0.127,0.85),new=TRUE)
par(mar = c(1,0.3,5,0.2))

#xhist.count[c(which.min(xhist.count!=0),which.max(xhist.count!=0))]=0;
z.bartender_fp = imageBlock(fp_bartender, step, 1200)
z.bartender_fp[z.bartender_fp >=902] = 902
image(1,1:1200, t(z.bartender_fp),
      col=bartender_color, 
      xlab="",ylab="",axes = F)

par(fig=c(0.84,0.88,0.127,0.85),new=TRUE)
par(mar = c(1,0.3,5,0.2))
fp_blast = valid_blast[!(valid_blast$barcode %in% overlap_blast_truth$barcode),c("frequency")]
#xhist.count[c(which.min(xhist.count!=0),which.max(xhist.count!=0))]=0;
z.blast_fp = imageBlock(fp_blast, step, 1300)
z.blast_fp[z.blast_fp >=902] = 902

image(1,1:1300, t(z.blast_fp),
      col=blast_color, 
      xlab="",ylab="",axes = F)
#mtext("False positive (spurious barcodes)",side=4,cex=1,line = 0.5, padj = 0) #Plot your title


par(fig=c(0.77,0.98,0.83,0.85),new=TRUE)
par(mar = c(0.2,0.2,0.2,0.2))

max_freq = max(z.np_blast, z.blast_fp)
image(seq(0,max_freq,by = 200),1,as.matrix(seq(0,max_freq,by = 200)),col=blast_color
      ,xlab="",ylab="", axes = F)
axis(1, labels = seq(0,max_freq,by = 200), at = seq(0,max_freq,by = 200), tick = F,cex = 0.3)

par(fig=c(0.77,0.98,0.90,0.92),new=TRUE)
par(mar = c(0.2,0.2,0.2,0.2))
max_freq = max(z.np.img, z.bartender_fp)

image(seq(0,max_freq,by = 20),1,as.matrix(seq(1,max_freq,by = 20)),col=bartender_color
      ,xlab="",ylab="", axes = F)
axis(1, labels = seq(0,max_freq,by =20), at = seq(0,max_freq,by = 20), tick = F,cex = 0.3)
#mtext("Estimated counts for the color bar",side=3,cex=1,line = 0.5, padj = 0)
dev.off()

ks.test(overlap_both$blast_freq, overlap_both$truth_freq)
ks.test(overlap_both$bartender_freq, overlap_both$truth_freq)

# 
# image(0:4,1,as.matrix(1:4),col=mypalette,xlab="",
#       ylab="", axes = F)
# axis(1, labels = c("0","10", "30", "40"), at = c(0.5,1.5,2.5,3.5), tick = F,cex = 0.4)
# 
# 
# 
# hist(missing_blast, breaks = 50, xlim = c(1,1200), col = rgb(0,0,1,alpha = 0.3))
# hist(missing_bartender,add = T, col = rgb(1,0,0,alpha = 1))
# 
# 
# blast_false_positive = valid_blast[!(valid_blast$barcode %in% overlap_blast_truth$barcode), c("frequency")]
# bartender_false_positive = valid_bartender[!(valid_bartender$Center %in% overlap_bartender_truth$barcode), c("time_point_1")]
# hist(bartender_false_positive,col = rgb(1,0,0,alpha = 1),xlim = c(1,1200),breaks = 10)
# hist(blast_false_positive, breaks = 50, 
#      col = rgb(0,0,1,alpha = 0.1),add = T)
# 
# 
# 
# 
# 
# 
# 
# 
# # Another try to use barplot instead of image plot given that the distribution of blast fn or fp is widely spread.
# detach(overlap_both)
# 
# dev.off()
# overlap_blast_truth = merge(valid_truth, valid_blast, by = "barcode")
# overlap_bartender_truth = merge(valid_truth, valid_bartender, by.x = "barcode", by.y = "Center")
# plot.new()
# library(LSD)
# par(fig = c(0.01,0.8,0.01,0.8), new = T)
# break.x = c(0,seq(100, max(truth_freq), by = 100),1200)
# break.y = c(0, seq(100, max(max(blast_freq),max(bartender_freq)), by = 100),1200)
# 
# with(overlap_blast_truth,plot(frequency.y ~ frequency.x, xlim = c(0,1200), ylim = c(0,1200), col = rgb(0,0,1,alpha = 0.5),
#      xlab = "", ylab = "", pch = 19, cex = 1.5, axes = F))
# axis(3, at = break.x, col.lab = 'red', pos = 1200,  tck = -0.008)
# axis(4, at = break.y, col.lab = "blue",tck = -0.008, pos = 1200)
# 
# lines(c(0,1200), c(0, 1200))
# with(overlap_bartender_truth, points(frequency, time_point_1, col = rgb(1,0,0, alpha = 1),
#        pch = 19, cex = 0.5))
# text(600,1170, labels = "Truth", cex = 1.2)
# text(1180, 600, labels = "Estimated", srt = -90, cex = 1.2)
# 
# break.x = with(overlap_blast_truth, seq(min(frequency.x), max(frequency.x), by = 5))
# break.y = with(overlap_blast_truth,seq(min(frequency.x), max(frequency.x), by = 5))
# counts_blast_fn = with(valid_truth,
#                        hist(valid_truth[!(barcode %in% overlap_blast_truth$barcode),"frequency"],
#                             plot = F, breaks = break.x))$counts
# counts_bartender_fn = with(valid_truth,
#                            hist(valid_truth[!(barcode %in% overlap_bartender_truth$barcode),"frequency"],
#                                 plot = F, breaks = break.x))$counts
# par(fig=c(0.08,0.765,0.765,0.9), new=TRUE)
# par(mar = c(0.5,0.5,1,0.5),mgp = c(3,1,0))
# # barplot(counts_blast_fn, xlim = c(0, 1200), 
# #         xlab = '', ylab = '',axes = F,
# #         col = rgb(0,0,1,alpha = 0.3), main = '')
# with(valid_truth,
#      hist(valid_truth[!(barcode %in% overlap_blast_truth$barcode),"frequency"],
#           ,breaks = break.x,col = rgb(0,0,1,alpha = 0.3),
#           xlab = '', ylab = '', main = '', axes = F))
# 
# 
# 

abs_diff =  abs(overlap_both$truth_freq - overlap_both$blast_freq)
overlap_both = overlap_both[order(-abs_diff),]


library(seqLogo)

plotSeqLogo <- function(single_frequency,...) {
  pwm <- apply(single_frequency, 2, proportion)
  pwm <- makePWM(pwm) 
  seqLogo(pwm, ...)
}

proportion <- function(x){
  rs <- sum(x);
  return(x / rs);
}

findBartenderClusters <- function(unique_reads, barcode_cluster_mapping) {
  ids = unique(barcode_cluster_mapping[barcode_cluster_mapping$Unique.reads %in% unique_reads$V1,'Cluster.ID'])
  ids
}

barcode_cluster_mapping = read.csv("/Users/lukez/Downloads/simulated_38_seed_6_barcode.csv",
                                   header = T)
quality = read.csv("~/Downloads/simulated_38_seed_6_d3_quality.csv", header = T)
c1 = "GGTACCACAGCAATGTAAAACACCCTTCTTGCATAACT"
unique_reads = read.table()

bartender_selected_clusters = c(65460,71846)

c1_pwm = read.csv(paste("~/Downloads/",c1,"_pwm_matrix.csv", sep = ''), header = F)
corresponding_clusters_quality = quality[which(quality$Cluster.ID %in% bartender_selected_clusters),]

plotSeqLogo(t(c1_pwm), yaxis = F, xaxis = F, ic.scale = F)

plotSeqLogo(corresponding_clusters_quality[1:4,3:40],ic.scale = F, yaxis = F, xaxis = F)
plotSeqLogo(corresponding_clusters_quality[5:8,3:40], ic.scale = F, yaxis = F, xaxis = F)



c2 = "ACAATTCTCTGTGTACGAAC"
c2_pwm = read.csv(paste("~/Desktop/barcode_literature/",c2,"_pwm_matrix.csv", sep = ''), header = F)
unique_reads = read.csv(paste("~/Desktop/barcode_literature/",c2,"_unique_reads.csv", sep = ''), header = F)
mapped_ids = findBartenderClusters(unique_reads, barcode_cluster_mapping)
corresponding_clusters_quality = quality[which(quality$Cluster.ID %in% mapped_ids),]

plotSeqLogo(t(c2_pwm), yaxis = F, xaxis = F, ic.scale = F)
plotSeqLogo(corresponding_clusters_quality[1:4,3:22],ic.scale = F, yaxis = F, xaxis = F)
plotSeqLogo(corresponding_clusters_quality[5:8,3:22], ic.scale = F, yaxis = F, xaxis = F)
plotSeqLogo(corresponding_clusters_quality[9:12,3:22], ic.scale = F, yaxis = F, xaxis = F)


overlap_bartender_truth = with(overlap_bartender_truth, 
                               overlap_bartender_truth[Cluster.ID %in% bartender_selected_clusters,])


m = c(12,16,17,15,6,53982,53982,2,9,21,33,9,53982,53982,5,53950,12,12,6,0,0,7,9,53942,11,8,0,0,0,0,
      53942,23,2,27,13,0,0,13,2,1,3,53949,0,0,53951,2,4,1,53968,0,0,3,2,4,4,6,0,0,0,0,
      13,11,53952,8,15,0,0,1,53963,53945,53920,10,0,0,8,19,53947,53955,2,0,0,53961,53962,24,53957,53962,0,0,0,0,
      15,53932,11,53932,53948,0,0,53966,8,15,26,14,0,0,18,11,19,14,6,53982,53982,11,9,12,10,6,0,0,0,0)


pwm_m = matrix(m, nrow = 30)
plotSeqLogo(t(pwm_m[1:26,]),yaxis = F, xaxis = F, ic.scale = F)

m_bl = c(12,16,17,15,8,53986,53984,2,9,21,33,11,53986,53984,7,53952,12,12,6,0,0,7,11,53943,11,8,0,0,0,0,
         53944,23,2,27,13,0,0,13,2,1,5,53951,0,2,53953,2,4,3,53970,0,0,3,2,5,5,7,0,0,0,0,
         13,13,53954,8,15,0,0,3,53967,53949,53922,10,0,0,8,21,53951,53957,2,0,2,53965,53964,26,53960,53962,0,0,0,0,
         17,53934,13,53936,53950,0,2,53968,8,15,26,14,0,0,18,11,19,14,8,53986,53984,11,9,12,10,6,0,0,0,0)


pwm_mbl = matrix(m_bl, nrow = 30)
plotSeqLogo(t(pwm_mbl[1:26,]),yaxis = F, xaxis = F, ic.scale = F)

m_bl = c(12092,6154,6711,7565,6463,51177,51171,2021,12442,3107,3551,1315,51177,51173,1285,1991,1616,2306,1806,0,1,1455,1333,1379,426,2008,2,0,0,0,
         24469,29705,26531,21251,18747,2,0,5125,9155,391,1653,1644,0,0,2295,999,2961,4186,2523,0,0,1129,1804,1762,2397,1765,0,0,0,0,
         12345,14497,17765,21639,25715,1,9,43893,25722,46037,45671,48211,3,7,45326,48181,46590,43259,45979,3,6,48586,46518,46883,48342,47006,1,0,0,0,
         2274,824,173,725,255,0,0,141,3861,1645,305,10,0,0,2274,9,13,1429,872,51177,51173,10,1525,1156,15,394,0,0,0,0)


pwm_mbl = matrix(m_bl, nrow = 30)
plotSeqLogo(t(pwm_mbl[1:26,]),yaxis = F, xaxis = F, ic.scale = F)


m_bl = c(0,0,0,0,796,797,797,0,0,0,0,0,797,797,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
         0,397,0,0,0,0,0,398,797,0,400,0,0,0,0,0,0,0,0,0,0,0,795,0,395,0,0,0,0,0,
         0,1,797,0,1,0,0,0,0,797,0,796,0,0,797,0,797,797,797,0,0,797,0,796,402,797,0,0,0,0,
         797,399,0,797,0,0,0,399,0,0,397,1,0,0,0,797,0,0,0,797,797,0,1,1,0,0,0,0,0,0)

pwm_mbl = matrix(m_bl, nrow = 30)
plotSeqLogo(t(pwm_mbl[1:26,]),yaxis = F, xaxis = F, ic.scale = F)

m_bl = c(32,30,178412,178403,12,178492,178511,178439,19,178453,178300,24,178453,178511,178386,46,90,31,26,0,0,51,34,49,34,178420,3,0,0,0,
         49,70,20,12,21,0,0,2,178458,2,36,10,3,0,11,178378,12,3,3,0,0,8,100,19,20,9,0,0,0,0,
         33,56,67,79,56,15,0,67,2,48,128,178442,53,0,98,20,178348,178438,178464,0,1,178425,27,178412,178424,62,0,0,0,0,
         178397,178355,12,17,178422,4,0,3,32,8,47,35,2,0,16,67,61,39,18,178511,178510,27,178350,31,33,13,0,0,0,0)
pwm_mbl = matrix(m_bl, nrow = 30)
plotSeqLogo(t(pwm_mbl[1:26,]),yaxis = F, xaxis = F, ic.scale = F)

m_bl = c(32,29,178401,178393,11,178481,178500,178429,18,178442,178290,23,178442,178500,178376,46,90,31,26,0,0,51,34,48,30,178418,0,0,0,0,
         49,70,20,12,21,0,0,1,178448,2,36,10,3,0,10,178368,12,3,3,0,0,8,100,19,20,9,0,0,0,0,
         33,56,67,79,56,15,0,67,2,48,127,178432,53,0,98,19,178337,178427,178454,0,0,178415,26,178403,178418,61,0,0,0,0,
         178386,178345,12,16,178412,4,0,3,32,8,47,35,2,0,16,67,61,39,17,178500,178500,26,178340,30,32,12,0,0,0,0)
pwm_mbl = matrix(m_bl, nrow = 30)
plotSeqLogo(t(pwm_mbl[1:26,]),yaxis = F, xaxis = F, ic.scale = F)


m_bl = c(65941,66052,65475,9808037,65956,66140,2500340,2469413,2474714,2513050,2518057,9808293,9808313,2496213,2482134,2520231,2510823,2472104,9808228,9807985,2542817,2512358,2449206,2494503,2504473,65892,65863,2454082,2474811,2502020,2483432,2484186,9808434,66584,9808985,9808293,66369,66301,
         65666,66064,66429,66263,9808706,9808078,2505902,2527450,2503098,2502611,2455294,65644,66025,2517785,2528451,2496927,2498207,2498294,66439,66083,2501374,2494247,2511714,2496419,2501897,66141,66146,2520050,2516429,2518514,2531954,2531207,65809,66000,65732,65977,9808353,65967,
         9808561,9808340,66433,65978,65929,65921,2468489,2503319,2521419,2506847,2500926,66295,66035,2507938,2501585,2493362,2500777,2519654,66035,66029,2480181,2477867,2499724,2497713,2520346,66150,66291,2488318,2540451,2512921,2523417,2500770,66012,66265,66141,65983,65640,65832,
         66222,65934,9808053,66112,65799,66251,2531659,2506208,2507159,2483882,2532113,66158,66017,2484454,2494220,2495870,2496583,2516338,65688,66293,2482018,2521918,2545746,2517755,2479674,9808207,9808090,2543940,2474699,2472935,2467587,2490227,66135,9807541,65532,66137,66028,9808290)
pwm_mbl = matrix(m_bl, nrow = 38)
plotSeqLogo(t(pwm_mbl[1:38,]),yaxis = F, xaxis = F, ic.scale = F)


overlap_truth_simu = read.table("~/Downloads/simu_truth_overlap.csv", header = T)

plot(overlap_truth_simu$V2.x, overlap_truth_simu$V2.y, pch = 19,
     col = 'blue', xlim = c(0,1200), ylim = c(0, 1200),
     xlab = "Truth frequency", ylab = "Frequency after sequencing", main = '')
abline(0,1)


