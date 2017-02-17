# plots for multiple time points
setwd("~/Documents/barcode_single_end/simulation_data")

#load estimated
timepoint = c(1,20, 50, 60, 65,70,seq(73,81), 83, 85,87,89, 91, 93,97,105, 113)
files = paste("Truth_time_point_", timepoint+1, "_seq.csv", sep = "")
before_files = paste("Truth_before_sequence_time_point_", timepoint+1, "_seq.csv", sep = "")

truth = read.table(before_files[1], sep =',', header = T)
for( f in before_files[-1]) {
  temp = read.table(f, sep = ',', header = T)
  truth = cbind(truth, temp[,2])
}

colnames(truth) = c("barcode", timepoint)


# load estimated trajectory

combined_files = "combine_result_cluster.csv"

combined_result = read.csv(combined_files, header = T)

names(combined_result) = c("Cluster.ID", "barcode", "score",timepoint)


overlap= merge(truth, combined_result, by = "barcode")
sum_tr = apply(non_overlap[,-c(1,2,3)] > 0, 2, sum)

# pick up some representative trajectory for plotting




N = 1000
set.seed(2)
index = sample(1:nrow(overlap), N, replace = F)

selected_traj_truth = as.data.frame(t(overlap[index, c(paste(timepoint, 'x', sep = '.'))]))
selected_traj_est = as.data.frame(t(overlap[index, c(paste(timepoint, 'y', sep = '.'))]))

selected_traj = cbind(selected_traj_truth, selected_traj_est)
selected_traj = cbind(timepoint, selected_traj)

colnames(selected_traj) = c("timepoint", paste(rep(c("truth", "estimated"), each = N), rep(1:N, 2), sep = '_'))


library(ggplot2)
library(reshape2)
library(plyr)
pdf("trajectory_plot_simulation.pdf", width = 8, height = 8)
groups = paste(rep(c("truth", "estimated"), each = N), rep(1:N, 2), sep = '_')

colors()
melted_traj <- melt(selected_traj, id = "timepoint") 
#my.colors <- rep(rep(sample(colors(), N, replace = T), each = 21),2)


set.seed(23)
color_pool = c("#FF2A68","#FF5E3A", "#FFCD02","#0BD318","#5AC8FB", "#1D62F0", 
               "#C644FC", "#EF4DB6", "#4A4A4A", "#DBDDDE", "#FF3830","#FF9500", 
               "#FFCC00","#4CD964", "#34AADC", "#007AFF", "5856D6","#FF2D55",
               "#5856D6","#FF2D55", "#4CD964", "#5856D6", "#5AD427", "#C86EDF",
               "#C644FC","#5BCAFF","#D1EEFC", "#5AD427")
condition = ifelse(rep(c(1,0), each = length(timepoint)*N), "Truth","Estimated")
line_type = as.factor(rep(rep(c("Truth","Estimated"), each = N),each = length(timepoint)))
line_width = rep(c(1,0), each = length(timepoint)*N)

my.colors = rep(rep(sample(color_pool, N, replace = T), 2),each = length(timepoint))
fill_value = rep(as.numeric(apply(selected_traj[, -1], 2, max)), each = length(timepoint))
p <- ggplot(data=melted_traj)
p <-p + geom_line(aes(x=timepoint-1, y=value, group = variable, fill = condition,size = line_width,linetype=line_type, colour = my.colors)) 
p <- p + ggtitle("")+ xlab("") + ylab("") 
p <- p + theme_bw() + theme(
  axis.title.x = element_text(color="black", vjust=-0.35,size = 15),
  axis.title.y = element_text(color="black" , vjust=0.35, size = 15),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.position = 'top')

p<- p + scale_x_continuous(limits = c(0,114),breaks = c(seq(0,100,by = 20),114), labels = c(seq(0,100,by = 20),114))
p <- p + guides(colour = FALSE)
p <- p + scale_size(range=c(1, 2))
p <- p + guides(size = FALSE)
p <- p + guides(group = FALSE)
p <- p + guides(condition = FALSE)

#p <- p + guides(linetype = guide_legend(title = ""))
#p <- p + scale_linetype_discrete(name="Group")
#p <- p + guides(fill = guide_legend(override.aes = list(size=20)))
p

dev.off()


# generate boxplot
estimated_files = paste("result_Sequenced_time_point_", timepoint + 1, "_seq_umi_cluster.csv", sep = '')
estimated_timepoints = list()

# only interested in the clusters whose size is above this cutoff
cutoff = 2
result = c()
for(i in 1:length(estimated_files)) {
  temp = read.csv(estimated_files[i], header = T)
  overlap_tmp = sum(temp$Center %in% truth[truth[,i + 1] > cutoff, 1]) 
  total_truth = sum(truth[, i + 1] > cutoff)
  result = rbind(result,c(total_truth, overlap_tmp))
} 

# generate the numbers

cat_result = as.data.frame(cbind(timepoint, timepoint,result )  )

pdf("Barcodes_count_traj.pdf", width = 8, height = 8)

names(cat_result) = c("timepoints", "group", "Truth", "Identified")
melted_result = melt(cat_result,id = c("timepoints", "group"))
means <- ddply(melted_result, c("group", "variable"), summarise,
               mean=mean(value))
means.barplot <- qplot(x=group, y=mean, fill=variable,
                       data=means, geom="bar", stat="identity",
                       position="dodge")
p <- ggplot(data=melted_result, aes(x=group, y = value, fill = variable, color = variable))
p <- p + geom_line(stat = "identity", position = "dodge")
p <- p + geom_point(aes(x=group, y = value, fill = variable,color = variable, shape = variable, size = 1 ))
p <- p + ggtitle("")
p <- p + xlab("") + ylab("") 
p <- p + theme_bw() + theme(
  axis.title.x = element_text(color="black", vjust=-0.35,size = 15),
  axis.title.y = element_text(color="black" , vjust=0.35, size = 15),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15),
  legend.title=element_blank(),
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  #panel.border = element_blank(),
  panel.background = element_blank(),
  legend.text=element_text(size=15),
  legend.position = 'top')
#p <- p + xlim(c(timepoint[1], timepoint[24]))
p<- p + scale_x_continuous(limits = c(0,114),
                           breaks = c(seq(0,100,by = 20),114), 
                           labels = c(seq(0,100,by = 20),114))
p <- p + ylim(c(0,100000))
p <- p + guides(linetype = guide_legend(title = ""))
p <- p + scale_size(range=c(3, 3))
p <- p + guides(size = FALSE)

p <- p + guides(colour = guide_legend(override.aes = list(linesize=1)))
p

dev.off()
# Plot the estimated error rate along the time.
timepoint = c(1,20, 50, 60, 65,70,seq(73,81), 83, 85,87,89, 91, 93,97,105, 113) - 1
error_rate = c(0.019401,0.0193418,0.0192438,0.0194944,0.0196037,0.0196595,
               0.0197155,0.0196882,0.0196974,0.0196849,0.019683,0.0196874,
               0.0197069,0.0196915,0.0196668,0.0197005,0.0197186,0.0197108,
               0.0197126,0.0197106,0.0196915,0.0196947,0.0196945,0.0197154)
plot(timepoint, error_rate, xlab = 'Time point', ylab = 'Estimated Error rate',
     main = 'Estimated errror rate', type = 'o', pch = 19,cex = 0.5,
     ylim = c(0,.02))
abline(h = 0.01999,lty = 2, col = 'gray')
