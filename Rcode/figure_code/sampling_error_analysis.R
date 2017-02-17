#!/usr/bin/Rscript

num_partition = 15 
whole_file = 'True_cluster_38_low_error.csv'
#whole_file = 'result_T0_cluster.csv'
whole_result = read.csv(whole_file, header = T)[, c("Cluster.ID","Center", "time_point_1")]
colnames(whole_result) = c("Cluster.ID","Center", "whole_freq")
sub = seq(0,num_partition - 1, by = 1)
library(stringdist)
if (F) {
for (i in sub) {
	temp = paste("result_simulation_sub_sample", i, "_cluster.csv",sep = '')
	bartender_temp = read.csv(temp, header = T)[,c("Center", "time_point_1")]
	colnames(bartender_temp) = c("Center", paste("bartender_one_mis_freq",i, sep = '_'))

	temp_whole_result = merge(whole_result,bartender_temp , by = "Center", all.x = F, all.y = F)
	unmatched_truth = whole_result[!(whole_result$Cluster.ID %in% temp_whole_result$Cluster.ID),]
	unmatched_result = bartender_temp[!(bartender_temp$Center %in% temp_whole_result$Center),]
	matches = amatch(unmatched_truth$Center, unmatched_result$Center)
	valid_matches_index = which(matches != 0)
	valid_combined = cbind(unmatched_truth[valid_matches_index,], unmatched_result[matches[valid_matches_index],2])
	colnames(valid_combined) = colnames(temp_whole_result)
	print(head(valid_combined))	
	if (False) {
	matches = matches[order(temp_sub$Cluster.ID, -temp_sub$time_point_1),]
	temp_sub = temp_sub[!duplicated(temp_sub$Cluster.ID),]
	
	print(dim(temp_whole_result))
	print(dim(whole_result))	
	for (c in unmatched_result$Center) {
		index = amatch(c, unmatched_truth$Center, maxDist = 1, method = "hamming", nomatch = 0)
		if (index > 0) {
			extra_row = cbind(unmatched_truth[index,], unmatched_result[unmatched_result$Center == c,-1])
			colnames(extra_row) = colnames(temp_whole_result)
			temp_whole_result = rbind(temp_whole_result, extra_row, use.names = F)
			unmatched_truth = unmatched_truth[-index,]
		}
	}
	}
	whole_result = rbind(temp_whole_result, valid_combined)	
	whole_result = rbind(temp_whole_result, cbind(unmatched_truth[!valid_matches_index,], rep(0, nrow(unmatched_truth))), use.names = F) 
}
}
for (i in sub) {
	print(paste("sub", i, sep = '-'))
	temp = paste("simulation_sub_sample_low_error", i, "original_sampled_cluster.csv",sep = '')
	temp_sub = read.csv(temp, header = T)[, c("Cluster.ID", "time_point_1")]
	colnames(temp_sub) = c("Cluster.ID", paste("sampled_freq",i, sep = '_'))
	whole_result = merge(whole_result,temp_sub , by = "Cluster.ID", all.x = T)
}

#for (i in sub) {
#	print(paste("sub", i, sep = '-'))
#	temp = paste("result_simulation_sub_sample", i, "_cluster_corrected.csv",sep = '')
#	temp_sub = read.csv(temp, header = T)[, c("Cluster.ID", "time_point_1")]
#	temp_sub = temp_sub[order(temp_sub$Cluster.ID, -temp_sub$time_point_1),]
#	temp_sub = temp_sub[!duplicated(temp_sub$Cluster.ID),]
#	colnames(temp_sub) = c("Cluster.ID", paste("bartender_corrected_freq",i, sep = '_'))
#	whole_result = merge(whole_result,temp_sub , by = "Cluster.ID", all.x = T)
#}

for (i in sub) {
	temp = paste("result_simulation_sub_sample_low_error", i, "_cluster.csv",sep = '')
	bartender_temp = read.csv(temp, header = T)[,c("Center", "time_point_1")]
	colnames(bartender_temp) = c("Center", paste("bartender_freq",i, sep = '_'))

	whole_result = merge(whole_result,bartender_temp , by = "Center", all.x = T)
}
for (i in sub) {
	print(paste("sub", i, sep = '-'))
	temp = paste("result_simulation_sub_sample_low_error", i, "_cluster_one_mis.csv",sep = '')
	temp_sub = read.csv(temp, header = T)[, c("Cluster.ID", "time_point_1")]
	temp_sub = temp_sub[order(temp_sub$Cluster.ID, -temp_sub$time_point_1),]
	temp_sub = temp_sub[!duplicated(temp_sub$Cluster.ID),]
	colnames(temp_sub) = c("Cluster.ID", paste("bartender_onemis_freq",i, sep = '_'))
	whole_result = merge(whole_result,temp_sub , by = "Cluster.ID", all.x = T)
}
#for (i in sub) {
#	print(paste("sub", i, sep = '-'))
#	temp = paste("result_simulation_sub_sample", i, "_cluster_two_mis.csv",sep = '')
#	temp_sub = read.csv(temp, header = T)[, c("Cluster.ID", "time_point_1")]
#	temp_sub = temp_sub[order(temp_sub$Cluster.ID, -temp_sub$time_point_1),]
#	temp_sub = temp_sub[!duplicated(temp_sub$Cluster.ID),]
#	colnames(temp_sub) = c("Cluster.ID", paste("bartender_twomis_freq",i, sep = '_'))
#	whole_result = merge(whole_result,temp_sub , by = "Cluster.ID", all.x = T)
#}
whole_result[is.na(whole_result)] <- 0
write.csv(whole_result[,-1], paste("simulation_whole_matrix_38_low_error_3", num_partition, '.csv', sep =''), col.names = T, row.names = F, quote = F) 
