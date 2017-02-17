#!/usr/bin/Rscript
# overlap between sub samples.
#whole_result = read.csv("~/Downloads/T0_whole_matrix_10.csv", header = T)
#whole_result = read.csv("simulation_whole_matrix_low_error_415.csv", header = T)
#whole_result = read.csv("simulation_whole_matrix_4_15.csv", header = T)
whole_result = read.csv("T0_whole_matrix_15.csv", header = T)

batch = 15


expected_size = whole_result$whole_freq * (1/batch)
sd_theoretical = sqrt(whole_result$whole_freq * (1/batch) * (1 - 1/batch))
sd_cv_theoretical = sd_theoretical / expected_size

# SSM_sampled = apply((whole_result[,3:(batch + 2)] - expected_size)^2,1,sum)/batch
# SSM_bartender = apply((whole_result[,(batch + 3):(2*batch + 2)] - expected_size)^2,1,sum)/batch
# MSE_sampled = sqrt(SSM_sampled)
# MSE_bartender = sqrt(SSM_bartender)
# MSE_CV_sampled = MSE_sampled / expected_size
# MSE_CV_bartender = MSE_bartender / expected_size


var_sampled = apply(whole_result[,3:(batch + 2)],1,var)
#var_bartender_corrected = apply(whole_result[,(batch + 3):(2*batch + 2)],1,var)
var_bartender = apply(whole_result[,(batch + 3):(2*batch + 2)],1,var)
#var_bartender_one_mis = apply(whole_result[,(2*batch + 3):(3*batch + 2)],1,var)
#var_bartender_two_mis = apply(whole_result[,(4*batch + 3):(5*batch + 2)],1,var)

sd_sampled = sqrt(var_sampled)
#sd_bartender_corrected = sqrt(var_bartender_corrected)
sd_bartender = sqrt(var_bartender)
#sd_bartender_one_mis =sqrt(var_bartender_one_mis)
#sd_bartender_two_mis =sqrt(var_bartender_two_mis)

mean_sampled = apply(whole_result[, 3:(batch + 2)], 1, mean)
#mean_bartender_corrected = apply(whole_result[, (batch + 3):(2*batch + 2)], 1, mean)
mean_bartender = apply(whole_result[,(batch + 3):(2*batch + 2)],1,mean)
#mean_bartender_one_mis = apply(whole_result[, (2*batch + 3):(3*batch + 2)], 1, mean)
#mean_bartender_two_mis = apply(whole_result[, (4*batch + 3):(5*batch + 2)], 1, mean)


sd_cv_sampled = sd_sampled / mean_sampled
#sd_cv_bartender_corrected = sd_bartender_corrected / mean_bartender_corrected
sd_cv_bartender = sd_bartender / mean_bartender
#sd_cv_bartender_one_mis = sd_bartender_one_mis/ mean_bartender_one_mis
#sd_cv_bartender_two_mis = sd_bartender_two_mis/ mean_bartender_two_mis

whole_result = cbind(whole_result[,1:(2*batch + 2)],expected_size,
                     sd_theoretical,sd_cv_theoretical,
                     sd_cv_sampled,# sd_cv_bartender_corrected,
                     sd_cv_bartender)#,
	#	sd_cv_bartender_one_mis)
#		     sd_cv_bartender_two_mis)

lower = 0
upper = 40
middle_range_clusters = whole_result[which(whole_result$expected_size >= lower & whole_result$expected_size <= upper),]

middle_range_clusters = na.omit(middle_range_clusters)

library(data.table)

step = 0.5 
dt_table = data.table(middle_range_clusters)

dt_table$bucket = (dt_table$expected_size - lower) %/% step


dt_table = dt_table[order(dt_table$bucket),]
mean_cv_sd_sample_theory =  dt_table[,mean(sd_cv_theoretical), by = bucket]
#mean_sd_sample = dt_table[,mean(sd_sampled), by = bucket]
#mean_sd_bartender = dt_table[,mean(sd_bartender), by = bucket]

#mean_cv_mse_sample = dt_table[,mean(MSE_CV_sampled), by = bucket]
#mean_cv_mse_bartender = dt_table[,mean(MSE_CV_bartender), by = bucket]
#sd_cv_mse_sample = dt_table[,sd(MSE_CV_sampled), by = bucket]
#sd_cv_mse_bartender = dt_table[, sd(MSE_CV_bartender),by = bucket]

# dev.off()
x_value = dt_table[,mean(expected_size), by = bucket]
# y_lim = c(0, max(mean_cv_sd_sample_theory$V1,mean_cv_mse_sample$V1,mean_cv_mse_bartender$V1))
# plot(x_value$V1, mean_cv_mse_bartender$V1, type = 'l', col = 'red',
#      ylim =y_lim, 
#      main = 'CV for MSE', 
#      xlab = 'Expected barcode size', 
#      ylab = 'Coefficient Of Variation')
# #bartender_ci = CI(mean_cv_mse_bartender,sd_cv_mse_bartender, counts)
# #lines(x_value$V1, bartender_ci[,1], lty = 2, col = 'red')
# #lines(x_value$V1, bartender_ci[,2], lty = 2, col = 'red')
# lines(x_value$V1, mean_cv_mse_sample$V1, 
#         type = 'l', col = 'blue')
# sample_ci = CI(mean_cv_mse_sample,sd_cv_mse_sample, counts)
# #lines(x_value$V1, sample_ci[,1], lty = 2, col = 'blue')
# #lines(x_value$V1, sample_ci[,2], lty = 2, col = 'blue')
# lines(x_value$V1, mean_cv_sd_sample_theory$V1, 
#        type = 'l', col = 'black')
# legend("topright", legend = c("bartender", "sampled", "Theoretical",
#                               "95% CI of bartender",
#                               "95% CI of sampling"),
#        col = c('red', 'blue', 'black', 'red','blue'), lty = c(1,1,1,2,2), bty = 'n',
#        cex = 0.8)


# check the center matching
# include the 1-mismatch when pooling the result.
mean_cv_sd_sample = dt_table[,mean(sd_cv_sampled), by = bucket]
mean_cv_sd_bartender = dt_table[,mean(sd_cv_bartender), by = bucket]
#mean_cv_sd_bartender_corrected = dt_table[,mean(sd_cv_bartender_corrected), by = bucket]
#mean_cv_sd_bartender_one_mis = dt_table[,mean(sd_cv_bartender_one_mis), by = bucket]
#mean_cv_sd_bartender_two_mis = dt_table[,mean(sd_cv_bartender_two_mis), by = bucket]

#sd_cv_sd_sample = dt_table[,sd(sd_cv_sampled), by = bucket]
#sd_cv_sd_bartender = dt_table[, sd(sd_cv_bartender),by = bucket]
pdf("T0_cv_plot_main_15.pdf")
line_width = 3
y_lim = c(0, 3.5)
#max(mean_cv_sd_sample_theory$V1,
 #                mean_cv_sd_sample$V1,
  #               mean_cv_sd_bartender$V1#,
                 #mean_cv_sd_bartender_corrected$V1)#,
                 #mean_cv_sd_bartender_one_mis$V1
                 #mean_cv_sd_bartender_two_mis$V1)
#		 ))
plot(x_value$V1, mean_cv_sd_bartender$V1,
     type = 'l', col = 'red',lwd = line_width,
     xlim = c(lower,upper),
     ylim =y_lim, 
     main = '', 
     xlab = '', 
     ylab = '')
#bartender_ci = CI(mean_cv_sd_bartender,sd_cv_sd_bartender, counts)
#lines(x_value$V1, bartender_ci[,1], lty = 2, col = 'red')
#lines(x_value$V1, bartender_ci[,2], lty = 2, col = 'red')
lines(x_value$V1, mean_cv_sd_sample$V1, 
       type = 'l', col = 'blue', lwd = line_width)
#sample_ci = CI(mean_cv_sd_sample,sd_cv_sd_sample,counts)
#lines(x_value$V1, sample_ci[,1], lty = 2, col = 'blue')
#lines(x_value$V1, sample_ci[,2], lty = 2, col = 'blue')
lines(x_value$V1, mean_cv_sd_sample_theory$V1, 
       type = 'l', col = 'black', lwd = line_width)
#lines(x_value$V1, mean_cv_sd_bartender_corrected$V1,
 #     type = 'l', col = 'green')
#lines(x_value$V1, mean_cv_sd_bartender_one_mis$V1,
#      type = 'l', col = 'pink', lwd = line_width)
#lines(x_value$V1, mean_cv_sd_bartender_two_mis$V1,
 #     type = 'l', col = 'purple')
#legend("topright", legend = c("Bartender", "Sampled only", "Theoretical"),
  #                            "Bartender without sequence error",
   #                           "Bartender with one mismatch"),
   #                           "Bartender with two mismatch"),
#       col = c('red', 'blue', 'black'),# 'pink'),# 'purple')
#	 lty = c(1,1,1,1,1,1), bty = 'n',
#	 lwd = line_width,
#       cex = 2)

dev.off()

# y_lim = c(0, max(mean_mse_sample$V1,mean_mse_bartender$V1))
# plot(x_value$V1, mean_mse_sample$V1, type = 'l', col = 'red',
#      ylim =y_lim, main = 'MSE WPT theoretical mean', xlab = 'freq', ylab = 'MSE')
# points(x_value$V1, mean_mse_bartender$V1, 
#        type = 'l', col = 'blue')
# legend("topleft", legend = c("MSE_sampled", "MSE_bartender"),
#        col = c('red', 'blue'), lty = c(1,1))
# 
# 
# y_lim = c(0, max(mean_sd_sample_theory$V1,mean_sd_bartender_experimental$V1,mean_sd_sample_experimental$V1))
# plot(mean_sd_bartender_experimental$bucket * step + lower, mean_sd_bartender_experimental$V1, 
#      col = 'red', ylim =y_lim, main = 'Standard deviation', xlab = 'freq', ylab = 'STD', type = 'l')
# points(mean_sd_sample_theory$bucket * step + lower, mean_sd_sample_theory$V1, 
#        col = 'blue', type = 'l')
# points(mean_sd_sample_experimental$bucket * step + lower, mean_sd_sample_experimental$V1, 
#        col = 'black',type = 'l')
# legend("topleft", legend = c("bartender std", "sampled std", "Theoretical std"),
#        col = c('red', 'black', 'blue'), lty = c(1,1,1))
# 
# mean_var_sample_theory =  dt_table[,mean(var_sampled_theory), by = bucket]
# mean_var_sample = dt_table[,mean(var_sampled), by = bucket]
# mean_var_bartender = dt_table[,mean(var_bartender), by = bucket]
# y_lim = c(0, max(mean_var_sample_theory$V1,mean_var_bartender$V1,mean_var_sample$V1))
# plot(mean_var_bartender$bucket * step + lower, mean_var_bartender$V1, pch = 17, col = 'red',
#      ylim =y_lim, main = 'Variance', xlab = 'freq', ylab = 'var')
# points(mean_var_sample_theory$bucket * step + lower, mean_var_sample_theory$V1, 
#        pch = 19, col = 'blue')
# points(mean_var_sample$bucket * step + lower, mean_var_sample$V1, 
#        pch = 15, col = 'black')
# legend("topright", legend = c("bartender var", "sampled var", "theoretical var"),
#        col = c('red', 'black', 'blue'), pch = c(17, 15, 19))
# proportion = 1 -mean_var_sample_theory$V1/mean_var_bartender$V1
# plot(mean_sd_sample$bucket * step + lower,proportion, type = 'b')
# 
# 
# # explore the reason for the large deviation at the inverval 
# 
# std_buckets = cbind(mean_sd_sample,mean_sd_bartender)
# 
# large_variance_group = dt_table[dt_table$bucket == 41,]
# 
# ordered_by_variance = whole_result[order(whole_result$sd_sampled_experimental),]
# write.table(ordered_by_variance, "~/Desktop/barcode_literature/whole_matrix_15_ordered.csv", sep = ",",
#             quote = F, col.names = T, row.names = F)
# 
# large_variance_group = dt_table[dt_table$bucket %in% c(196,214,213,223),]
