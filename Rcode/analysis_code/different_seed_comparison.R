
setwd("~/Documents/barcode_single_end/Rcode")

#source("/Users/lukez/Documents/barcode_single_end/Rcode/distance_bitwise_util.R")
truth = read.table("../simulation_data/True_barcode_seq.csv", 
                   header = F, sep = ',', stringsAsFactors = F)

prefix = "primer_test_seed"
suffix = "cluster.csv"
seed_length = c(seq(3,7,by = 1), seq(3,7,by = 1))
step = c(rep(1,7 - 3 + 1),seq(3,7,by = 1))
files = paste(prefix, seed_length, step,suffix,sep = '_')
threshold = 3

valid_truth = truth[which(truth$V2 >= 3),]
stat <- function(valid_truth, f, threshold = 3) {
  cluster <- read.table(f,header = TRUE, sep = ',', stringsAsFactors = F)
  valid_estimate = cluster[which(cluster$time_point_1 >= threshold),] 
  merged = merge(valid_truth, valid_estimate, by.x = "V1", by.y = "Center")
  sum_sq = sum((merged[, "V2"] - merged[,'time_point_1'])^2)
  missing = length(valid_truth[!(valid_truth$V1 %in% merged$V1),c("V2")])
  
  false_positive = length(valid_estimate[!(valid_estimate$Center %in% merged$V1), c("time_point_1")])
  
  res =c(nrows(valid_truth), nrows(valid_estimate), sum_sq, missing, false_positive)
  return(res)
}

result = sapply(files, stat, valid_truth = valid_truth)


