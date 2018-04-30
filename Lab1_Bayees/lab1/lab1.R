library(e1071)
library(kernlab)
library(data.table)

common_dir_path <- "../../Common/"

source(paste(common_dir_path,"common_functions.R",sep=""))

generate_points <- function() {
  points_quantity <- 50
  
  x1_points <- c(rnorm(n=points_quantity, mean=10, sd=4),
                 rnorm(n=points_quantity, mean=18, sd=3))
  
  x2_points <- c(rnorm(n=points_quantity, mean=20, sd=4),
                 rnorm(n=points_quantity, mean=14, sd=3))
  
  class_column <- factor(c(rep(-1,points_quantity),rep(1,points_quantity)))
  point_frame = data.frame(x1_points, x2_points,class_column)
  
  colnames(point_frame) <- c("X1","X2", "type")
  
  circle_pch <- 21
  
  pairs(point_frame[1:2], main="scatter plot",
        pch=circle_pch, bg=point_frame$type)
  return (point_frame)
}

seq_step <- 0.05
seq_from <- 0.05
seq_to <- 0.95

n_seq <- seq(seq_from, seq_to, by = seq_step)

tic_tac_data_frame <- validate_model(naiveBayes, read_tic_tac_toe, n_seq)
spam_data_frame <- validate_model(naiveBayes, read_spam, n_seq)
points_data_frame <- validate_model(naiveBayes, generate_points, seq(seq_to,seq_to))

print(tic_tac_data_frame)
print(spam_data_frame)
print(points_data_frame)
