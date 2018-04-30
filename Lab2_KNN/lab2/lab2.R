library(kknn)

common_dir_path <- "../../Common/"

source(paste(common_dir_path,"common_functions.R",sep=""))

kknn_wrapper <- function(class_column, data) {
  return(train.kknn(class_column, data, kmax = 15,
             kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), 
             distance = 1))
}

seq_step <- 0.05
seq_from <- 0.05
seq_to <- 0.95

n_seq <- seq(seq_from, seq_to, by = seq_step)

tic_tac_data_frame <- validate_model(kknn_wrapper, read_tic_tac_toe, n_seq)
print(tic_tac_data_frame)

spam_data_frame <- validate_model(kknn_wrapper, read_spam, n_seq)
print(spam_data_frame)
