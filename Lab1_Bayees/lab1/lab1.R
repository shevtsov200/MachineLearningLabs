library(e1071)
library(kernlab)
library(data.table)

data(spam)

read_tic_tac_toe <- function() {
  raw_data <- read.table("Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
  colnames(raw_data)[colnames(raw_data)=="V10"] <- "type"
  return(raw_data)
}

read_spam <- function() {
  return(spam)
}

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

validate_model <- function(classifier, read_data, n_seq) {
  A_raw <- read_data()
  rounded_digits <- 2
  set_size <- dim(A_raw)[1]
  
  set.seed(12345)
  A_rand <- A_raw[ order(runif(set_size)), ]
  
  seq_length <- length(n_seq)#(seq_to - seq_from + seq_step)/seq_step
  
  percentage_matrix <- sapply(n_seq, function(x) {
    train_set_size <- round(set_size*x)
    
    index <- sample(1:set_size,train_set_size)
    A_train <- A_rand[index, ]
    A_test <- A_rand[-index, ]
    prop.table(table(A_train$type))
    prop.table(table(A_test$type))
    
    A_classifier <- classifier(type ~ ., data = A_train)
    
    A_predicted <- predict(A_classifier, A_test)
    
    t <- table(A_predicted, A_test$type)
    
    rownames(t) <- c("predicted_negative","predicted_positive")
    colnames(t) <- c("real_negative","real_positive")
    
    train_set_percentage <- round(train_set_size/(set_size/100))
    
    test_set_size <- set_size - train_set_size
    test_set_percent <- (test_set_size/100)
    
    predicted_correctly <- round((t["predicted_positive","real_positive"]+t["predicted_negative","real_negative"])/test_set_percent, digits=rounded_digits)
    predicted_wrong <- round((t["predicted_negative","real_positive"]+t["predicted_positive","real_negative"])/test_set_percent, digits=rounded_digits)
    
    return (c(train_set_percentage,predicted_correctly,predicted_wrong))
  })
  percentage_data_frame <- as.data.frame(t(percentage_matrix))
  names(percentage_data_frame) <- c("train_set_percentage","predicted_correctly","predicted_wrong")
  return(percentage_data_frame)
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
