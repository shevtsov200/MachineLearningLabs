
read_tic_tac_toe <- function() {
  raw_data <- read.table(paste(common_dir_path,"Tic_tac_toe.txt",sep=""), sep = ",", stringsAsFactors = TRUE)
  colnames(raw_data)[colnames(raw_data)=="V10"] <- "type"
  return(raw_data)
}

read_spam <- function() {
  data(spam)
  return(spam)
}

validate_model <- function(classifier, read_data, n_seq) {
  A_raw <- read_data()
  rounded_digits <- 2
  set_size <- dim(A_raw)[1]
  
  set.seed(12345)
  A_rand <- A_raw[ order(runif(set_size)), ]
  
  seq_length <- length(n_seq)
  
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
