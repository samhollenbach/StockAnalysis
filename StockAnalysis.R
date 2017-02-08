#Set to TRUE to load in data_file.
#Data is stored in memory so only load again data is corrupted, changed or want to use a new dataset
rload_data <- FALSE
data_file <- "IVE_bidask1min.txt"
if(load_data){
  print("")
  point_minutes <- 5
  dat <- read.csv(data_file,header = FALSE)
  data <- dat[,10]
  data <- unname(tapply(data, (seq_along(data)-1) %/% point_minutes, mean))
}

#Length of master pattern to compare patterns to
L <- 20
#How many points past the master pattern are we predicting
P <- 15
#Numbers of patterns in the data that we want to compare with the master pattern
match_number <- 5 
#Length of the dataset
N <- length(data)
#How many indeces back the master pattern is in the dataset (set to 0 when trying to compare ) 
B <- as.integer(N*runif(1))
#Start index of master set
I <- N - (B + L + P)
#Create the master pattern matrix
master <- data[I:(I+L)]
#Stores the top MATCH_NUMBER pattern matches
top_matches <- matrix(0,nrow = match_number, ncol = 3+L+P)

rough_fit <- FALSE

search_end <- 25000 #default I-L
if(search_end > I){
  search_end <- I - L - P
}

cat(paste0("Your master data pattern starts at index ", I, " and is ", L, " points long\n"))
cat(paste0("Predicting next ", P, " points by analyzing ", search_end, " points of data and matching the best ", match_number, " patterns to the master\n" ))


#Progress bar to display how far into calculations we are
pb <- txtProgressBar(min = 1, max = search_end, initial = 1, char = "=", style = 3)
TIME_START <- Sys.time()

#Main pattern match loop (default index end: I-L)
for (i in 1:search_end) {
  setTxtProgressBar(pb, i)
  
  point_match <- 1
  pattern_r_sqr <- 0
  #Create pattern at index i
  pattern <- data[i:(i+L)]
  
  #Once a pattern is created we run through the pattern 
  #and determine which point match to the master gives the best fit
  for (j in 1:L) {
    r_sqr <- 0
    offset <- (data[i+j-1] - master[j])
    pattern <- master
    pattern <- pattern + offset
    
    #For each point match calculate r_sqr val and get lowest
    if(rough_fit){
      rf <- 1
    }else{
      rf <- L
    }
    for (k in 1:rf) {
      r_sqr <- r_sqr + (pattern[k] - data[i+k-1])^2
    }
    if(r_sqr < pattern_r_sqr || pattern_r_sqr == 0){
      pattern_r_sqr <- r_sqr
    }
    
  }
  
  #If the pattern has an r_sqr lower than the highest r_sqr in the 
  #top_matches list or the list isnt full, add it to the list
  if (pattern_r_sqr < top_matches[1,1] || top_matches[1,2] == 0){
    for(match_slot in 1:match_number){
      mn <- 1
      if(top_matches[match_slot,2] == 0){
        mn <- match_slot
        break
      }
    }
    top_matches[mn,1] <- pattern_r_sqr
    top_matches[mn,2] <- i
    top_matches[mn,3] <- point_match
    top_matches[mn,4:(L+P+3)] <- data[i:(i+L+P-1)]
    top_matches <- top_matches[order(top_matches[,1],decreasing = TRUE),]
  }
  
}

#Divide R_Sqr values by L to get average R_sqr per point
top_matches[,1] <- sqrt(top_matches[,1]/L)

#ADD THRESHOLD LOGIC HERE
TIME_DIF <- Sys.time() - TIME_START
cat("\n")
print(TIME_DIF)
#R_SQR is the average error , squared, when comparing the master to the specific pattern
cat(paste0("Found the top ", match_number, 
           " matching patterns from your dataset for the ", L, " points at index ", (I)))
cat(paste0("\nBest R-squared value: ", top_matches[match_number,1]," at index ", top_matches[match_number,2]))
cat(paste0("\nWorst R-squared value: ", top_matches[1,1]," at index ", top_matches[1,2]))

#Order top_matches increasing by r_sqr value and then eliminate index, r_sqr and point_match vars
top_matches <- top_matches[order(top_matches[,1],decreasing = FALSE),]
top_matches <- top_matches[,4:(L+P+3)]

#Sum all of the matches and take average to create our final pattern
match_sum <- colSums(top_matches)
match_sum <- match_sum/match_number

#Adjusts the pattern to fit with the master data 
pattern <- match_sum[1:L]
sum_low_point <- 1
r_sum_low <- -1
for (p in 1:L) {
  r_sum <- 0
  off <- (pattern[p] - master[p])
  t <- master + off
  for(l in 1:L){
    r_sum <- r_sum + (t[l] - master[l])^2
  }
  if(r_sum_low == -1){
    r_sum_low <- r_sum
  }else if(r_sum < r_sum_low){
    r_sum_low <- r_sum
    sum_low_point <- p
  }
}

#Align final pattern with master set
offset <- (master[sum_low_point] - pattern[sum_low_point])
pattern <- pattern + offset
#Create prediction pattern and actual data of prediction (if available)
predict <- match_sum[(L+1):(L+P)] + offset
actual <- data[(I+L):(I+L+P-1)]


#Plot y range
ymin <- min(min(master),min(pattern),min(predict),min(actual))
ymax <- max(max(master),max(pattern),max(predict),max(actual))

#Plot master and final pattern points
plot(pattern,xlim = c(1,L+P),ylim=c(ymin,ymax))
points(master,col='red',pch=4)

#Plot prediction pattern and actual prediction points (if available)
prediction_range <- (L+1):(L+P)
points(prediction_range,predict,col='blue',pch=11)
points(prediction_range,actual,col='green',pch=4)
