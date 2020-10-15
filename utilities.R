
# Find tasks, where starting distance quals ending distance
false_buzzer_cleaner <- function(x){
  subject_data_cleaned <- x
  if(0 %in% subject_data_cleaned$taskNumber){
    subject_data_cleaned <- subject_data_cleaned[-which(subject_data_cleaned$taskNumber == 0),]
  }
  for (i in 1:length(unique(subject_data_cleaned$taskNumber))){
    false_buzzer_hits <- (subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)][1] == subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)][length(subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)])])
    
    if (false_buzzer_hits == T) {
      subject_data_cleaned <- subject_data_cleaned[-(which(subject_data_cleaned$taskNumber == i)),]
    }
  }
  return(subject_data_cleaned)
}

# Cutting out the time between final placement and buzzer hit
# Cuts task when there is no movement at the end (Time Lag: 5 Periods)
# always use AFTER false buyyer cleaner!

final_placement_cleaner <- function(x){ 
  subject_data_cleaned <- x
  xyz <- subject_data_cleaned$currentObjectRotationX +
    subject_data_cleaned$currentObjectRotationY +
    subject_data_cleaned$currentObjectRotationZ
  
  for (i in unique(subject_data_cleaned$taskNumber)){
    #calculate lagged distance change
    dist_lag5 <- c(rep(NA, 5), subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)][1:(length(subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)]) - 5)])
    distance_pause5 <- subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)] - dist_lag5
    
    #calculate lagged rotation change
    xyz_lag5 <- c(rep(NA, 5), xyz[which(subject_data_cleaned$taskNumber == i)][1:(length(subject_data_cleaned$distance[which(subject_data_cleaned$taskNumber == i)]) - 5)])
    xyz_pause5 <- xyz[which(subject_data_cleaned$taskNumber == i)] - xyz_lag5
    
    #calculate the period where the last lagged change in both conditions has happened
    cut_off_point <- which(distance_pause5 == 0 & xyz_pause5 == 0)[last(which(diff(which(distance_pause5 == 0 & xyz_pause5 == 0)) != 1))+1]
    if(length(cut_off_point) != 0){
      if (is.na(cut_off_point) == F){
        subject_data_cleaned[which(subject_data_cleaned$taskNumber == i),][(cut_off_point:nrow(subject_data_cleaned[which(subject_data_cleaned$taskNumber == i),])),] <- NA
      }
    
    
    subject_data_cleaned <- na.omit(subject_data_cleaned)
    return(subject_data_cleaned)
    }else{
      return(x)
    }
  }
}

correct_rotations_cleaner <- function(x){ #x is subject_data
  
  t <- x
  
  t$currentObjectRotationX[which(t$currentObjectRotationX > 180)] <- abs(t$currentObjectRotationX[which(t$currentObjectRotationX > 180)] -180)
  t$currentObjectRotationY[which(t$currentObjectRotationY > 180)] <- abs(t$currentObjectRotationY[which(t$currentObjectRotationY > 180)] - 180)
  t$currentObjectRotationZ[which(t$currentObjectRotationZ > 180)] <- abs(t$currentObjectRotationZ[which(t$currentObjectRotationZ > 180)] - 180)
  
  t$targetRotationX[which(t$targetRotationX > 180)] <- abs(t$targetRotationX[which(t$targetRotationX > 180)] -180)
  t$targetRotationY[which(t$targetRotationY > 180)] <- abs(t$targetRotationY[which(t$targetRotationY > 180)] - 180)
  t$targetRotationZ[which(t$targetRotationZ > 180)] <- abs(t$targetRotationZ[which(t$targetRotationZ > 180)] - 180)
  
 # t$currentObjectRotationY[which(t$currentObjectRotationY - t$targetRotationY > 90)] <- abs(t$currentObjectRotationY[which(t$currentObjectRotationY - t$targetRotationY > 90)] - 180)
  #t$currentObjectRotationX[which(t$currentObjectRotationX - t$targetRotationX > 90)] <- abs(t$currentObjectRotationX[which(t$currentObjectRotationX - t$targetRotationX > 90)] - 180)
  #t$currentObjectRotationZ[which(t$currentObjectRotationZ - t$targetRotationZ > 90)] <- abs(t$currentObjectRotationZ[which(t$currentObjectRotationZ - t$targetRotationZ > 90)] - 180)
  
  return(t)
}

find_ballistic_normalized <- function(task_stats){
  time <- normalize(task_stats$elapsedTime, method = "range")
  distance <- task_stats$distance
  
  ballistic_begin <- time[which(diff(sign(diff(distance)))==-1)+1][1]
  
  if (is.na(ballistic_begin)) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
    
  }
  
  if(is.na(time[which(diff(sign(diff(distance)))==1)+1][1]) == F){
    if (time[which(diff(sign(diff(distance)))==1)+1][1] < ballistic_begin) {
      ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
    }
  }
  
  
  ballistic_end <- time[which(diff(sign(diff(distance)))>=1 & time > ballistic_begin)+1][1]
  
  if ((ballistic_end - ballistic_begin) > 0.5) {
    ballistic_end <- time[which(diff(sign(diff(distance)))==2)+1][1]
  }
  if ((ballistic_end - ballistic_begin) < 0.01) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
  }
  
  if (ballistic_begin > ballistic_end) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
  }
  
  if (ballistic_begin > ballistic_end) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==2)+1][1]
    ballistic_end <- time[which(diff(sign(diff(distance)))==2)+1][2]
  }
  
  ballistic_length <- ballistic_end - ballistic_begin
  
  return(c(ballistic_begin, ballistic_end, ballistic_length))
}


find_ballistic_absolute <- function(task_stats){
  time <- task_stats$elapsedTime - min(task_stats$elapsedTime)
  distance <- task_stats$distance
  
  ballistic_begin <- time[which(diff(sign(diff(distance)))==-1)+1][1]
  
  if (is.na(ballistic_begin)) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
    
  }
  
  if(is.na(time[which(diff(sign(diff(distance)))==1)+1][1]) == F){
    if (time[which(diff(sign(diff(distance)))==1)+1][1] < ballistic_begin) {
      ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
    }
  }
  
  
  ballistic_end <- time[which(diff(sign(diff(distance)))>=1 & time > ballistic_begin)+1][1]
  
  if ((ballistic_end - ballistic_begin) > 0.5) {
    ballistic_end <- time[which(diff(sign(diff(distance)))==2)+1][1]
  }
  if ((ballistic_end - ballistic_begin) < 0.01) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
  }
  
  if (ballistic_begin > ballistic_end) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==1)+1][1]
  }
  
  if (ballistic_begin > ballistic_end) {
    ballistic_begin <- time[which(diff(sign(diff(distance)))==2)+1][1]
    ballistic_end <- time[which(diff(sign(diff(distance)))==2)+1][2]
  }
  
  ballistic_length <- ballistic_end - ballistic_begin
  
  return(c(ballistic_begin, ballistic_end, ballistic_length))
}

# function to compute average silhouette for k clusters
avg_sil <- function(xvec, yvec, zvec, k) {
  km.res <- kmeans(cbind(xvec, yvec, zvec), centers = k, nstart = 25, iter.max = 25)
  ss <- silhouette(km.res$cluster, dist(cbind(xvec, yvec, zvec)))
  mean(ss[, 3])
}

# function to determine optimal k and do kmeans clustering on it

kmeans_opt <- function(xvec, yvec, zvec){
  set.seed <- 1
  avg_sil_values <- c()
  for(j in 2:15){
    k.values <- j
    avg_sil_values[j] <- avg_sil(xvec, yvec, zvec, k.values)
  }
  k_opt <- which(avg_sil_values == max(avg_sil_values, na.rm = T))
  
  set.seed <- 1
  kmeans_result <-  kmeans(cbind(xvec, yvec, zvec), centers = k_opt, nstart = 25, iter.max = 25)
  return(list("kmeans" = kmeans_result, "sill_vals" = avg_sil_values))
}

#Cluster Statistics
cluster_stats <- function(xvec, yvec, zvec){
  clustering_output <- kmeans_opt(xvec, yvec, zvec)
  
  cluster_center_target_distance <- c()
  for(k in 1:nrow(clustering_output$kmeans$centers)){
    cluster_center_target_distance[k] <- dist(rbind(clustering_output$kmeans$centers[k,], c(mean(subject_data$currentTarget.transform.position.x), mean(subject_data$currentTarget.transform.position.y), mean(subject_data$currentTarget.transform.position.z))))
  }
  distance_weights <- clustering_output$kmeans$size / sum(clustering_output$kmeans$size)
  average_cluster_distance <- weighted.mean(cluster_center_target_distance, distance_weights)
  
  return(c("NoClusters" = nrow(clustering_output$kmeans$centers), "WeighedAvgDistanceToTarget" = average_cluster_distance))
}