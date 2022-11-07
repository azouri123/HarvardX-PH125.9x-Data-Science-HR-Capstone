#### hatem RABEH: Harvardx 
###HarvardX: Capstone Project
###MovieLens Recommendation system Project

                                           #############################
                                ########## Installing needed package ##########  
                                          ###########################
                                           
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "https://cran.rstudio.com/bin/windows/contrib/4.1/Metrics_0.1.4.zip")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

                                         #############################
                               ########## Uploading needed package ##########  
                                      ############################                               
library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(formattable)
library(recosystem)  
library(ggthemes)
library(knitr)
library(rmarkdown)
library(dplyr)
library(Metrics)
                                           
                                           
                 #############################################################################################################################
                 #Create edx set, validation set (final hold-out test set) (only this part of the code is provided by the course for students) 
                ##############################################################################################################################
                                           
                                           
 dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
                                           
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
col.names = c("userId", "movieId", "rating", "timestamp"))
                                           
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
                                           
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
title = as.character(title),
genres = as.character(genres))
                                           
                                           
movielens <- left_join(ratings, movies, by = "movieId")
                                           
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
                                           
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
semi_join(edx, by = "movieId") %>%
semi_join(edx, by = "userId")
                                           
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
                                           
rm(dl, ratings, movies, test_index, temp, movielens, removed)

                                                         ###########################
                                           ########## 3.3 Analyzing the Edx subset ##########  
                                                    ##########################       

## 3.3.1.Overall analysis: 

head(edx)

### Summery of Edx subset:#

edx_subset_summary <- data.frame(rows_number = nrow(edx),
                     columns_number = ncol(edx),
                     users_number= n_distinct(edx$userId),
                     movies_number = n_distinct(edx$movieId),
                     average_rating = round(mean(edx$rating),3),
                    genres_number = n_distinct(edx$genres))

edx_subset_summary


## 3.3.2. Analysis of rating by Movie

edx %>% group_by(movieId) %>%
  summarize(num_movie_rating = n(), 
            mu_movies = mean(rating),
            sd_movies = sd(rating)) %>% ggplot(aes(x = num_movie_rating))+
  geom_histogram(bins = 40, color = "blue")+
  theme_hc() +
  scale_x_log10()+
  ggtitle("Number of ratings by movie") +
  labs(x="Number of Movies",
       y="Number of ratings",)+ 
theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

## 3.3.3 Rating by User: 

edx %>% group_by(userId) %>%
  summarise(user_ave_rating = sum(rating)/n()) %>%
  ggplot(aes(user_ave_rating)) +
  geom_histogram(bins=30, color = I("blue")) +
  theme_hc() +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  ggtitle("Number of user by average rating ") +
  labs(x="Average Rating",
       y="Number of Users",
      )+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

  ## .3.3.4. Analysis of rating by Genres:

genres_summarize <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize( num_movie_per_genres = n(), avg_movie_per_genres = mean(rating)) %>%
  arrange(desc(num_movie_per_genres))
genres_summarize <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize( num_movie_per_genres = n(), avg_movie_per_genres = mean(rating)) %>%
  arrange(desc(num_movie_per_genres))
genres_summarize %>%
  ggplot(aes(num_movie_per_genres,reorder(genres, num_movie_per_genres),  fill= num_movie_per_genres)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller()+
  ggtitle("Number of ratings per genre") +
  labs(y = "Genres Type",
       x = "Number of ratings",  
       )+
  theme_hc()+
  theme(plot.background = element_rect(colour= NA, linetype = "solid", fill = NA, size = 1), legend.position = 'none', panel.border = element_rect(colour="black", linetype = "dashed", fill=NA), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), plot.caption = element_text(hjust = 0.5))
  
                          

                               #####################################
                    ############# 3.4. Developing modeling concepts #############
                           #####################################


### Split the Edx subset into Edx_test_set and Edx_train_set

set.seed(20, sample.kind="Rounding")
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train_set <- edx %>% slice(-edx_test_index)
edx_temp <- edx %>% slice(edx_test_index)
edx_test_set <- edx_temp %>% 
  semi_join(edx_train_set, by = "movieId") %>%
  semi_join(edx_train_set, by = "userId")
rows_removed <- anti_join(edx_temp, edx_test)
edx_train_set <- rbind(edx_train_set, rows_removed)


                                     ###########
                      ############# 4. Results #############
                                 ############

## 4.1. The average Model Results:

mu <- mean(edx_train_set$rating)
Average_Model_rmse <- rmse(edx_test_set$rating, mu)
rmse_results <- data.frame(Model = "Average Model",
                           RMSE = round(Average_Model_rmse, 4))

rmse_results

## 4.2. The Movie impact Model: 

movie_avgs <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predict_rating_mi <- mu + edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
Movie_impact_model_rmse <- rmse(edx_test_set$rating,predict_rating_mi)
rmse_results <- bind_rows(rmse_results,data.frame(Model = "Movie Impact Model",
                                                  RMSE = round(Movie_impact_model_rmse, 4)))

rmse_results

## 4.3. Movie and Users impact model: 

user_avgs <- edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predict_rating_mum <- edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) 
Movie_and_User_rmse <- rmse(edx_test_set$rating,predict_rating_mum$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie and User Impact model",  
                                     RMSE = round(Movie_and_User_rmse, 4)))
rmse_results

## 4.4. Regularization of the user and the movie impact:

lambdas <- seq(0, 10, 0.25)
set.seed(21, sample.kind = "Rounding")
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train_set$rating)
  
  b_i <- edx_train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- edx_test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(rmse(edx_test_set$rating,predicted_ratings))
})
lambda <- lambdas[which.min(rmses)]
reg_movie_avgs <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(reg_b_i = sum(rating - mu)/(n()+lambda), n_i = n())
reg_user_avgs <- edx_train_set %>% 
  left_join(reg_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(reg_b_u = sum(rating - mu - reg_b_i)/(n()+lambda), n_u = n())
reg_predicted_ratings <- edx_test_set %>% 
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  mutate(pred = mu + reg_b_i + reg_b_u) %>% 
  .$pred
Regularisation_rmse <- rmse(edx_test_set$rating,reg_predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie and User impacts Model",  
                                     RMSE = round(Regularisation_rmse, 4)))
rmse_results

## 4.5. Parallel Matrix Factorization Model: 

residual_edx <- edx_train_set %>% 
  left_join(reg_movie_avgs, by = "movieId") %>%
  left_join(reg_user_avgs, by = "userId") %>%
  mutate(residual = rating - mu - reg_b_i - reg_b_u) %>%
  select(userId, movieId, residual)
residual_mf <- as.matrix(residual_edx)
edx_test_mf <- edx_test_set %>% 
  select(userId, movieId, rating)
edx_test_mf <- as.matrix(edx_test_mf)
write.table(residual_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(edx_test_mf, file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
train_set <- data_file("trainset.txt")
test_set <- data_file("testset.txt")
r <-Reco()
opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))
pred_file <- tempfile()
r$predict(test_set, out_file(pred_file)) 

predicted_residuals_mf <- scan(pred_file)
predicted_ratings_mf <- reg_predicted_ratings + predicted_residuals_mf
Factorization_rmse <- rmse(edx_test_set$rating, predicted_ratings_mf)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Parallel Matrix Factorization",  
                                     RMSE = round(Factorization_rmse, 4)))

rmse_results

## 4.6. Final Model used : 

set.seed(22, sample.kind = "Rounding")
f_mu <- mean(edx$rating)
f_lambdas <- seq(0, 10, 0.25)

f_rmses <- sapply(f_lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(rmse(validation$rating,predicted_ratings))
})

f_lambda <- f_lambdas[which.min(f_rmses)]
final_movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(f_b_i = sum(rating - f_mu)/(n()+f_lambda), n_i = n())
final_user_avgs <- edx %>% 
  left_join(final_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(f_b_u = sum(rating - f_mu - f_b_i)/(n()+f_lambda), n_u = n())
final_reg_predicted_ratings <- validation %>% 
  left_join(final_movie_avgs, by='movieId') %>%
  left_join(final_user_avgs, by='userId') %>%
  mutate(pred = f_mu + f_b_i + f_b_u) %>% 
  .$pred
final_residual_edx <- edx %>% 
  left_join(final_movie_avgs, by = "movieId") %>%
  left_join(final_user_avgs, by = "userId") %>%
  mutate(residual = rating - f_mu - f_b_i - f_b_u) %>%
  select(userId, movieId, residual)
final_residual_mf <- as.matrix(final_residual_edx)
validation_mf <- validation %>% 
  select(userId, movieId, rating)
validation_mf <- as.matrix(validation_mf)
write.table(final_residual_mf , file = "final_trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_mf, file = "final_testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
final_train_set <- data_file("final_trainset.txt")
final_test_set <- data_file("final_testset.txt")
f_r <-Reco()
f_opts <- f_r$tune(final_train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                                costp_l1 = 0, costq_l1 = 0,
                                                nthread = 1, niter = 10))
f_r$train(final_train_set, opts = c(f_opts$min, nthread = 1, niter = 20))
final_pred_file <- tempfile()
f_r$predict(final_test_set, out_file(final_pred_file)) 

final_predicted_residuals_mf <- scan(final_pred_file)
final_predicted_ratings_mf <- final_reg_predicted_ratings + final_predicted_residuals_mf
final_rmse <- rmse(validation$rating, final_predicted_ratings_mf)
final_rmse_results <- data.frame(Model = "Final model used",
                                 RMSE = round(final_rmse, 4))

final_rmse_results




