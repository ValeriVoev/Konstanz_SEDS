library(sparklyr)
library(dplyr)
sc <- spark_connect(master = "local")

movies <- data.frame(
  user   = c(1, 1, 2, 3, 4, 5),
  item   = c(2, 4, 3, 2, 1, 3),
  rating = c(1, 5, 3, 4, 3, 2)
)

movies_wide <- movies %>% 
  arrange(item) %>% 
  tidyr::pivot_wider(names_from = item, values_from = rating, names_prefix = "item_") %>% 
  arrange(user)

movies_tbl <- sdf_copy_to(sc, movies, overwrite = TRUE)

model <- ml_als(movies_tbl, rating ~ user + item, rank = 10)
#model <- ml_als(movies_tbl, rating ~ user + item, reg_param = 0.0000001, rank = 10)

# new_user <- tibble(user = 5, item = 1, rating = 3)
# new_user_tbl <- sdf_copy_to(sc, new_user, overwrite = TRUE)
# 
preds <-  ml_predict(model, movies_tbl) %>% collect()

recs <- ml_recommend(model, type = "items", 3) %>% collect

model_tidy <- tidy(model) %>% 
  collect() %>% 
  arrange(id)

user_factors <-  model_tidy$user_factors %>% unlist() %>% na.omit()
dim(user_factors) <- c(10, 5)
user_factors <- t(user_factors)

item_factors <-  model_tidy$item_factors %>% unlist() %>% na.omit()
dim(item_factors) <- c(10, 4)

round(user_factors %*% item_factors, 2)

spark_disconnect(sc)