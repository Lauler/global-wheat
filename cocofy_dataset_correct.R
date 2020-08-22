library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)


df <- read_csv("Yet-Another-EfficientDet-Pytorch/datasets/train.csv")
df$bbox <- stringr::str_remove_all(df$bbox, pattern = "\\[|\\]")
df <- df %>%
  separate(col = bbox, into = c("x", "y", "w", "h"), sep = ",") %>%
  mutate_at(.vars = vars(x:h), .funs = function(x) stringr::str_trim(x) %>% as.numeric())

# df$id <- 0:(nrow(df)-1) # bbox id

# df_all_img <- tibble(image_id = dir("Yet-Another-EfficientDet-Pytorch/datasets/train") %>% stringr::str_remove(".jpg"))
# df_all_img$id <- 0:(nrow(df_all_img)-1) # image id

df_train_img <- tibble(image_id = dir("Yet-Another-EfficientDet-Pytorch/datasets/wheat/train") %>% stringr::str_remove(".jpg"))
df_valid_img <- tibble(image_id = dir("Yet-Another-EfficientDet-Pytorch/datasets/wheat/val") %>% stringr::str_remove(".jpg"))
df_train_img$id <- 0:(nrow(df_train_img)-1)
df_valid_img$id <- 0:(nrow(df_valid_img)-1)

instances <- list(info = list(description = "", url = "", version = "", year = "2020",
                              contributor = "", date_created = "2020-05-15"),
                  licenses = list(list(
                    id = 1,
                    name = NULL,
                    url = NULL
                  )),
                  categories = list(list(
                    id = 1, 
                    name = "wheat",
                    supercategory = "None"
                  )))


images_list <- function(df, instances){
  img_list <- vector(mode = "list", length = nrow(df))
  
  for (i in 1:nrow(df)){
    img_list[[i]]$id <- i-1
    img_list[[i]]$file_name <- paste0(df$image_id[i], ".jpg")
    img_list[[i]]$width <- 1024
    img_list[[i]]$height <- 1024
    img_list[[i]]$date_captured <- "2020-05-15"
    img_list[[i]]$license <- 1
    img_list[[i]]$coco_url <- ""
    img_list[[i]]$flickr_url <- ""
  }
  
  instances$images <- img_list
  return(instances)
}

instances_train <- images_list(df_train_img, instances)
instances_valid <- images_list(df_valid_img, instances)


# Bounding boxes

df_train_bbox <- df_train_img %>%
  inner_join(df, by = "image_id")

df_valid_bbox <- df_valid_img %>%
  inner_join(df, by = "image_id")


annotation_list <- function(df, instances){
  img_list <- vector(mode = "list", length = nrow(df))
  
  for (i in 1:nrow(df)){
    img_list[[i]]$id <- i # bbox ids
    img_list[[i]]$image_id <- df$id[i] # image id
    img_list[[i]]$category_id <- 1 
    img_list[[i]]$iscrowd <- 0 
    img_list[[i]]$area <- df$w[i] * df$h[i]
    img_list[[i]]$bbox <- c(df$x[i], df$y[i], df$w[i], df$h[i])
    img_list[[i]]$segmentation <- ""
  }
  
  instances$annotations <- img_list
  return(instances)
}

instances_train <- annotation_list(df_train_bbox, instances_train)
instances_valid <- annotation_list(df_valid_bbox, instances_valid)

jsonlite::write_json(instances_valid, path = "Yet-Another-EfficientDet-Pytorch/datasets/wheat/annotations/instances_val.json",
                     auto_unbox = TRUE)
jsonlite::write_json(instances_train, path = "Yet-Another-EfficientDet-Pytorch/datasets/wheat/annotations/instances_train.json",
                     auto_unbox = TRUE)

a <- read_json("Yet-Another-EfficientDet-Pytorch/datasets/wheat/annotations/instances_val.json")
