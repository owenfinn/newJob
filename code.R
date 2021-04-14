# start with a clean slate!
rm(list = ls())

# Set working directory
setwd("~/DRAKE_DEV/SPRING_2021/STAT_190/final")

# Read in data
jobs <- read.csv("aug_train.csv")

jobs$enrollee_id[jobs$enrollee_id == ""] <- NA
jobs$city[jobs$city == ""] <- NA
jobs$city_development_index[jobs$city_development_index == ""] <- NA
jobs$gender[jobs$gender == ""] <- NA
jobs$relevent_experience[jobs$relevent_experience == ""] <- NA
jobs$enrolled_university[jobs$enrolled_university == ""] <- NA
jobs$education_level[jobs$education_level == ""] <- NA
jobs$major_discipline[jobs$major_discipline == ""] <- NA
jobs$experience[jobs$experience == ""] <- NA
jobs$company_size[jobs$company_size == ""] <- NA
jobs$company_type[jobs$company_type == ""] <- NA
jobs$last_new_job[jobs$last_new_job == ""] <- NA
jobs$training_hours[jobs$training_hours == ""] <- NA
jobs$target[jobs$target == ""] <- NA

jobs2 <- jobs[complete.cases(jobs), ]
