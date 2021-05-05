# Set Up ------------------------------------------------------------------
library(ggplot2)
library(randomForest)
library(pROC)

# start with a clean slate!
rm(list = ls())

# Set working directory
setwd("~/DRAKE_DEV/SPRING_2021/STAT_190/newJob")

# Read in data
jobs <- read.csv("aug_train.csv")


# Clean Data --------------------------------------------------------------
# Currently any missing value is stored as a blank character, "", rather than NA
# First for each variable we make the missing values = NA
# Then I factor each categorical variable
jobs$enrollee_id[jobs$enrollee_id == ""] <- NA

jobs$city[jobs$city == ""] <- NA
jobs$city <- as.factor(jobs$city)

jobs$city_development_index[jobs$city_development_index == ""] <- NA


jobs$gender[jobs$gender == ""] <- NA
jobs$gender <- as.factor(jobs$gender)

jobs$relevent_experience[jobs$relevent_experience == ""] <- NA
jobs$relevent_experience <- as.factor(jobs$relevent_experience)

jobs$enrolled_university[jobs$enrolled_university == ""] <- NA
jobs$enrolled_university <- as.factor(jobs$enrolled_university)

jobs$education_level[jobs$education_level == ""] <- NA
jobs$education_level <- as.factor(jobs$education_level)

jobs$major_discipline[jobs$major_discipline == ""] <- NA
jobs$major_discipline <- as.factor(jobs$major_discipline)

jobs$experience[jobs$experience == ""] <- NA
jobs$experience <- factor(jobs$experience ,
                          levels = c("<1", "1", "2", "3", "4", "5", "6", "7", "8",
                                     "9", "10", "11", "12", "13", "14", "15", "16",
                                     "17", "18", "19", ">20"))

jobs$company_size[jobs$company_size == ""] <- NA
jobs$company_size[jobs$company_size == "10/49"] <- "10-49"
jobs$company_size <- as.factor(jobs$company_size)

jobs$company_type[jobs$company_type == ""] <- NA
jobs$company_type <- as.factor(jobs$company_type)

jobs$last_new_job[jobs$last_new_job == ""] <- NA
jobs$last_new_job <- as.factor(jobs$last_new_job)

jobs$training_hours[jobs$training_hours == ""] <- NA
jobs$training_hours <- as.numeric(jobs$training_hours)

jobs$target[jobs$target == ""] <- NA
jobs$target <- as.factor(jobs$target)

# Use na.roughfix to fill in the missing values so there will not be any errors
# when creating the random forest
jobs <- na.roughfix(jobs)


# Visualizations ----------------------------------------------------------

# Looking at City Development Index
ggplot(data = jobs) +
  geom_jitter(aes(x = target, y = city_development_index), alpha = I(.4), colour = "grey") +
  geom_boxplot(aes(x = target, y = city_development_index), alpha = 0) +
  labs(x = "Target", y = "City Development Index") +
  theme_bw()

# Looking at relevent experience
ggplot(data = jobs) +
  geom_bar(aes(x = relevent_experience, fill = target), position="fill") +
  ggtitle("Relation of Relevent Experience vs Target") +
  labs(x = "Relevent Experience",
       y = "Proportional Amount") +
  scale_fill_grey("Target") + 
  scale_colour_grey("Target") + 
  theme_bw()

# Now just experience
ggplot(data = jobs) +
  geom_bar(aes(x = experience, fill = target), position="fill") +
  ggtitle("Relation of Experience vs Target") +
  labs(x = "Experience",
       y = "Proportional Amount") +
  scale_fill_grey("Target") + 
  scale_colour_grey("Target") + 
  theme_bw()

# Looking at number of training hours
ggplot(data = jobs) +
  geom_jitter(aes(x = target, y = training_hours), alpha = I(.4), colour = "grey") +
  geom_boxplot(aes(x = target, y = training_hours), alpha = 0) +
  labs(x = "Target", y = "Number of Training Hours") +
  theme_bw()

# Random Forest -----------------------------------------------------------
# Set seed and create training and testing data sets
RNGkind(sample.kind = "default")
set.seed(190)
train.idx <- sample(x = 1:nrow(jobs), size = floor(.8*nrow(jobs)))
train.df <- jobs[train.idx,]
test.df <- jobs[-train.idx,] 

# Create baseline forest with high mtry, ntree
# Using every variable except the ID and city. City is a factor with more than
# 53 levels, so the random forest will not work.
myforest <- randomForest(target ~ city_development_index + gender + relevent_experience +
                           enrolled_university + education_level + major_discipline +
                           experience + company_size + company_type + last_new_job + training_hours,
                         data = train.df,
                         ntree = 1000,
                         mtry = 11, 
                         importance = TRUE) 
# Tuning forest
mtry <- c(1:11) # Since we have 11 x variables
keeps2 <- data.frame(m = rep(NA,length(mtry)),
                     OOB_err_rate = rep(NA, length(mtry)))

# Loop through forests for each mtry
for (idx in 1:length(mtry)){
  tempforest<- randomForest(target ~ city_development_index + gender + relevent_experience +
                              enrolled_university + education_level + major_discipline +
                              experience + company_size + company_type + last_new_job + training_hours,
                            data = train.df,
                            ntree = 1000,
                            mtry = mtry[idx])
  keeps2[idx, "m"] <- mtry[idx]
  keeps2[idx,"OOB_err_rate"] <- mean(predict(tempforest)!= train.df$target)
  print(mtry[idx])
}
qplot(m, OOB_err_rate, geom = c("line", "point"), data = keeps2) + 
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate")

# Despite sqrt(11) = 3.3, the mtry value that minimized OOB error rate was
# mtry = 2, OOB = 0.2210622

# Create a final forest with mtry = 2
final_forest <- randomForest(target ~ city_development_index + gender + relevent_experience +
                           enrolled_university + education_level + major_discipline +
                           experience + company_size + company_type + last_new_job + training_hours,
                         data = train.df,
                         ntree = 1000,
                         mtry = 2, 
                         importance = TRUE) 

# Prediction
test.df$forest_pred <- predict(final_forest, test.df, type = "class")

# Prediction graph
ggplot(data = test.df) +
  geom_bar(aes(x = forest_pred, fill = target), position="fill") +
  ggtitle("Relation of Predicted Target vs Actual Target") +
  labs(x = "Predicted Overall Target",
       y = "Proportional Amount") +
  scale_fill_grey("Actual Target") + 
  scale_colour_grey("Actual Target") + 
  theme_bw()

# ROC curve
# 1 (looking for new job) is the value of interest
pi_hat <- predict(final_forest, test.df, type = "prob")[, 1]

rocCurve <- roc(response = test.df$target,
                predictor = as.vector(pi_hat),
                levels = c(0, 1))

plot(rocCurve, print.auc = TRUE, print.thres = TRUE)

varImpPlot(final_forest, type = 1)
