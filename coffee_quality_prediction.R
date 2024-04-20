#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(stringr)
library(lubridate)
library(dplyr)
library(MASS)
library(tidyr)
library(factoextra)
library(psych)
library(GGally)
library(readxl)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df <- read.csv("/Users/nuo/Documents/高级统计学/coffee_quality.csv")
View(df)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# EDA
summary(df)
cov(df)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# std
st_df <- as.data.frame(scale(df))
df2 <- subset(st_df, select = -quality_score)

# change data type
df2 <- data.frame(df2)
quality_score<- data.frame(quality_score)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pca_result <- prcomp(df2, scale = TRUE)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(pca_result)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pca_result$rotation

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Visualize
fviz_eig(pca_result, addlabels = TRUE)
# Graph of the variables
fviz_pca_var(pca_result, col.var = "black")
fviz_cos2(pca_result, choice = "var", axes = 1:2)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(caret)
library(glmnet)

# split
set.seed(123)
splitIndex <- createDataPartition(df$quality_score, p = 0.7, list = FALSE)
train_data <- df[splitIndex,]
test_data <- df[-splitIndex,]

# std
train_data_scaled <- scale(train_data[, -which(names(train_data) == "quality_score")])

# PCA
pca <- prcomp(train_data_scaled, center = TRUE, scale. = TRUE)

train_data_pca <- data.frame(predict(pca, train_data_scaled)[, c("PC1", "PC2", "PC4")])
train_data_pca$quality_score <- train_data$quality_score

# build PCA model
pca_model <- lm(quality_score ~ PC1 + PC2 + PC4, data = train_data_pca)

test_data_scaled <- scale(test_data[, -which(names(test_data) == "quality_score")])
test_data_pca <- data.frame(predict(pca, test_data_scaled)[, c("PC1", "PC2", "PC4")])

# predictions
predictions <- predict(pca_model, newdata = test_data_pca)

# mse
mean((predictions - test_data$quality_score)^2)

summary(pca_model)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(pca_model)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot result
ggplot(test_data, aes(x = quality_score, y = predictions)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("PCA prediction") +
  xlab("quality score") +
  ylab("predictions")

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fa_result <- factanal(df2, factors = 5)
print(fa_result)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
loadings <- fa_result$loadings

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fa.diagram(loadings)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(loadings, type='n', main='Varimax rotated factor analysis loadings')
text(loadings, rownames(loadings))

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 切分數據為訓練集和測試集
set.seed(123) # 確保結果可重現
splitIndex <- createDataPartition(df$quality_score, p = .70, list = FALSE)
train_data <- df[splitIndex,]
test_data <- df[-splitIndex,]

# build regression model
model <- lm(quality_score ~ Aroma + Flavor + Aftertaste + Acidity + Body + Balance + Uniformity + Clean.Cup + Sweetness , data = train_data)
summary(model)

# predictions
predictions <- predict(model, newdata = test_data)

# mse
mean((predictions - test_data$quality_score)^2)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot result
ggplot(test_data, aes(x = quality_score, y = predictions)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("prediction") +
  xlab("quality score") +
  ylab("predictions")

#' 
#' 
#' 
#' 
#' 
