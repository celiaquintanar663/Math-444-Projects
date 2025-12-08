required <- c("data.table","dplyr","ggplot2","scales")
installed <- rownames(installed.packages())
for (p in required) if (! (p %in% installed)) install.packages(p)
library(data.table); library(dplyr); library(ggplot2); library(scales)

set.seed(2025)

train <- read.csv("train.csv")
test  <- read.csv("test.csv")
test_ids <- test$Id

dim(train)
dim(test)

get_missing_counts <- function(df) sapply(df, function(x) sum(is.na(x)))

getmode <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

miss_train_before <- sort(get_missing_counts(train), decreasing = TRUE)
miss_test_before  <- sort(get_missing_counts(test), decreasing = TRUE)

message("Top Missing from train:")
print(head(miss_train_before[miss_train_before>0], 20))

message("Top missing from test:")
print(head(miss_test_before[miss_test_before>0], 20))

# Plotting missing
miss_df <- data.frame(
  var = names(miss_train_before),
  miss = as.numeric(miss_train_before)
) %>% arrange(desc(miss)) %>% filter(miss > 0)

if (nrow(miss_df) > 0) {
  ggplot(miss_df[1:min(30,nrow(miss_df)),], aes(x = reorder(var, miss), y = miss)) +
    geom_bar(stat="identity", fill="steelblue") + coord_flip() +
    labs(title="Top missing counts (train)", x="", y="Missing count")
}

train <- train %>% mutate(across(where(is.character), as.factor))
test  <- test  %>% mutate(across(where(is.character), as.factor))

# Cleaning blocks (garage, basement, fireplace, etc.) — unchanged
garage_nums <- intersect(c("GarageYrBlt","GarageArea","GarageCars"), names(train))
garage_cats <- intersect(c("GarageType","GarageFinish","GarageQual","GarageCond"), names(train))

for (v in garage_nums) {
  train[[v]][is.na(train[[v]])] <- 0
  if (v %in% names(test)) test[[v]][is.na(test[[v]])] <- 0
}
for (v in garage_cats) {
  train[[v]] <- as.character(train[[v]])
  test[[v]]  <- as.character(test[[v]])
  train[[v]][is.na(train[[v]])] <- "Absent"
  if (v %in% names(test)) test[[v]][is.na(test[[v]])] <- "Absent"
  train[[v]] <- factor(train[[v]])
  if (v %in% names(test)) test[[v]] <- factor(test[[v]])
}

# Basement
bsmt_cats <- intersect(c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2"), names(train))
bsmt_nums <- intersect(c("BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath","BsmtHalfBath"), names(train))

for (v in bsmt_cats) {
  train[[v]] <- as.character(train[[v]])
  test[[v]]  <- as.character(test[[v]])
  train[[v]][is.na(train[[v]])] <- "Absent"
  if (v %in% names(test)) test[[v]][is.na(test[[v]])] <- "Absent"
  train[[v]] <- factor(train[[v]])
  if (v %in% names(test)) test[[v]] <- factor(test[[v]])
}
for (v in bsmt_nums) {
  train[[v]][is.na(train[[v]])] <- 0
  if (v %in% names(test)) test[[v]][is.na(test[[v]])] <- 0
}

# Fireplaces, pools, alley, misc
other_absent <- intersect(
  c("FireplaceQu","PoolQC","Fence","MiscFeature","Alley","MasVnrType"),
  names(train)
)
for (v in other_absent) {
  train[[v]] <- as.character(train[[v]])
  test[[v]]  <- as.character(test[[v]])
  train[[v]][is.na(train[[v]])] <- "Absent"
  if (v %in% names(test)) test[[v]][is.na(test[[v]])] <- "Absent"
  train[[v]] <- factor(train[[v]])
  if (v %in% names(test)) test[[v]] <- factor(test[[v]])
}

# MasVnrArea
if ("MasVnrArea" %in% names(train)) {
  idx_none <- which(tolower(as.character(train$MasVnrType)) %in% c("none","absent"))
  train$MasVnrArea[idx_none] <- 0
  if ("MasVnrType" %in% names(test)) {
    idx_none_t <- which(tolower(as.character(test$MasVnrType)) %in% c("none","absent"))
    test$MasVnrArea[idx_none_t] <- 0
  }
}

# LotFrontage neighborhood median impute
if ("LotFrontage" %in% names(train) & "Neighborhood" %in% names(train)) {
  lf_meds <- train %>% group_by(Neighborhood) %>% summarize(med = median(LotFrontage, na.rm = TRUE))
  
  for (i in 1:nrow(train)) {
    if (is.na(train$LotFrontage[i])) {
      nb <- train$Neighborhood[i]
      medv <- lf_meds$med[lf_meds$Neighborhood == nb]
      if (length(medv)==0 || is.na(medv)) medv <- median(train$LotFrontage, na.rm=TRUE)
      train$LotFrontage[i] <- medv
    }
  }
  
  for (i in 1:nrow(test)) {
    if (is.na(test$LotFrontage[i])) {
      nb <- test$Neighborhood[i]
      medv <- lf_meds$med[lf_meds$Neighborhood == nb]
      if (length(medv)==0 || is.na(medv)) medv <- median(train$LotFrontage, na.rm=TRUE)
      test$LotFrontage[i] <- medv
    }
  }
}

# Electrical
if ("Electrical" %in% names(train)) {
  mode_elec <- getmode(as.character(train$Electrical))
  train$Electrical[is.na(train$Electrical)] <- mode_elec
  if ("Electrical" %in% names(test)) test$Electrical[is.na(test$Electrical)] <- mode_elec
}

# Remainder NA fix
train <- train %>% mutate(across(where(is.character), factor))
cat_vars <- names(train)[sapply(train, is.factor)]
num_vars <- names(train)[sapply(train, is.numeric)]

for (v in cat_vars) {
  if (any(is.na(train[[v]]))) {
    mv <- getmode(as.character(train[[v]]))
    if (is.na(mv)) mv <- "Absent"
    train[[v]][is.na(train[[v]])] <- mv
  }
  if (v %in% names(test) && any(is.na(test[[v]]))) {
    mv <- getmode(as.character(test[[v]]))
    test[[v]][is.na(test[[v]])] <- mv
  }
}

for (v in num_vars) {
  medv <- median(train[[v]], na.rm = TRUE)
  if (is.na(medv)) medv <- 0
  train[[v]][is.na(train[[v]])] <- medv
  if (v %in% names(test)) test[[v]][is.na(test[[v]])] <- medv
}

sum(is.na(train))
sum(is.na(test))

# log target
train$logSale <- log(train$SalePrice)

#match factor levels
for (v in intersect(names(train), names(test))) {
  if (is.factor(train[[v]])) {
    test[[v]] <- factor(test[[v]], levels=levels(train[[v]]))
  }
}

train_num <- model.matrix(~ . - SalePrice - logSale - Id - 1, data=train)
test_num  <- model.matrix(~ . - Id - 1, data=test)

common_cols <- intersect(colnames(train_num), colnames(test_num))
train_num <- train_num[, common_cols]
test_num  <- test_num[, common_cols]

# scale
train_num_scaled <- scale(train_num)
test_num_scaled <- scale(test_num, center = attr(train_num_scaled,"scaled:center"),scale  = attr(train_num_scaled,"scaled:scale"))


pca_fit <- prcomp(train_num_scaled, center = FALSE, scale. = FALSE)
var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
cumvar <- cumsum(var_explained)

# cross validation for choosing
num_pcs <- min(60, ncol(pca_fit$x))
cat("Using", num_pcs, "PCs\n")

train_pcs <- as.data.frame(pca_fit$x[,1:num_pcs, drop=FALSE])
colnames(train_pcs) <- paste0("PC", 1:num_pcs)
train_pcs$logSale <- train$logSale

formula_pcs <- as.formula(paste("logSale ~", paste(colnames(train_pcs)[1:num_pcs], collapse=" + ")))
lm_pcs <- lm(formula_pcs, data=train_pcs)
summary(lm_pcs)

# test PCA
test_pcs_scores <- predict(pca_fit, newdata=test_num_scaled)[,1:num_pcs, drop=FALSE]
test_pcs <- as.data.frame(test_pcs_scores)
colnames(test_pcs) <- paste0("PC", 1:num_pcs)

# predict + submission
pred_log_test <- predict(lm_pcs, newdata=test_pcs)
pred_price_test <- exp(pred_log_test)

submission <- data.frame(Id=test_ids, SalePrice=as.numeric(pred_price_test))
write.csv(submission, "submission_pca.csv", row.names=FALSE)

#train rsme
train_pred_log <- predict(lm_pcs, newdata=train_pcs)
train_rmse_log <- sqrt(mean((train_pred_log - train_pcs$logSale)^2))
train_rmse_log

ggplot(data.frame(obs=train_pcs$logSale, pred=train_pred_log),
       aes(obs,pred)) +
  geom_point(alpha=0.4) +
  geom_abline(color="red") +
  labs(title="Predicted vs Observed logSale")


#knn regression on pca
knn_pcs <- min(20, num_pcs)

x_train <- as.matrix(train_pcs[,1:knn_pcs])
y_train <- train_pcs$logSale
x_test  <- as.matrix(test_pcs[,1:knn_pcs])
k_value <- 10

knn_predict_point <- function(x0, X, Y, k) {
  d <- sqrt(rowSums((X - matrix(x0, nrow(X), ncol(X), byrow=TRUE))^2))
  idx <- order(d)[1:k]
  mean(Y[idx])
}

train_pred_log_knn <- numeric(nrow(x_train))
for (i in 1:nrow(x_train)) {
  train_pred_log_knn[i] <- knn_predict_point(x_train[i,], x_train[-i,,drop=FALSE], y_train[-i], k_value)
}

sqrt(mean((train_pred_log_knn - y_train)^2))

test_pred_log_knn <- numeric(nrow(x_test))
for (i in 1:nrow(x_test)) {
  test_pred_log_knn[i] <- knn_predict_point(x_test[i,], x_train, y_train, k_value)
}

submission_knn <- data.frame(Id=test_ids, SalePrice=exp(test_pred_log_knn))
write.csv(submission_knn, "submission_knn.csv", row.names=FALSE)
head(submission)
