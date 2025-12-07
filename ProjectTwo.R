#PROJECT 2 - Predicting Delays in Crime Reporting in Los Angeles

library(fastDummies)
library(SignifReg)
library(car)
library(MASS)

#evaluation function 

results <- list()

evaluate_model <- function(model, eval_data, response="Delay_days") {
  preds <- predict(model, newdata=eval_data)
  RMSE <- sqrt(mean((eval_data[[response]] - preds)^2))
  MAE  <- mean(abs(eval_data[[response]] - preds))
  SSE  <- sum((eval_data[[response]] - preds)^2)
  SST  <- sum((eval_data[[response]] - mean(eval_data[[response]]))^2)
  R2   <- 1 - SSE/SST
  
  return(c(RMSE=RMSE, MAE=MAE, R2=R2))
}

# Import Delay Days Data
data <- read.csv("Delay_Days_Crime_Data.csv")

# Learning about the data

print("Variables:")
print(names(data))

print("Summary:")
print(summary(data))




#####CLEANING DATASET

#shorten length of dataset - get random samples
set.seed(123)
data <- data[sample(1:nrow(data), size = 20000), ]

# Because Delay_days is a calculation of Date.Rptd - DATE.OCC, inclusion of these values
# in the model would create perfect multicollinearity, therefore in order to extract
# useful data from these values we can create new variables DayOfWeek and Month
# and remove the original ones from the model

data$DATE_OCC <- as.Date(data$DATE.OCC)

data$DayOfWeek <- weekdays(data$DATE_OCC)
data$Month     <- months(data$DATE_OCC)

# Mocodes in the data contains multiple codes in the same cell separated by a space
# to fix this for the model we decided to create a new variable with the number
# of codes included and only state the primary Mo code which every data point contains

mo_split <- strsplit(data$Mocodes, " ")

data$MO_count <- sapply(mo_split, length)
data$MO_count[is.na(data$MO_count)] <- 0

data$MO_primary <- sapply(mo_split, `[`, 1)
data$MO_primary[is.na(data$MO_primary) | data$MO_primary == ""] <- "NONE"

# LAPD only records Weapon.Used.Cd when there was a weapon involved, we will test 
# two models one where Weapon's are treated as dummy variables (0 no weapon 1 weapon)
# and a second model where it is coded categorically and imputing 0 for the missing
# values 

# MODEL 1: Dummy variables
data$Weapon_dummy <- ifelse(is.na(data$Weapon.Used.Cd), 0, 1)

# MODEL 2:categorical
data$Weapon_code <- data$Weapon.Used.Cd
data$Weapon_code[is.na(data$Weapon_code)] <- 0

#Cleaned dataset variables

data_clean <- data[, c("Delay_days", "AREA", "Crm.Cd", "Premis.Cd","Vict.Age", "Vict.Sex", "Vict.Descent","TIME.OCC", "LAT", "LON","MO_count", "MO_primary","DayOfWeek", "Month", "Weapon_dummy", "Weapon_code")]

data_clean[] <- lapply(data_clean, function(x) {
  if (is.numeric(x)) return(x) else return(as.numeric(as.factor(x)))
})

#Creating dummy variables

small_cat_vars <- c("Vict.Sex", "Vict.Descent", "DayOfWeek", "Month")

all_cat_vars <- c("AREA","Crm.Cd","Premis.Cd","Vict.Sex", "Vict.Descent","MO_primary","DayOfWeek", "Month","Weapon_code")

# only small categorical variables, large ones are too much for the machine to handle 
full <- dummy_cols(data_clean,select_columns = small_cat_vars, remove_first_dummy = TRUE,remove_selected_columns = TRUE)

#make all of the variables numeric
full <- data_clean
full[] <- lapply(full, function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(as.numeric(as.factor(x)))
  }
})



#####TESTING INITIAL MODEL

initial_model <- lm(Delay_days ~ ., data = full)
print("Initial Model Summary")
print(summary(initial_model))

print("Initial Model AIC:")
print(AIC(initial_model))
print("Initial Model BIC:")
print(BIC(initial_model))
plot(initial_model)

print("Initial Model VIF")
print(vif(initial_model))

results$InitialModel <- evaluate_model(initial_model, data)


# test different weapons models

# Model A: Weapon_dummy (0 = no weapon, 1 = weapon)
model_weapon_A <- lm(Delay_days ~ Weapon_dummy + ., data = full[, !(names(full) %in% c("Weapon_code"))])

print("Weapon dummy encoded - model A")
print(summary(model_weapon_A))


# Model B: Weapon_code (categorical)
weapon_B_cols <- grep("Weapon_code", names(full), value = TRUE)

model_weapon_B_formula <- as.formula(paste("Delay_days ~", paste(c(weapon_B_cols), collapse = " + ")))

model_weapon_B <- lm(model_weapon_B_formula, data = full)

print("Weapon categorical - Model B")
print(summary(model_weapon_B))


# Compare AIC
cat("weapon codes compare AIC")
print(AIC(model_weapon_A, model_weapon_B))

results$WeaponA <- evaluate_model(model_weapon_A, full)
results$WeaponB <- evaluate_model(model_weapon_B, full)

###seperate continuous and categorical variables

continuous_variables <- c("Vict.Age","TIME.OCC","LAT","LON", "MO_count","Weapon_dummy")

continuous_data <- full[, continuous_variables]

categorical_variables <- setdiff(names(full),c("Delay_days", continuous_variables))

categorical_data <- full[, categorical_variables]

#####PCA

#continuous

pca_cont <- prcomp(continuous_data, scale = TRUE)

print("PCA continuous summary")
print(summary(pca_cont))


var_expl <- summary(pca_cont)$importance[3,]
num_cont_PCs <- which(var_expl >= 0.90)[1] #only keep PCS that will explain more than 90 percent of the variance

print("PC's being kept")
print(num_cont_PCs)

for (i in 1:num_cont_PCs) {
  full[[paste0("CONT_PC", i)]] <- pca_cont$x[, i]
}

#categorical

pca_cat <- prcomp(categorical_data, scale = TRUE)

print("PCA categorical summary")
print(summary(pca_cat))

var_expl_cat <- summary(pca_cat)$importance[3,]
num_cat_PCs <- which(var_expl_cat >= 0.90)[1]#only keep PCS that will explain more than 90 percent of the variance

print("PC's being kept")
print(num_cat_PCs)

for (i in 1:num_cat_PCs) {
  full[[paste0("CAT_PC", i)]] <- pca_cat$x[, i]
}


### Split training and testing  data
set.seed(123)
n <- nrow(full)
train_idx <- sample(1:n, size = 0.8*n)

train <- full[train_idx, ]
test  <- full[-train_idx, ]

#model with pca

pc_names <- c(paste0("CONT_PC",1:num_cont_PCs),paste0("CAT_PC",1:num_cat_PCs))

pca_formula <- as.formula( paste("Delay_days ~", paste(pc_names, collapse=" + ")))

pca_model <- lm(pca_formula, data=train)

print("PCA model summary")
print(summary(pca_model))

print("VIF PCA model")
print(vif(pca_model))

plot(pca_model)

results$PCA_Model <- evaluate_model(pca_model, test)

#forward and backward selection

null <- lm(Delay_days ~ 1, data=train)
full <- lm(pca_formula, data=train)

scope_pc <- list(lower = formula(null), upper  = formula(full))


# Forward Selection

forward_model <- SignifReg(fit = null, scope = scope_pc, direction = "forward",criterion = "AIC",adjust.method = "fdr",trace = FALSE)
print("forward summary")
print(summary(forward_model))
results$Forward_Model <- evaluate_model(forward_model, test)


# Backward Selection
backward_model <- SignifReg(fit = full,scope = scope_pc,direction = "backward",criterion = "AIC",adjust.method = "fdr",trace = FALSE)
print("backwards summary")
print(summary(backward_model))
results$Backwards_Model <- evaluate_model(forward_model, test)


#stepwise selection
stepwise_model <- SignifReg(fit = null,scope = scope_pc,direction = "both", criterion = "AIC", adjust.method = "fdr", trace = FALSE)

print("stepwise Summary")
print(summary(stepwise_model))
results$Stepwise_Model <- evaluate_model(stepwise_model, test)



# forwards/backwards/stepwise choosing best model


model_selection_AIC <- c(forward = AIC(forward_model),backward = AIC(backward_model),stepwise = AIC(stepwise_model))

print("use AIC for model selection:")
print(model_selection_AIC)

best_selection_name <- names(which.min(model_selection_AIC)) #minimizing AIC
print("Best model BEFORE transformations:")
print(best_selection_name)

# assign selected model object
selected_model <- switch(best_selection_name,forward = forward_model,backward = backward_model,stepwise = stepwise_model)
selected_formula <- formula(selected_model)


# transformations - log

train$logDelay <- log(train$Delay_days + 1) #transform delay_days

log_model <- lm(update(selected_formula, logDelay ~ .), data=train)

print("log on selected model")
print(summary(log_model))

plot(log_model)
results$Log_Model <- evaluate_model(log_model, test)



#transformations - box-cox

bc <- boxcox(update(selected_formula, Delay_days ~ .), data = train, plotit = FALSE)
lambda_bc <- bc$x[which.max(bc$y)]
print("bc lambda ")
print(lambda_bc)

bc_transform <- function(y, lambda) {
  if (lambda == 0) log(y) else (y^lambda - 1) / lambda
}

train$Delay_BC <- bc_transform(train$Delay_days + 1, lambda_bc)
boxcox_model <- lm(update(pca_formula, Delay_BC ~ .), data = train)

print("box cox on selected summary")
print(summary(boxcox_model))
results$BoxCox_Model <- evaluate_model(boxcox_model, test)



#####FINAL SUMMARY AND VISALIZATIONS



print("final summary :")
print(results)

# choose best final model with adjusted r2
R2_values <- sapply(results, function(x) x["R2"])
best_final_model_name <- names(which.max(R2_values))

print("Best final model : ")
print(best_final_model_name)

# save best final model
best_final_model <- switch(best_final_model_name,PCA_Model = pca_model,Log_Model = log_model,BoxCox_Model = boxcox_model,Forward_Model = forward_model,Backward_Model = backward_model,Stepwise_Model = stepwise_model)

print("Best Coefficients :")
print(coef(best_final_model))

print("Chosen Predictors")
print(setdiff(names(coef(best_final_model)), "(Intercept)"))

best_preds <- predict(best_final_model, newdata=test)
plot(test$Delay_days, best_preds)
