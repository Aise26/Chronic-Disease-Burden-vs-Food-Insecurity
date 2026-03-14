setwd("~/BUSPH/Fall Semester 2025 courses/BS 845/Project/Places")

# Sorting through PLACES data per health outcome
# Load and combine all diabetes files
library(dplyr)

diabetes_all <- bind_rows(
  read.csv("Diabetes_2019.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("Diabetes_2020.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("Diabetes_2021.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("Diabetes_2022.csv") %>% mutate(LocationName = as.character(LocationName))
)
# Filter for Massachusetts counties
diabetes_ma <- diabetes_all %>%
  filter(StateAbbr == "MA")

# Load and combine all HBP files
HBP_all <- bind_rows(
  read.csv("HBP_2019.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("HBP_2021.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("HBP_2022.csv") %>% mutate(LocationName = as.character(LocationName))
)
# Filter for Massachusetts counties
HBP_ma <- HBP_all %>%
  filter(StateAbbr == "MA")

# Load and combine all leisure-time files
leisure_all <- bind_rows(
  read.csv("leisure_time 2019.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("leisure_time 2020.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("leisure_time 2021.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("leisure_time 2022.csv") %>% mutate(LocationName = as.character(LocationName))
)
# Filter for Massachusetts counties
leisure_ma <- leisure_all %>%
  filter(StateAbbr == "MA")

# Load and combine all Obesity files
Obesity_all <- bind_rows(
  read.csv("Obesity_2019.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("Obesity_2020.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("Obesity_2021.csv") %>% mutate(LocationName = as.character(LocationName)),
  read.csv("Obesity_2022.csv") %>% mutate(LocationName = as.character(LocationName))
)
# Filter for Massachusetts counties
Obesity_ma <- Obesity_all %>%
  filter(StateAbbr == "MA")

# Merge into an aggregated health data set for Massachusetts
health_ma <- bind_rows(
  diabetes_ma %>% mutate(Condition = "Diabetes"),
  HBP_ma %>% mutate(Condition = "High Blood Pressure"),
  leisure_ma %>% mutate(Condition = "Leisure Activity"),
  Obesity_ma %>% mutate(Condition = "Obesity")
)

# Sorting Feeding America data set
library(openxlsx)
Food_all <- read.xlsx("C:/Users/user/Documents/BUSPH/Fall Semester 2025 courses/BS 845/Project/Places/Feeding America_2019-2023_Data_To_Share.xlsx", sheet = 2)

# Filter for Massachusetts counties
Food_ma <- Food_all %>%
  filter(State == "MA", Year != 2023)
View(Food_ma)
# Merging both data sets
dim(health_ma)
dim(Food_ma)

# add food insecurity data to your health dataset (so every 
# health record gets its matching food insecurity value for that county and year):
# Make both FIPS codes character strings with 5 digits
health_ma <- health_ma %>%
  mutate(CountyFIPS = sprintf("%05s", as.character(CountyFIPS)))

Food_ma <- Food_ma %>%
  mutate(FIPS = sprintf("%05s", as.character(FIPS)))

# Merge data
merged_data <- left_join(
  health_ma,
  Food_ma,
  by = c("CountyFIPS" = "FIPS", "Year" = "Year")
)
nrow(merged_data)
View(merged_data)


# Sorting RuralUrban Continuum codes data set
Rural_urban1 <- read.csv("Ruralurbancontinuumcodes2023.csv")

# Filter for Massachusetts counties
Rural_urban <- Rural_urban1 %>%
  filter(State == "MA", Attribute == "Description")

# remove “County” from the end of Rural_urban county names
library(stringr)

Rural_urban <- Rural_urban %>%
  mutate(
    County_Name = str_to_title(str_trim(County_Name)),   # clean whitespace & capitalization
    County_Name = str_remove(County_Name, " County$")    # remove "County" at the end
  )
View(Rural_urban)

# Merge final data
Rural_urban <- Rural_urban %>%
  mutate(FIPS = sprintf("%05s", as.character(FIPS)))

Outcomes_data <- left_join(
  merged_data,
  Rural_urban,
  by = c("CountyFIPS" = "FIPS", "CountyName" = "County_Name")
)
dim(Outcomes_data)
View(Outcomes_data)
colnames(Outcomes_data)

#Removing redundant variables
Outcomes_data <- Outcomes_data[ ,-c(6,7,8,18,11,13,14,18,19,20,21,22,23,24,26,27,44)]
colnames(Outcomes_data)

# Save cleaned and aggregated data set
write.csv(Outcomes_data, "Outcomes_cleaned.csv", row.names = FALSE)

# == 845 PROJECT WORK ==

# Creating subset of Outcomes_data needed for the project
Outcomes_data1 <- Outcomes_data[ ,-c(3,5,7,9,10,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)]
View(Outcomes_data1)
colnames(Outcomes_data1)

# Rename variables
library(reshape)
Outcomes_data1 = rename(Outcomes_data1, c("Data_Value"="Crude_Prevalence", 
                                          "Value"="Location_Type"))

# Create variable Location_Class from Location_Type values
table(Outcomes_data1$Location_Type)

library(stringr)

Outcomes_data1 <- Outcomes_data1 %>%
  mutate(
    Location_Class = case_when(
      str_starts(Location_Type, "Metro") ~ "Urban",
      str_starts(Location_Type, "Nonmetro") ~ "Nonmetro",
      TRUE ~ NA_character_
    )
  )
#Checking the structure of the data variables
str(Outcomes_data1)

# Convert TotalPopulation to numeric variable
Outcomes_data1$TotalPopulation <- Outcomes_data1$TotalPopulation %>%
  gsub(",", "", .) %>%
  as.numeric()

# Summarize all years, but keep them separate (County × Condition × Year)
MA_summary <- Outcomes_data1 %>%
  filter(!is.na(CountyName) & !is.na(Condition) & !is.na(Year) & !is.na(Location_Class)) %>%
  group_by(CountyName, Condition, Year, Location_Class) %>% 
  summarise(
    mean_crude_prev = mean(Crude_Prevalence, na.rm = TRUE),
    mean_TotalPopulation = mean(TotalPopulation, na.rm = TRUE),
    mean_FoodInsecRate = mean(`Overall.Food.Insecurity.Rate`, na.rm = TRUE),
    mean_FoodInsecPersons = mean(`#.of.Food.Insecure.Persons.Overall`, na.rm = TRUE),
    mean_FoodInsecPersons_Black = mean(`Food.Insecurity.Rate.among.Black.Persons.(all.ethnicities)`, na.rm = TRUE),
    mean_FoodInsecPersons_Hispanic = mean(`Food.Insecurity.Rate.among.Hispanic.Persons.(any.race)`, na.rm = TRUE),
    mean_FoodInsecPersons_White = mean(`Food.Insecurity.Rate.among.White,.non-Hispanic.Persons`, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  )
View(MA_summary)

# Save cleaned and aggregated summary table
write.csv(MA_summary, "MA_summary.csv", row.names = FALSE)

library(gt)

MA_summary %>%
  gt() %>%
  tab_header(
    title = md("**Massachusetts Health Outcomes Summary (2019–2022)**"),
    subtitle = md("Mean prevalence and population measures by County, Condition, and Year")
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center",
    table.width = pct(100)
  ) %>%
  fmt_number(
    columns = c(mean_crude_prev,
                mean_TotalPopulation,
                mean_FoodInsecRate,
                mean_FoodInsecPersons,
                mean_FoodInsecPersons_Black,
                mean_FoodInsecPersons_Hispanic,
                mean_FoodInsecPersons_White),
    decimals = 2
  ) %>%
  fmt_integer(columns = n_records) %>%
  cols_label(
    CountyName = "County",
    Condition = "Condition",
    Year = "Year",
    mean_crude_prev = "Mean Crude Prevalence",
    mean_TotalPopulation = "Mean Total Population",
    mean_FoodInsecRate = "Mean Food Insecurity Rate",
    mean_FoodInsecPersons = "Mean # Food Insecure Persons",
    mean_FoodInsecPersons_Black = "Mean FI Rate (Black)",
    mean_FoodInsecPersons_Hispanic = "Mean FI Rate (Hispanic)",
    mean_FoodInsecPersons_White = "Mean FI Rate (White, non-Hispanic)",
    n_records = "N Records"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f2f2f2"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) 

# DESCRIPTIVE PLOTS

library(ggplot2)

# Grouped bar chart (mean FI by counties grouped within each year)
ggplot(MA_summary, aes(x = Year, y = mean_FoodInsecRate, fill = CountyName)) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean Food Insecurity Rate by County and Year",
    x = "Year",
    y = "Mean Food Insecurity Rate",
    fill = "County"
  ) +
  theme_bw()

# Mean crude prevalence of Diabetes per county per year
# Filter for Diabetes
diabetes_data <- MA_summary %>%
  filter(Condition == "Diabetes")

ggplot(diabetes_data, aes(x = Year, y = mean_crude_prev, color = CountyName, group = CountyName)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Crude Prevalence of Diabetes by County and Year",
    x = "Year",
    y = "Mean Crude Prevalence (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of HBP per county per year
# Filter for High Blood Pressure
HBP_data <- MA_summary %>%
  filter(Condition == "High Blood Pressure")

ggplot(HBP_data, aes(x = Year, y = mean_crude_prev, color = CountyName, group = CountyName)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Crude Prevalence of High Blood Pressure by County and Year",
    x = "Year",
    y = "Mean Crude Prevalence (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean Food Insecurity Rate per county per year
ggplot(MA_summary, aes(x = Year, y = mean_FoodInsecRate, color = CountyName, group = CountyName)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Food Insecurity Rate by County and Year",
    x = "Year",
    y = "Food Insecurity Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of Diabetes vs mean Food Insecurity Rate
ggplot(diabetes_data, aes(x = mean_FoodInsecRate, y = mean_crude_prev, color = CountyName)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Diabetes Prevalence vs Food Insecurity Rate by County",
    x = "Mean Food Insecurity Rate (%)",
    y = "Mean Crude Prevalence of Diabetes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of High Blood Pressure vs mean Food Insecurity Rate
ggplot(HBP_data, aes(x = mean_FoodInsecRate, y = mean_crude_prev, color = CountyName)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "High Blood Pressure Prevalence vs Food Insecurity Rate by County",
    x = "Mean Food Insecurity Rate (%)",
    y = "Mean Crude Prevalence of High Blood Pressure (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of Diabetes vs Obesity
library(tidyr)

# Filter for Diabetes and Obesity and pivot wider
diab_obesity <- MA_summary %>%
  filter(Condition %in% c("Diabetes", "Obesity")) %>%
  select(CountyName, Year, Condition, mean_crude_prev) %>%
  pivot_wider(names_from = Condition, values_from = mean_crude_prev)

ggplot(diab_obesity, aes(x = Obesity, y = Diabetes, color = CountyName)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Diabetes vs Obesity Prevalence by County and Year",
    x = "Mean Crude Prevalence of Obesity (%)",
    y = "Mean Crude Prevalence of Diabetes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of Diabetes vs Leisure Activity
# Filter for Diabetes and Obesity and pivot wider
diab_leisure <- MA_summary %>%
  filter(Condition %in% c("Diabetes", "Leisure Activity")) %>%
  select(CountyName, Year, Condition, mean_crude_prev) %>%
  pivot_wider(names_from = Condition, values_from = mean_crude_prev)

diab_leisure <- dplyr::rename(diab_leisure,
                             Leisure = `Leisure Activity`,
                             )

ggplot(diab_leisure, aes(x = Leisure, y = Diabetes, color = CountyName)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Diabetes vs Leisure Activity Prevalence by County and Year",
    x = "Mean Crude Prevalence of Leisure Activity (%)",
    y = "Mean Crude Prevalence of Diabetes (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of High Blood Pressure vs Obesity
# Filter for High Blood Pressure and Obesity and pivot wider
HBP_obesity <- MA_summary %>%
  filter(Condition %in% c("High Blood Pressure", "Obesity")) %>%
  select(CountyName, Year, Condition, mean_crude_prev) %>%
  pivot_wider(names_from = Condition, values_from = mean_crude_prev)

HBP_obesity <- dplyr::rename(HBP_obesity,
                             HighBP = `High Blood Pressure`)

ggplot(HBP_obesity, aes(x = Obesity, y = HighBP, color = CountyName)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "High Blood Pressure vs Obesity Prevalence by County and Year",
    x = "Mean Crude Prevalence of Obesity (%)",
    y = "Mean Crude Prevalence of High Blood Pressure (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mean crude prevalence of High Blood Pressure vs Leisure Activity
# Filter for High Blood Pressure and Obesity and pivot wider
HBP_leisure <- MA_summary %>%
  filter(Condition %in% c("High Blood Pressure", "Leisure Activity")) %>%
  select(CountyName, Year, Condition, mean_crude_prev) %>%
  pivot_wider(names_from = Condition, values_from = mean_crude_prev)

HBP_leisure <- dplyr::rename(HBP_leisure,
                             Leisure = `Leisure Activity`,
                             HighBP = `High Blood Pressure`)
  
ggplot(HBP_leisure, aes(x = Leisure, y = HighBP, color = CountyName)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "High Blood Pressure vs Leisure Activity Prevalence by County and Year",
    x = "Mean Crude Prevalence of Leisure Activity (%)",
    y = "Mean Crude Prevalence of High Blood Pressure (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# BIVARIATE ANALYSIS

library(broom)

# Pivot outcomes wide (Diabetes and High Blood Pressure)
outcomes_wide <- MA_summary %>%
  filter(Condition %in% c("Diabetes", "High Blood Pressure")) %>%
  select(CountyName, Year, Condition, mean_crude_prev) %>%
  pivot_wider(names_from = Condition, values_from = mean_crude_prev)

# Join predictors (Obesity, Leisure, FoodInsecRate)
predictors <- MA_summary %>%
  filter(Condition %in% c("Obesity", "Leisure Activity")) %>%
  select(CountyName, Year, Condition, mean_crude_prev) %>%
  pivot_wider(names_from = Condition, values_from = mean_crude_prev) %>%
  left_join(MA_summary %>% select(CountyName, Year, mean_FoodInsecRate), 
            by = c("CountyName", "Year"))

data_ttest <- left_join(outcomes_wide, predictors, by = c("CountyName", "Year"))

#Function for t-test
ttest_summary_simple <- function(outcome, predictor, data) {
  data <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[predictor]]))
  
  # Split predictor into high/low based on median
  data$group <- ifelse(data[[predictor]] >= median(data[[predictor]], na.rm = TRUE), "High", "Low")
  
  test <- t.test(data[[outcome]] ~ data$group)
  
  # Return only t-statistic and p-value
  tibble(
    Outcome = outcome,
    Predictor = predictor,
    t_value = test$statistic,
    p_value = test$p.value
  )
}

# Loop over outcome and predictor combinations
outcomes <- c("Diabetes", "High Blood Pressure")
predictors <- c("Obesity", "Leisure Activity", "mean_FoodInsecRate")

# Create the grid
ttest_grid <- expand.grid(Outcome = outcomes, Predictor = predictors, stringsAsFactors = FALSE)

# Run t-tests
ttest_results <- ttest_grid %>%
  rowwise() %>%
  do(ttest_summary_simple(.$Outcome, .$Predictor, data_ttest)) %>%
  ungroup()

View(ttest_results)

# ==REGRESSION MODELS==

# 1. CRUDE LINEAR REGRESSION MODELS FOR EACH CHRONIC DIEASE CONDITION

# Create data from MA_summary
# Join Food Insecurity Rate (predictor) to outcomes_wide
data_lm <- outcomes_wide %>%
  left_join(MA_summary %>% 
              select(CountyName, Year, mean_FoodInsecRate) %>%
              distinct(),  # keep one value per County-Year
            by = c("CountyName", "Year"))

# View data
head(data_lm)

# # Remove rows with any missing values in relevant columns
# data_lm_clean <- data_lm %>%
#   filter(!is.na(Diabetes), 
#          !is.na(`High Blood Pressure`), 
#          !is.na(mean_FoodInsecRate))

# Run crude regression models
lm_diabetes <- lm(Diabetes ~ mean_FoodInsecRate, data = data_lm, na.action = na.omit)
summary(lm_diabetes)
tidy(lm_diabetes)

lm_HBP <- lm(`High Blood Pressure` ~ mean_FoodInsecRate, data = data_lm, na.action = na.omit)
summary(lm_HBP)
tidy(lm_HBP)

# 2. ADJUSTED LM MODELS WITH LEISURE SCTIVITY AND OBESITY AS CO-VARIATES
# # Create data from MA_summary
# Pivot outcomes wide
data_lm_adj <- MA_summary %>%
  filter(Condition %in% c("Diabetes", "High Blood Pressure", "Obesity", "Leisure Activity")) %>%
  select(CountyName, Year, Location_Class, Condition, mean_crude_prev, mean_FoodInsecRate) %>%
  pivot_wider(
    names_from = Condition,
    values_from = mean_crude_prev
  ) %>%
  filter(
    !is.na(Diabetes),
    !is.na(`High Blood Pressure`),
    !is.na(mean_FoodInsecRate),
    !is.na(Obesity),
    !is.na(`Leisure Activity`)
  )
View(data_lm_adj)
# Adjusted model for Diabetes
lm_diabetes_adj <- lm(Diabetes ~ mean_FoodInsecRate + Obesity + `Leisure Activity`, data = data_lm_adj)
summary(lm_diabetes_adj)
tidy(lm_diabetes_adj)

# Adjusted model for High Blood Pressure
lm_HBP_adj <- lm(`High Blood Pressure` ~ mean_FoodInsecRate + Obesity + `Leisure Activity`, data = data_lm_adj)
summary(lm_HBP_adj)
tidy(lm_HBP_adj)

# Add interaction term to the Diabetes model

data_lm_adj$Location_Class <- factor(data_lm_adj$Location_Class)

lm_diabetes_interaction <- lm(
  Diabetes ~ mean_FoodInsecRate * Location_Class + 
    Obesity + `Leisure Activity`,
  data = data_lm_adj
)

summary(lm_diabetes_interaction)
# Add interaction term to the Hypertension model
lm_HBP_interaction <- lm(
  `High Blood Pressure` ~ mean_FoodInsecRate * Location_Class + 
    Obesity + `Leisure Activity`,
  data = data_lm_adj
)

summary(lm_HBP_interaction)
# #==RANDOM FOREST REGRESSION==
# library(randomForest)
# 
# # Select relevant columns and ensure there are no missing values
# rf_data <- data_lm_adj %>%
#   select(Diabetes, `High Blood Pressure`, mean_FoodInsecRate, Obesity, `Leisure Activity`) %>%
#   dplyr::rename(Leisure = `Leisure Activity`,
#                 HBP = `High Blood Pressure`) %>%
#   na.omit()
# 
# # Random Forest for Diabetes
# set.seed(123)
# 
# # Random Forest for Diabetes
# rf_diabetes <- randomForest(
#   Diabetes ~ mean_FoodInsecRate + Obesity + Leisure,
#   data = rf_data,
#   ntree = 500,
#   importance = TRUE
# )
# 
# print(rf_diabetes)
# importance(rf_diabetes)  
# 
# #Random Forest for High Blood Pressure
# 
# set.seed(123)
# 
# rf_HBP <- randomForest(
#   HBP ~ mean_FoodInsecRate + Obesity + Leisure,
#   data = rf_data,
#   ntree = 500,
#   importance = TRUE
# )
# 
# print(rf_HBP)
# importance(rf_HBP)

# ==COMPARING LINEAR REGRESSION MODELS WITH ML REGRESSION MODELS==
detach("package:reshape", unload = TRUE)

data_reg <- MA_summary %>%
  select(CountyName, Year, Location_Class, Condition, mean_crude_prev, mean_FoodInsecRate) %>%
  filter(Condition %in% c("Diabetes", "High Blood Pressure", "Obesity", "Leisure Activity")) %>%
  pivot_wider(
    names_from = Condition,
    values_from = mean_crude_prev
  ) %>%
  rename(
    Leisure = `Leisure Activity`,
    HBP = `High Blood Pressure`
  ) %>%
  select(Diabetes, HBP, mean_FoodInsecRate, Obesity, Leisure, Location_Class) %>%
  na.omit()

#1.  SPLIT DATA INTO TRAINING AND TEST DATA SETS
library(caret)

# For diabetes
set.seed(123)

# Create a partition index
sampling_index_d <- createDataPartition(
  y = data_reg$Diabetes,  
  times = 1,
  p = 0.7,
  list = FALSE
)

# Split the dataset
train_data <- data_reg[sampling_index_d, ]
test_data <- data_reg[-sampling_index_d, ]


# For HBP
set.seed(123)

# Create a partition index
sampling_index_h <- createDataPartition(
  y = data_reg$HBP,  # or another continuous variable
  times = 1,
  p = 0.7,
  list = FALSE
)

# Split the dataset
train_data <- data_reg[sampling_index_h, ]
test_data  <- data_reg[-sampling_index_h, ]

# 2. Train and tune each model using k-fold cross-validation on the training set
cv_5 <- trainControl(
  method = "cv",        # k-fold cross-validation
  number = 5,          # 10 folds
  savePredictions = "final"
)

# REGRESSION MODELS
# Linear Regression with 5-Fold CV
train_data <- train_data %>% 
  mutate_if(is.factor, as.numeric) %>%  # convert any factors to numeric
  na.omit()

#For Diabetes
lm_model <- train(
  Diabetes ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "lm",
  trControl = cv_5,
  metric = "RMSE"
)

lm_model

# For HBP
lm_model2 <- train(
  HBP ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "lm",
  trControl = cv_5,
  metric = "RMSE"
)

lm_model2

# Random Forest Regression with 5-Fold CV
# For Diabetes
set.seed(123)

rf_model <- train(
  Diabetes ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "rf",
  trControl = cv_5,
  tuneLength = 5,
  metric = "RMSE",
  importance = TRUE
)

rf_model

# For HBP
set.seed(123)

rf_model2 <- train(
  HBP ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "rf",
  trControl = cv_5,
  tuneLength = 5,
  metric = "RMSE",
  importance = TRUE
)

rf_model2

# Elastic Net, Lasso, and Ridge with 5-fold CV
# Remove any NAs just in case
train_data <- train_data %>% na.omit()
test_data  <- test_data %>% na.omit()

# Elastic Net Model
#For Diabetes
set.seed(123)
enet_model <- train(
  Diabetes ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "glmnet",
  trControl = cv_5,
  tuneLength = 10,  # caret will try 10 combinations of alpha & lambda
  metric = "RMSE"
)

enet_model
enet_model$bestTune # shows the best alpha and lambda.
enet_model$results #shows RMSE for all tuning combinations.

# Evaluate on test set
preds_enet <- predict(enet_model, test_data)
postResample(preds_enet, test_data$Diabetes)  # RMSE, R-squared, MAE

# Variable Importance
varImp(enet_model)

# For HBP
set.seed(123)
enet_model2 <- train(
  HBP ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "glmnet",
  trControl = cv_5,
  tuneLength = 10,  # caret will try 10 combinations of alpha & lambda
  metric = "RMSE"
)

enet_model2

enet_model2$bestTune # shows the best alpha and lambda.
enet_model2$results #shows RMSE for all tuning combinations.

# Evaluate on test set
preds_enet2 <- predict(enet_model2, test_data)
postResample(preds_enet2, test_data$HBP)  # RMSE, R-squared, MAE

# Variable Importance
varImp(enet_model2)

# LASSO MODEL (alpha = 1)
# For Diabetes
set.seed(123)
lasso_model <- train(
  Diabetes ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "glmnet",
  trControl = cv_5,
  tuneGrid = expand.grid(
    alpha = 1,                       # Lasso
    lambda = seq(0.001, 0.1, length = 10)
  ),
  metric = "RMSE"
)

lasso_model

# Evaluate on test set
preds_lasso <- predict(lasso_model, test_data)
postResample(preds_lasso, test_data$Diabetes)

# Variable Importance
varImp(lasso_model)

# For HBP
set.seed(123)
lasso_model2 <- train(
  HBP ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "glmnet",
  trControl = cv_5,
  tuneGrid = expand.grid(
    alpha = 1,                       # Lasso
    lambda = seq(0.001, 0.1, length = 10)
  ),
  metric = "RMSE"
)

lasso_model2

# Evaluate on test set
preds_lasso2 <- predict(lasso_model2, test_data)
postResample(preds_lasso2, test_data$HBP)

# Variable Importance
varImp(lasso_model2)

# RIDGE MODEL (alpha = 0)
set.seed(123)
ridge_model <- train(
  Diabetes ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "glmnet",
  trControl = cv_5,
  tuneGrid = expand.grid(
    alpha = 0,                       # Ridge
    lambda = seq(0.001, 0.1, length = 10)
  ),
  metric = "RMSE"
)

ridge_model

# Evaluate test set
preds_ridge <- predict(ridge_model, test_data)
postResample(preds_ridge, test_data$Diabetes)

# Variable Importance
varImp(ridge_model)

# For HBP
set.seed(123)
ridge_model2 <- train(
  HBP ~ mean_FoodInsecRate + Obesity + Leisure,
  data = train_data,
  method = "glmnet",
  trControl = cv_5,
  tuneGrid = expand.grid(
    alpha = 0,                       # Ridge
    lambda = seq(0.001, 0.1, length = 10)
  ),
  metric = "RMSE"
)

ridge_model2

# Evaluate test set
preds_ridge2 <- predict(ridge_model2, test_data)
postResample(preds_ridge2, test_data$HBP)

# Variable Importance
varImp(ridge_model2)

# RMSE comparison plot

# Order models consistently
model_results$Model <- factor(
  model_results$Model,
  levels = c("Linear Regression", "Ridge", "Lasso", "Elastic Net", "Random Forest")
)

ggplot(model_results, aes(x = Model, y = RMSE, fill = Outcome)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(RMSE, 3)),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Model Comparison Based on RMSE",
    x = "Model Type",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylim(0, max(model_results$RMSE) * 1.15)

