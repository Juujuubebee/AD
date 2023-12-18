install.packages('tidyverse')
install.packages('openintro')
install.packages('corrplot')
install.packages('rsample')

# Importing libraries
library(tidyverse)
library(openintro)
library(readr)
library(dplyr)
library(knitr)

library (corrplot)


library(rsample)
### Prerequisites ###
library(dplyr)
library(ggplot2)

# modeling packages
library(caret)

# model interpretability packages
library(vip)


#Exploratory Data Analysis

# Importing CSV-file
score <- read.csv("C:\\Users\\User\\Desktop\\AD\\Student_score.csv")



#Meeting Data Types
glimpse(score)

# Changing Data Types from character to factor
score <- data.frame(lapply(score, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))

glimpse(score)

# Assuming 'score' is your dataset
data_types <- sapply(score, class)
print(data_types)


# Вывести типы данных в виде красивой таблицы
kable(as.data.frame(sapply(score, class)), col.names = c("Column Name", "Data Type"), caption = "Data Types in 'score'")

# Подсчет количества нулевых (пропущенных) значений в каждом столбце
null_counts <- score %>%
  summarise_all(~ sum(is.na(.)))

# Преобразование результатов в удобный формат
null_counts_df <- as.data.frame(t(null_counts))

print(null_counts_df)

# Assuming 'train' is your dataset
unique_counts_df <- as.data.frame(t(unique_counts))
print(unique_counts_df)


Gender_levels <- levels(score$Gender)
print(Gender_levels)

EthnicGroup_levels <- levels(score$EthnicGroup)

print(EthnicGroup_levels)

ParentEduc_levels <- levels(score$ParentEduc)
print(ParentEduc_levels)

LunchType_levels <- levels(score$LunchType)
print(LunchType_levels)


print("Пропущенные значения в каждом столбце:")
print(colSums(is.na(score)))

# Assuming 'data' is your dataframe
outcome_counts <- score %>%
  group_by(Gender) %>%
  summarise(count = n())

# Create the pie chart with percentage labels
ggplot(outcome_counts, aes(x = "", y = count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Gender Cases",
       fill = "Gender") +
  theme_void() +
  geom_text(aes(label = scales::percent(count / sum(count))), position = position_stack(vjust = 0.5))


ggplot(score, aes(x = ParentEduc, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Parent Education vs. Gender", x = "Parent Education", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(score, aes(x = EthnicGroup, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "EthnicGroup vs. Gender", x = "EthnicGroup", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(score, aes(x = MathScore, group = Gender, color = Gender)) +
  geom_line(stat = "count") +
  labs(title = "Line Plot of MathScore by Gender",
       x = "MathScore",
       y = "Count") +
  theme_minimal()+
  xlim(0, 101)

# Создаем Line Plot с жирными линиями
ggplot(score, aes(x = MathScore, group = Gender, color = Gender)) +
  geom_line(stat = "count", size = 2) +  # Установка параметра size
  labs(title = "Line Plot of MathScore by Gender",
       x = "MathScore",
       y = "Count") +
  theme_minimal()

ggplot(score, aes(x = ReadingScore, group = Gender, color = Gender)) +
  geom_line(stat = "count") +
  labs(title = "Line Plot of ReadingScore by Gender",
       x = "ReadingScore",
       y = "Count") +
  theme_minimal()+
  xlim(0, 101)


# Создаем Line Plot с жирными линиями
ggplot(score, aes(x = ReadingScore, group = Gender, color = Gender)) +
  geom_line(stat = "count", size = 2) +  # Установка параметра size
  labs(title = "Line Plot of ReadingScore by Gender",
       x = "ReadingScore",
       y = "Count") +
  theme_minimal()


ggplot(score, aes(x = WritingScore, group = Gender, color = Gender)) +
  geom_line(stat = "count") +
  labs(title = "Line Plot of WritingScore by Gender",
       x = "WritingScore",
       y = "Count") +
  theme_minimal()+
  xlim(0, 101)


# Создаем Line Plot с жирными линиями
ggplot(score, aes(x = WritingScore, group = Gender, color = Gender)) +
  geom_line(stat = "count", size = 2) +  # Установка параметра size
  labs(title = "Line Plot of WritingScore by Gender",
       x = "WritingScore",
       y = "Count") +
  theme_minimal()

# Создаем Bubble Chart
ggplot(score, aes(x = MathScore, y = ReadingScore, size = WritingScore, color = WritingScore)) +
  geom_point(alpha = 0.7) +  # Прозрачность точек
  labs(title = "Bubble Chart of Scores",
       x = "MathScore",
       y = "ReadingScore",
       size = "WritingScore",
       color = "WritingScore") +
  theme_minimal()


install.packages('gapminder')
install.packages("hrbrthemes")
install.packages("viridis")

# Подключаем библиотеки
library(gapminder)
library(hrbrthemes)

# Создаем Bubble Chart с тремя признаками
ggplot(score, aes(x = MathScore, y = ReadingScore, size = WritingScore)) +
  geom_point(aes(label = Name), alpha = 0.7) +
  scale_size(range = c(1.4, 19), name = "WritingScore") +
  scale_color_manual(values = c("red", "blue", "green")) +  # Выберите нужные цвета
  labs(title = "Bubble Chart of Scores",
       x = "MathScore",
       y = "ReadingScore",
       size = "WritingScore") +
  geom_text_repel(aes(label = Name), size = 3, box.padding = 0.5) +  # Добавляем текст (имена) с использованием ggrepel
  theme_minimal()



# Создание блоков с усами (box plots)
plot1 <- ggplot(score, aes(x = Gender, y = MathScore, fill = Gender)) +
  geom_boxplot() +
  labs(title = "MathScore by Gender", x = "Gender", y = "MathScore") +
  theme_minimal()

plot2 <- ggplot(score, aes(x = Gender, y = ReadingScore, fill = Gender)) +
  geom_boxplot() +
  labs(title = "ReadingScore by Gender", x = "Gender", y = "ReadingScore") +
  theme_minimal()

plot3 <- ggplot(score, aes(x = Gender, y = WritingScore, fill = Gender)) +
  geom_boxplot() +
  labs(title = "WritingScore by Gender", x = "Gender", y = "WritingScore") +
  theme_minimal()

# Отображение графиков
print(plot1)
print(plot2)
print(plot3)



ggplot(score, aes(x = TestPrep, fill = LunchType)) +
  geom_bar(position = "dodge") +
  labs(title = "Test Preposition vs. LunchType", x = "Test Preposition", y = "LunchType") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(score, aes(x = TestPrep, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Test Preposition vs. Gender", x = "Test Preposition", y = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Групповая ящичная диаграмма
ggplot(score, aes(x = TestPrep, y = WritingScore, fill = Gender)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Test Preposition vs. Gender", x = "Test Preposition", y = "Writing Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



# Пример данных
score_correlation <- cor(score[c("MathScore", "ReadingScore", "WritingScore")])

# Создание тепловой карты корреляции с точными значениями
heatmap_plot <- ggplot() +
  geom_tile(data = data.frame(x = colnames(score_correlation), y = rownames(score_correlation), z = as.vector(score_correlation)),
            aes(x, y, fill = z)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  geom_text(data = data.frame(x = rep(colnames(score_correlation), each = ncol(score_correlation)),
                              y = rep(rownames(score_correlation), ncol(score_correlation)),
                              z = as.vector(score_correlation)),
            aes(x, y, label = round(z, 2)), vjust = 1, size = 3, color = "black")

# Отображение графика
print(heatmap_plot)


glimpse(score)

# Подсчитываем количество значений, равных нулю, в каждом столбце
zero_counts <- colSums(score == 0, na.rm = TRUE)

# Выводим результат
print(zero_counts)

# Удаляем столбец 'X'
score <- subset(score, select = -X)

glimpse(score)

install.packages("dummy")

library(dummy)

perform_one_hot_encoding <- function(df, column_name) { 
  dummies <- dummy_cols(df[, column_name], sep = "_")
  
  # Объединяем исходный датафрейм и новые dummy-столбцы
  df <- bind_cols(df, dummies)
  
  # Удаляем исходный столбец
  df <- select(df, -column_name)
  
  return(df)
}

# Создаем копии данных для каждого набора столбцов
data <- score

# Выполняем one-hot encoding для каждого набора столбцов
for (column in c('Gender', 'EthnicGroup', 'ParentEduc', 'LunchType','TestPrep')) {
  data <- perform_one_hot_encoding(data, column)
}


# Выводим результаты
print(data)
glimpse(data)



# Установка необходимых пакетов
install.packages("randomForest")
install.packages("e1071")
# Загрузка библиотек
library(randomForest)
library(e1071)
# modeling packages
library(caret)
library(rsample)

glimpse(score)
df <- data.frame(score)
glimpse(df)


dim(df)
df <- subset(df, select = -ReadingScore)
#df <- subset(df, select = -WritingScore)
glimpse(df)

dim(df)
# Create training (70%) and test (30%) sets
df_split <- initial_split(df, prop = .7, strata = 'MathScore')
df_train <- training(df_split)
df_test <- testing(df_split)


nrow(df_train)
nrow(df_test)

# Посмотреть структуру данных
glimpse(df_train)

# Создадим модель линейной регрессии
linear_model <- lm(MathScore ~ ., data = df)

# Выведем сводку по модели
summary(linear_model)


# Рассчитаем метрики
predictions <- predict(linear_model, newdata = df_test)
residuals <- residuals(linear_model)
rmse <- sqrt(mean(residuals^2))
r_squared <- summary(linear_model)$r.squared


# Визуализация важности переменных
vip(linear_model, num_features = 10,fill = "blue", bar = TRUE)
    
# Выведем метрики
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Создаем dataframe с данными
df_results <- data.frame(Actual = df$MathScore, Predicted = predictions)

# Создаем dataframe с данными из тестового набора
df_results <- data.frame(Actual = df_test$MathScore, Predicted = predictions)


ggplot(df_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "solid",  linetype = "dashed", size = 1.5) +
  labs(title = "Scatter Plot: Actual vs Predicted MathScore",
       x = "Actual MathScore", y = "Predicted MathScore")





install.packages("pROC")

library(randomForest)
library(e1071)
library(pROC)



# Обучение модели Random Forest
rf_model <- randomForest(MathScore ~ ., data = df_train)

# Предсказание на тестовом наборе
predictions_rf <- predict(rf_model, newdata = df_test)

# Добавление предсказанных значений в dataframe
df_results_rf$Predicted <- predictions_rf

# График фактических и предсказанных значений
plot(df_results_rf$Actual, df_results_rf$Predicted, 
     main = "Random Forest: Actual vs Predicted MathScore",
     xlab = "Actual MathScore", ylab = "Predicted MathScore", pch = 16, col = "blue")

# Добавление линии идентичности
abline(a = 0, b = 1, col = "red")


# Установка и загрузка необходимых пакетов
install.packages("Metrics")
library(Metrics)



# Метрики для Random Forest
rf_r_squared <- cor(df_results$Actual, df_results$Predicted)^2
rf_mae <- mae(df_results$Actual, df_results$Predicted)
rf_rmse <- rmse(df_results$Actual, df_results$Predicted)

cat("Random Forest Metrics:\n")
cat("R-squared:", rf_r_squared, "\n")
cat("MAE:", rf_mae, "\n")
cat("RMSE:", rf_rmse, "\n")


# Создаем dataframe с данными из тестового набора
df_results_svm <- data.frame(Actual = df_test$MathScore)

# Обучение модели SVM
svm_model <- svm(MathScore ~ ., data = df_train)

# Предсказание на тестовом наборе
predictions_svm <- predict(svm_model, newdata = df_test)

# Добавление предсказанных значений в dataframe
df_results_svm$Predicted <- predictions_svm

# График фактических и предсказанных значений
plot(df_results_svm$Actual, df_results_svm$Predicted, 
     main = "SVM: Actual vs Predicted MathScore",
     xlab = "Actual MathScore", ylab = "Predicted MathScore", pch = 16, col = "blue")

# Добавление линии идентичности
abline(a = 0, b = 1, col = "red")


# Метрики для SVM
svm_r_squared <- cor(df_results_svm$Actual, df_results_svm$Predicted)^2
svm_mae <- mae(df_results_svm$Actual, df_results_svm$Predicted)
svm_rmse <- rmse(df_results_svm$Actual, df_results_svm$Predicted)

cat("\nSVM Metrics:\n")
cat("R-squared:", svm_r_squared, "\n")
cat("MAE:", svm_mae, "\n")
cat("RMSE:", svm_rmse, "\n")

# Дополнительная метрика для SVM (AUC-ROC)
if (length(levels(as.factor(df_results_svm$Actual))) == 2) {
  svm_auc_roc <- roc(df_results_svm$Actual, df_results_svm$Predicted)
  cat("AUC-ROC (SVM):", auc(svm_auc_roc), "\n")
}



# Создаем dataframe с предсказанными значениями и остатками
residual_df <- data.frame(Predicted = predictions_svm, Residuals = residuals(svm_model))

# График остатков с использованием ggplot
ggplot(residual_df, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "green", shape = 16) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()






# Random Forest
model_rf <- randomForest(MathScore ~ ., data = df_train)
predicted_rf <- predict(model_rf, df_test)

# Создать Confusion Matrix для Random Forest
conf_matrix_rf <- table(predicted_rf, df_test$LeaveOrNot)

# Вывести Confusion Matrix
print(conf_matrix_rf)

# Вычислить Accuracy для Random Forest
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
cat("Random Forest Accuracy:", accuracy_rf, "\n")

# Support Vector Machine (SVM)
model_svm <- svm(LeaveOrNot ~ ., data = df_train)
predicted_svm <- predict(model_svm, df_test)

# Создать Confusion Matrix для SVM
conf_matrix_svm <- table(predicted_svm, df_test$LeaveOrNot)


# Вывести Confusion Matrix
print(conf_matrix_svm)

# Вычислить Accuracy для SVM
accuracy_svm <- sum(diag(conf_matrix_svm)) / sum(conf_matrix_svm)
cat("SVM Accuracy:", accuracy_svm, "\n")









