install.packages("ISLR2")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
library(ggplot2)
library(randomForest)
library(tidyverse)
library(ISLR2)
library(dplyr)
library(corrplot)
library(MASS)




?College
glimpse(College)


# Sumarizați numărul de valori lipsă pentru fiecare coloană
sum_na <- sapply(College, function(x) sum(is.na(x)))
print(sum_na)
summary(College)
str(College)
hist(College$Apps, main = "Histogram for Applications", xlab = "Applications")
barplot(table(College$Private), main = "Bar Plot for Private", xlab = "Private", ylab = "Frequency")

# Verificați dacă există universități cu rata de absolvire mai mare de 100%
over_100_grad_rate <- subset(College, Grad.Rate > 100)

# Afișați rezultatele
print(over_100_grad_rate)

# Numărul de cazuri cu rata de absolvire peste 100%
num_over_100 <- nrow(over_100_grad_rate)
cat("Numărul de universități cu rata de absolvire peste 100%:", num_over_100, "\n")

# Eliminați înregistrările cu o rată de absolvire mai mare de 100%
College <- College[College$Grad.Rate <= 100, ]


# Selectați doar coloanele numerice
numeric_data <- College[, sapply(College, is.numeric)]

# Calculați corelațiile dintre Grad.Rate și alte variabile numerice
correlations <- cor(numeric_data)

# Afișați corelațiile pentru Grad.Rate
grad_rate_correlations <- correlations["Grad.Rate", ]
print(grad_rate_correlations)

# Pentru a testa semnificația statistică a fiecărei corelații:
for (col in names(numeric_data)) {
  if (col != "Grad.Rate") {
    test_result <- cor.test(numeric_data[["Grad.Rate"]], numeric_data[[col]], 
                            use = "complete.obs")
    cat("Corelația dintre Grad.Rate și", col, ":\n")
    print(test_result)
    cat("\n")
  }
}


ggplot(College, aes(x = Accept, y = Grad.Rate)) + 
  geom_point() +
  labs(title = "Relația dintre Acceptări și Rata de Absolvire",
       x = "Numărul de Acceptări",
       y = "Rata de Absolvire (%)") +
  theme_minimal()



ggplot(College, aes(x = Outstate, y = Grad.Rate)) + 
  geom_density_2d_filled() +  # Adaugă densitatea 2D
  geom_point() +  # Adaugă puncte
  labs(title = "Densitatea 2D între Taxa Outstate și Rata de Absolvire",
       x = "Taxa de Școlarizare Out-of-State",
       y = "Rata de Absolvire (%)") +
  theme_minimal()



train_index5 <- sample(nrow(College), nrow(College)*0.7)
train_data5 <- College[train_index5,]
test_data5 <- College[-train_index5 ,]
rf_model <- randomForest(Accept ~ Outstate + Top10perc + perc.alumni, data = train_data5, importance = TRUE, ntree = 500)
predictions <- predict(rf_model, test_data5)
# feature importance
importance(rf_model)

linear_model <- lm(Grad.Rate ~ Accept +   Outstate + perc.alumni, data = College)
summary(linear_model)

ggplot(College, aes(x = Accept, y = Grad.Rate)) + 
  geom_point() +  # Adaugă punctele de dispersie
  geom_smooth(method = "lm", col = "blue") +  # Adaugă linia de regresie
  labs(title = "Regresia Liniară a Grad.Rate pe Accept",
       x = "Accept (Număr de Acceptări)",
       y = "Grad.Rate (Rata de Absolvire)")


ggplot(College, aes(x = Outstate, y = Grad.Rate)) + 
  geom_point() +  # Adaugă punctele de dispersie
  geom_smooth(method = "lm", col = "green") +  # Adaugă linia de regresie
  labs(title = "Regresia Liniară a Ratei de Absolvire pe Outstate",
       x = "Outstate",
       y = "Grad.Rate (Rata de Absolvire)")



linear_model_summary <- summary(linear_model)
r_squared <- linear_model_summary$r.squared

# Afișați R-squared
print(r_squared)



