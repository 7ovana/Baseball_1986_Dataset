library(tidyverse)
library(plotly)
library(lubridate)
library(heatmaply)
library(ggcorrplot)
library(ggbiplot)

# Colour palette
mypalette <- c('#d32d54', '#34dbca', '#f5ae1d', '#7b60f1', '#f3842f', '#4fafe8', '#dd86d8',  '#76df3a' )

# Loading raw data
data <-read.csv(file="Baseball.csv",sep=";")
head(data)
summary(data)
dim(data)

# Target
data$Salary_1987 %>% is.na %>% sum
# Creating subset of samples without salary entry
pred_set <- subset(data, is.na(data$Salary_1987))
dim(pred_set)

data <- subset(data, !is.na(data$Salary_1987))
head(data)
dim(data)

# Premier plot des données
fig <- plot_ly(data,
               x = ~Name, 
               y = ~Salary_1987, 
               type = 'scatter',
               marker = list(color = mypalette[1])) %>% 
  layout(xaxis = list(rangeslider_visible=T))
fig

# Histogram of target
fig <- plot_ly(data,
               x = ~Salary_1987, 
               type = 'histogram',
               marker = list(color = mypalette[2]),
               nbinsx = 100) %>% 
  layout(bargap = 0.1, title = "Salary")

fig

# Histogram log(salary)
log_salary <- log(data$Salary_1987)

fig <- plot_ly(x = log_salary, 
               type = 'histogram',
               marker = list(color = mypalette[2]),
               nbinsx = 50) %>% 
  layout(bargap = 0.1, title = "Log(Salary)")

fig

# Boxplot of salary
fig <- plot_ly(data, y = ~Salary_1987, 
               type = 'box',
               marker = list(color = mypalette[1]),
               name = "Salary")
fig

summary(data$Salary_1987)
summary(log_salary)
# We log everything to stabilize variance
var(log_salary)
var(data$Salary_1987)

# Boxplot of salary
fig <- plot_ly(y = log_salary, 
               type = 'box',
               marker = list(color = mypalette[1]),
               name = "Log(salary)")
fig

# Runs 1986 qqnorm
qqnorm(sqrt(data$Runs_1986))
qqline(sqrt(data$Runs_1986))

# log(Salary) qqnorm
qqnorm(log_salary)
qqline(log_salary)

# Log salary not linear with years of career
fig <- plot_ly(data, y = log_salary,
               x = ~Longevity,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Log(salary) in function of years of career") %>% 
  layout(yaxis = list(name = "log(salary)"))
fig

# Target and features separation

y <- log(data$Salary_1987)
X <- select(data, -c(1,22,23,24))
head(X)
dim(X)

# Hits_career vs. Bat_times_career
fig1 <- plot_ly(data, y = ~Bat_times_career,
               x = ~Hits_career,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Hits_career in function of Bat_times_career") %>% 
  layout(yaxis = list(name = "Hits_career"))

fig2 <- plot_ly(data, y = ~Bat_times_86,
               x = ~Hits_86,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Hits_86 in function of Bat_times_86") %>% 
  layout(yaxis = list(name = "Hits_86"))

fig <- subplot(fig1, fig2)
fig

# On utilise que Hits_career et Hits_86
X = subset(X, select = -c(Bat_times_career, Bat_times_86))

# Transforming years by criteria in article
years_less7 <- data$Longevity
years_less7
years_less7[years_less7 <= 7] = years_less7[years_less7 <=7] - 2
years_less7[years_less7 <=0] = 0
years_less7[years_less7 > 7] = 5
years_less7

years_more7 <- data$Longevity
years_more7[years_more7 >= 7] = years_more7[years_more7 >= 7] - 7
years_more7[years_more7 < 7] = 0
years_more7


# Encode categorical variables
X$League_1986 <- factor(data$League_1986, 
                          levels = data$League_1986 %>% unique %>% levels, 
                          labels = seq(1, data$League_1986 %>% unique %>% length, 1)) %>% as.numeric
X$Division_1986 <- factor(data$Division_1986, 
                          levels = data$Division_1986 %>% unique %>% levels, 
                          labels = seq(1, data$Division_1986 %>% unique %>% length, 1)) %>% as.numeric
X$Team_1986 <-  factor(data$Team_1986, 
                       levels = data$Team_1986 %>% unique %>% levels, 
                       labels = seq(1, data$Team_1986 %>% unique %>% length, 1)) %>% as.numeric
X$Position_1986 <- factor(data$Position_1986,
                          levels = data$Position_1986 %>% unique,
                          labels = seq(1, data$Position_1986 %>% unique %>% length,1)) %>% as.numeric
head(X)


X_with_target <- X
X_with_target['logSalary_1987'] = y

# Correlation entre les features + target

# Compute a correlation matrix
corr <- round(cor(X_with_target), 2)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(X_with_target)
# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
ggplotly(corr.plot)

dim(X_with_target)

# Hits_career, Bat_times_career faut en enlever une

# Total runs produced
TRP_career <- (X$Runs_career + X$Runs_batted_career - X$Home_runs_career) / X$Longevity
TRP_1986 <- (X$Runs_1986 + X$Runs_batted_1986 - X$Home_runs_1986)

# on se débarasse de ça aussi
X = subset(X, select = -c(Runs_career, Runs_batted_career, Home_runs_career, Runs_1986, Runs_batted_1986, Home_runs_1986))
X['TRP_career'] = TRP_career
X['TRP_1986'] = TRP_1986

dim(X)

X_with_target <- X
X_with_target['logSalary_1987'] = y

# Correlation entre les features + target

# Compute a correlation matrix
corr <- round(cor(X_with_target), 2)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(X_with_target)
# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
ggplotly(corr.plot)

# TRP_1986 vs. Hits_1986
fig1 <- plot_ly(data, y = ~Hits_86,
                x = ~TRP_1986,
                type = 'scatter',
                marker = list(color = mypalette[1]),
                name = "Hits_86 in function of TRP_1986") %>% 
  layout(yaxis = list(name = "TRP_1986")) 
fig1

# Longevity vs. Hits_career
fig1 <- plot_ly(X, y = ~Hits_career,
                x = ~Longevity,
                type = 'scatter',
                marker = list(color = mypalette[1]),
                name = "Hits_career in function of Longevity") %>% 
  layout(yaxis = list(name = "Longevity"))
fig1

cor(X$Hits_career, X$Longevity)


## PCA

# Remove variable categorielle
X_nocat <- subset(X, select = -c(League_1986, Division_1986, Team_1986, Position_1986))
head(X_nocat)
X.pca <- prcomp(X_nocat, center = TRUE,scale. = TRUE)
summary(X.pca)
ggplotly(ggbiplot(X.pca))

# eigenvectors
X.pca$rotation
dim(X_nocat)

y <- log(data$Salary_1987)
#y <- data$Salary_1987
head(X_nocat)

ggplotly(ggbiplot(X.pca, choice = c(3,4)))


## Regression multiple linéaire
model <- lm(log(Salary_1987) ~ Hits_86 + Walks_1986 + Longevity + Hits_career + Walks_career + Put_outs_1986 + Assists_1986 + Errors_1986 + TRP_career + TRP_1986, data)
model
summary(model)

y_train_pred <- predict(mod, X_nocat)

fig <- plot_ly(y = y_train_pred - y,
               x = y_train_pred,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Log(salary) in function of years of career") %>% 
  layout(yaxis = list(name = "log(salary)"))
fig

split_dummy <- sample(c(rep(0, 0.7 * nrow(data)),  # Create dummy for splitting
                        rep(1, 0.3 * nrow(data))))

#X$Longevity <- (X$Longevity)^2

X_train <- X_nocat[split_dummy == 0, ]
y_train <- y[split_dummy == 0]
X_test <- X_nocat[split_dummy == 1, ]
y_test <- y[split_dummy == 1]


mod <- lm(y_train ~ ., X_train)
summary(mod)

dim(X_test)
dim(X_train)

y_pred <- predict(mod, X_test)
hist(y_test - y_pred, breaks = 30)

exp(y_pred)
exp(y_test)

results <- data.frame(real_values = exp(y_test), predictions = exp(y_pred))
results

fig <- plot_ly(y = y_pred,
               x = y_test,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Log(salary) in function of years of career") %>% 
  layout(yaxis = list(name = "log(salary)"))
fig

rmse_test <- sqrt(mean((exp(y_test) - exp(y_pred))^2))
rmse_test
max(exp(y_test))
min(exp(y_test))
results

fig <- plot_ly(y = y_test - y_pred,
               x = y_pred,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Log(salary) in function of years of career") %>% 
  layout(yaxis = list(name = "log(salary)"))
fig


## On all the data

head(X)
dim(X)

X_train <- X[split_dummy == 0, ]
y_train <- y[split_dummy == 0]
X_test <- X[split_dummy == 1, ]
y_test <- y[split_dummy == 1]


mod <- lm(y_train ~ ., X_train)
summary(mod)

dim(X_test)
dim(X_train)

y_pred <- predict(mod, X_test)
rmse_test <- sqrt(mean((exp(y_test) - exp(y_pred))^2))
rmse_test

results_log <- data.frame(real_values = y_test, predictions = y_pred)
results_log

fig <- plot_ly(y = y_test - y_pred,
               x = y_pred,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Log(salary) in function of years of career") %>% 
  layout(yaxis = list(name = "log(salary)"))
fig


# Three predictors
X_3 <- subset(X, select = c(Longevity, TRP_career))
X_3['years_sq'] = X$Longevity^2

X_train <- X_3[split_dummy == 0, ]
y_train <- y[split_dummy == 0]
X_test <- X_3[split_dummy == 1, ]
y_test <- y[split_dummy == 1]

mod <- lm(y_train ~ ., X_train)
summary(mod)
y_pred <- predict(mod, X_test)
rmse_test <- sqrt(mean((exp(y_test) - exp(y_pred))^2))
rmse_test

## OUTLIERS DETECTED
fig <- plot_ly(y = y_test - y_pred,
               x = y_pred,
               type = 'scatter',
               marker = list(color = mypalette[1]),
               name = "Log(salary) in function of years of career") %>% 
  layout(yaxis = list(name = "log(salary)"))
fig

outlier <- data$Salary_1987[data$Name == 'Jeffrey Leonard']
log(outlier)
outlier2 <- data$Salary_1987[data$Name == 'Steve Sax']
log(outlier2)



#--------------------------------------------------------------------------
X_back <- X
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Assists_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Put_outs_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Team_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Hits_career))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(TRP_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Position_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Errors_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(Division_1986))
mod <- lm(y ~ ., X_back)
summary(mod)

X_back <- subset(X_back, select = -c(League_1986))
mod <- lm(y ~ ., X_back)
summary(mod)



Division_W <- subset(data, select = (Division_1986=="W"))
Division_E <- subset(data, select = (Division_1986=="E"))
League_A <- subset(data, League_1987=="A")
League_N <- subset(data, League_1987=="N")

fig1 <- plot_ly(x = Division_E$Salary_1987,
                type = 'box',
                marker = list(color = mypalette[2]))
fig1
fig2 <- plot_ly(x = Division_W$Salary_1987,
                type = 'histogram',
                marker = list(color = mypalette[3]),
                nbinsx = 100)
fig <- subplot(fig1, fig2)
fig

fig1 <- plot_ly(data, y = ~Salary_1987, color = ~Division_1986, type = "box",
               colors = mypalette[1:2])
fig2 <- plot_ly(data, y = ~Salary_1987, color = ~League_1986, type = "box",
                colors = mypalette[3:4])
subplot(fig1, fig2)
