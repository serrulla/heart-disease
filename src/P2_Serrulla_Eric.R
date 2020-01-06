heartData <- read.csv('heart.csv')
summary(heartData)

str(heartData)

# 

emptyCa <- which(heartData$ca > 3)
heartData[emptyCa,]$ca <- NA
emptyThal <- which(heartData$thal == 0)
heartData[emptyThal,]$thal <- NA

heartData$age <- as.numeric(heartData$age)
heartData$trestbps <- as.numeric(heartData$trestbps)
heartData$chol <- as.numeric(heartData$chol)
heartData$thalach <- as.numeric(heartData$thalach)
heartData$sex <- factor(heartData$sex)
levels(heartData$sex) <- c("female", "male")
heartData$cp <- factor(heartData$cp)
levels(heartData$cp) <- c("typical","atypical","non-anginal","asymptomatic")
heartData$fbs <- factor(heartData$fbs)
levels(heartData$fbs) <- c("false", "true")
heartData$restecg <- factor(heartData$restecg)
levels(heartData$restecg) <- c("normal","stt","hypertrophy")
heartData$exang <- factor(heartData$exang)
levels(heartData$exang) <- c("no","yes")
heartData$slope <- factor(heartData$slope)
levels(heartData$slope) <- c("upsloping","flat","downsloping")
heartData$ca <- factor(heartData$ca)
heartData$thal <- factor(heartData$thal)
levels(heartData$thal) <- c("normal","fixed","reversable")
heartData$target <- factor(heartData$target)
levels(heartData$target) <- c("no","yes")

summary(heartData)

table(heartData$ca) #Previsualización de los valores de ca
table(heartData$thal) #Previsualización de los valores de thal


suppressWarnings(suppressMessages(library(VIM)))
heartData$ca <- kNN(heartData)$ca
heartData$thal <- kNN(heartData)$thal

table(heartData$ca)  #Previsualización de los valores de ca tras imputación
table(heartData$thal) #Previsualización de los valores de thal tras imputación

# 

boxplot(heartData$age)
title("Age")
boxplot(heartData$trestbps)
title("Resting blood pressure")
plot(heartData$trestbps)
title("Resting blood pressure")
boxplot(heartData$chol)
title("Serum cholestoral (mg/dl)")
plot(heartData$chol)
title("Serum cholestoral (mg/dl)")
heartData[which(heartData$chol>500),]
boxplot(heartData$thalach)
title("Maximum heart rate achieved")
boxplot(heartData$oldpeak)
title("ST depression")


# Creamos un subconjunto de los datos con solo las variables cuantitativas
heartData.numeric <- heartData[,unlist(lapply(heartData, is.numeric))]
plot(heartData.numeric)

corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable cuantitativa
# con respecto al campo "age"
for (i in 2:(ncol(heartData.numeric))) {
  print(i)
  spearman_test = cor.test(heartData.numeric[,i],
                           heartData.numeric[,1],
                           method = "spearman",
                           exact = FALSE)
  corr_coef = spearman_test$estimate
  p_val = spearman_test$p.value
  # Add row to matrix
  pair = matrix(ncol = 2, nrow = 1)
  pair[1][1] = corr_coef
  pair[2][1] = p_val
  corr_matrix <- rbind(corr_matrix, pair)
  rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(heartData.numeric)[i]
}

par(mfrow=c(1,1))
print(corr_matrix)

par(mfrow=c(2,2))
for(i in 1:ncol(heartData.numeric)) {
  title <- colnames(heartData.numeric[i])
  qqnorm(heartData.numeric[,i],main = paste("Normal Q-Q Plot for",title))
  qqline(heartData.numeric[,i],col="red")
  hist(heartData.numeric[,i],
       xlab= title,
       freq = FALSE,
       main = paste("Histogram of", title))
  shapiro.test(heartData.numeric[,i])
  print(paste("Test for", title))
  print(shapiro.test(heartData.numeric[,i]))
}

# Test homogeneidad
fligner.test(chol ~ sex, data = heartData)


# 4.3


# 4.3.1. Contraste de hipótesis
t.test(heartData[heartData$sex == "female",]$chol, heartData[heartData$sex == "male",]$chol)

# 4.3.2. Modelo de regresión lineal
 
model <- lm(thalach ~ age, data = heartData.numeric)
summary(model)

model <- lm(thalach ~ ., data = heartData.numeric)
summary(model)

# Predicción de thalach para pacientes con enfermedad cardiovascular  
heartData.glm<-heartData[which(heartData$target=="yes"),]
ntrain <- nrow(heartData.glm)*0.8
ntest <- nrow(heartData.glm)*0.2
index_train<-sample(1:nrow(heartData.glm),size = ntrain)
train<-heartData.glm[index_train,]
test<-heartData.glm[-index_train,]
model<-lm(thalach ~ age, data=train)
summary(model)

predicted.data<-predict(model, test, type="response")
mc_sl<-data.frame(
  real=test$thalach,
  predicted= predicted.data,
  dif=ifelse(test$thalach>predicted.data, -predicted.data*100/test$thalach,predicted.data*100/test$thalach)
)
colnames(mc_sl)<-c("Real","Predicho","Dif (%)")
head(mc_sl)

# 4.3.3. Modelo de regresión logística

glmodel <- glm(target ~ ., data = heartData, family = "binomial")
summary(glmodel)

predicted.data<-data.frame(
  real=heartData$target,
  predicted.probability= glmodel$fitted.values
)
predicted.data <- predicted.data[order(predicted.data$predicted.probability, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

head(predicted.data)
tail(predicted.data)

library(ggplot2)
suppressWarnings(suppressMessages(library(cowplot)))

ggplot(data=predicted.data, aes(x=rank, y=predicted.probability)) +
  geom_point(aes(color=real), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

prob <- predict(glmodel, data.frame(age=41, sex = "female", cp="atypical", trestbps=130, chol=204, fbs="false", restecg="normal", thalach=172, exang="no", oldpeak= 1.4, slope="downsloping", ca="0", thal= "fixed") , type = "response")
print(paste("La probabilidad de que padezca enfermedad cardiovascular es de", format(round(prob*100, 2), nsmall = 2), "%"))



# Exportación de datos en csv
write.csv(heartData, "heart-clean.csv")

