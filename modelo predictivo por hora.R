
library(dplyr)
library(lubridate)
library(randomForest)
library(caret)
#dfi <- read.csv("feu-involucrado.csv")
# Suponiendo que ya tienes el dataframe `df` con la columna `siniestro_hora` convertida a formato de hora

setwd("C:/Users/jmour/Downloads/Observatorio Vial")
df <- read.csv("feu-siniestros.csv")
df$siniestro_hora <- hms::as_hms(df$siniestro_hora)

# Extraer la hora del día (sin minutos ni segundos) para usarla como variable predictora
df <- df %>%
  mutate(hora = hour(siniestro_hora))

# Si tienes otras variables predictoras, agrégalas aquí. Por ejemplo, el día de la semana
df <- df %>%
  mutate(dia_semana = wday(siniestro_fecha, label = TRUE, abbr = TRUE))

# Agrupar por hora para obtener la cantidad de siniestros por hora
df_model <- df %>%
  group_by(hora, dia_semana,luminosidad) %>%
  summarise(cantidad_siniestros = n())

# División en conjunto de entrenamiento (70%) y prueba (30%)
set.seed(123)  # Para hacer reproducible la división
trainIndex <- createDataPartition(df_model$cantidad_siniestros, p = 0.7, list = FALSE)

# Conjuntos de entrenamiento y prueba
df_train <- df_model[trainIndex, ]
df_test <- df_model[-trainIndex, ]

# Entrenar un modelo Random Forest para predecir cantidad de siniestros por hora
set.seed(123)
modelo_rf <- randomForest(cantidad_siniestros ~ hora + dia_semana + luminosidad, data = df_train, ntree = 500,mtry=2)

# Ver los resultados del modelo
print(modelo_rf)

# Predecir sobre el conjunto de prueba
predicciones <- predict(modelo_rf, newdata = df_test)
predicciones
# Evaluar el rendimiento del modelo con métricas como el RMSE (Error Cuadrático Medio)
rmse <- sqrt(mean((df_test$cantidad_siniestros - predicciones)^2))
cat("El RMSE del modelo es:", rmse)

r_squared <- cor(df_test$cantidad_siniestros, predicciones)^2
cat("El R² del modelo es:", r_squared)

##### predecir

df_test$predicciones <- predicciones
head(df_test)

ggplot(df_test, aes(x = cantidad_siniestros, y = predicciones)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicciones vs Valores Reales en Nuevos Datos",
       x = "Cantidad de Siniestros (Reales)",
       y = "Cantidad de Siniestros (Predichos)") +
  theme_minimal()

rmse <- sqrt(mean((df_test$cantidad_siniestros - df_test$predicciones)^2))
print( paste("rmse de los nuevos datos", rmse))





