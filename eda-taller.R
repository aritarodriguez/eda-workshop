# Código inicial
library(tidyverse)
library(ggplot2)
library(corrplot)

data(mtcars)
df <- mtcars

#Análisis Exploratorio Inicial 
str(df)
summary(df)

#Verifica si existen valores NA en el dataset
print(miss_var_summary(df))
# Visualizar valores faltantes
gg_miss_var(df) + 
  labs(title = "Patrón de Valores Faltantes por Variable")

#Estadísticas descriptivas: Calcula media, mediana, desviación estándar para las variables numéricas
df %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(. , na.rm = TRUE),
                     median = ~median(. , na.rm = TRUE),
                     sd = ~sd(. , na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}"))

# Identificar variables categóricas (factor o character)
categorical_vars <- df %>% 
  select(where(~ is.factor(.) | is.character(.))) %>% 
  names()

categorical_vars

#Parte 3: Visualización de Datos 
#Histogramas: Crea histogramas para mpg, wt, y hp

# Histograma de mpg
ggplot(df, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histograma de mpg", x = "mpg", y = "Frecuencia")

# Histograma de wt
ggplot(df, aes(x = wt)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Histograma de wt", x = "wt", y = "Frecuencia")

# Histograma de hp
ggplot(df, aes(x = hp)) +
  geom_histogram(binwidth = 20, fill = "salmon", color = "black") +
  labs(title = "Histograma de hp", x = "hp", y = "Frecuencia")


#Genera boxplots para mg, hp, wt
# Boxplot para mpg
ggplot(df, aes(y = mpg)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot de mpg", y = "mpg")

# Boxplot para disp
ggplot(df, aes(y = disp)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot de disp", y = "displacement")

# Boxplot para qsec
ggplot(df, aes(y = qsec)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Boxplot de qsec", y = "1/4 mile time (seconds)")

#Identifica valores atipicos 
# Función para detectar outliers usando la regla del IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  which(x < lower_bound | x > upper_bound)
}

# Aplicar la función a todas las variables numéricas
outliers_list <- lapply(df %>% select(where(is.numeric)), detect_outliers)

# Mostrar resultados
outliers_list

# Convertir am a variable categórica con etiquetas
df$am <- factor(df$am, labels = c("Automática", "Manual"))

# Boxplot de mpg por tipo de transmisión
ggplot(df, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  labs(
    title = "MPG por Tipo de Transmisión",
    x = "Tipo de Transmisión",
    y = "Millas por Galón (mpg)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  guides(fill = "none")

# Diagrama de dispersión de mpg vs wt
ggplot(df, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relación entre Peso del Vehículo (wt) y Consumo (mpg)",
    x = "Peso (1000 lbs)",
    y = "Millas por galón (mpg)"
  ) +
  theme_minimal()
# Convertir cyl a factor para que los colores representen categorías
df$cyl <- factor(df$cyl)

# Diagrama de dispersión mpg vs hp, coloreado por cyl
ggplot(df, aes(x = hp, y = mpg, color = cyl)) +
  geom_point(size = 3) +
  labs(
    title = "Relación entre HP y MPG, coloreado por número de cilindros",
    x = "Caballos de fuerza (hp)",
    y = "Millas por galón (mpg)",
    color = "Cilindros"
  ) +
  theme_minimal()



#Análisis de correlación:
# Matriz de correlación solo para variables numéricas
corr_matrix <- cor(df %>% select(where(is.numeric)))
corr_matrix
# Visualizar matriz de correlación
corrplot(corr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.6)
# Convertir matriz a tabla larga
corr_long <- as.data.frame(as.table(corr_matrix))

# Quitar duplicados y correlaciones de una variable consigo misma
corr_long <- corr_long %>%
  filter(Var1 != Var2) %>%
  distinct()

# Ordenar por la fuerza de la correlación (valor absoluto)
corr_ranked <- corr_long %>%
  arrange(desc(abs(Freq)))

# Mostrar las 3 más fuertes (positivas o negativas)
top3_correlations <- corr_ranked[1:3, ]
top3_correlations

#Analisis por grupos
df$cyl <- factor(df$cyl)   # Asegurar que cyl sea categórica

# Agrupar por cilindros
grouped <- df %>% group_by(cyl)
grouped

#Calcular estadísticas descriptivas de mpg por grupo
stats_mpg_cyl <- df %>%
  group_by(cyl) %>%
  summarise(
    count = n(),
    mean_mpg = mean(mpg, na.rm = TRUE),
    median_mpg = median(mpg, na.rm = TRUE),
    sd_mpg = sd(mpg, na.rm = TRUE),
    min_mpg = min(mpg),
    max_mpg = max(mpg)
  )

stats_mpg_cyl

#Crear boxplots de mpg por grupo de cilindros
ggplot(df, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  labs(
    title = "MPG por Grupo de Cilindros",
    x = "Número de Cilindros",
    y = "Millas por Galón (mpg)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = "none")

#Preprar la variable de transmision
df$am <- factor(df$am, labels = c("Automática", "Manual"))
#Estadisticas descriptivas de mpg por tipo de transmision
stats_mpg_am <- df %>%
  group_by(am) %>%
  summarise(
    count = n(),
    mean_mpg = mean(mpg),
    median_mpg = median(mpg),
    sd_mpg = sd(mpg),
    min_mpg = min(mpg),
    max_mpg = max(mpg)
  )

stats_mpg_am
#crear boxplots comparativos
ggplot(df, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  labs(
    title = "Comparación de MPG entre Transmisión Automática y Manual",
    x = "Tipo de Transmisión",
    y = "Millas por galón (mpg)"
  ) +
  theme_minimal() +
  guides(fill = "none")

#Analisis
t.test(mpg ~ am, data = df)

#modelo de rgresion multiple
modelo <- lm(mpg ~ wt + hp + cyl, data = df)
summary(modelo)





