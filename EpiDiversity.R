setwd("C:/Users/edith clemente arena/Desktop/EPIFITAS -GRADIENTE/treeline-epiphytes/treeline-epiphytes")
mi_data <- read.csv("estrucPOB.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
head(mi_data)     # Muestra las primeras filas
str(mi_data)      # Estructura de la base de datos
mi_data$n_shoots.leaf <- as.numeric(mi_data$n_shoots_leaf)
library(dplyr)

####PLOT
##CUENTA LA RIQUzA DE SPP POR QUE CUENTA EL NUMERO DE ESPECIES POR plot
RIQUEZASPP <- mi_data %>%
  group_by(plot) %>%              # Agrupa por árbol
  summarise(RIQUEZA = n_distinct(morphospe)) # Cuenta especies únicas
##conteo de individuos por plot
ABUNDANCIA <- mi_data %>%
  group_by(plot) %>%
  summarise(ABUNDANCIA = n())

####FOROFITO
#crea una tabla que resume la riqueza de especies y la abundancia a nivel de forofito
library(dplyr)
RESUMEN <- mi_data %>%
  group_by(forof) %>%
  summarise(
    RIQUEZA    = n_distinct(morphospe),  # especies únicas
    ABUNDANCIA = n()                     # número de individuos (filas)
  )
RESUMEN

####agregar la columna dap y plot a a tabla DATAT
data_tree <- mi_data %>%
  group_by(forof) %>%
  distinct(forof, dap, plot, genera_tree)

RESUMEN <- RESUMEN %>%
  left_join(data_tree, by = "forof")

RESUMEN

###crea la columna elevacion 
library(dplyr)
RESUMEN <- RESUMEN %>%
  mutate(
    elevation = case_when(
      plot == "ACJ-01" ~ 3537,
      plot == "APK-01" ~ 3625,
      plot == "TRU-01" ~ 3402,
      plot == "TRU-02" ~ 3261,
      TRUE ~ NA_real_   # si hubiera otros plots no definidos
    )
  )
RESUMEN

###CREAR TABLA 
write.csv2(RESUMEN,
           file = "tabforo.csv",
           row.names = FALSE
)



## GRAFICO puntos
library(ggplot2)

ggplot(RESUMEN, aes(x = elevation, y = ABUNDANCIA, color = plot)) +
  geom_point(size = 3) +
  labs(title = "Abundancia por plot",
       x = "elevation",
       y = "Abundancia") +
  theme_minimal()


###grafico de violin por elevacion

library(ggplot2)

###ABUNDANCIA
counts <- RESUMEN %>%
  group_by(elevation) %>%
  summarise(n = n_distinct(forof))

ggplot(RESUMEN, aes(x = factor(elevation), y = ABUNDANCIA, fill = factor(elevation))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_text(
    data = counts,
    aes(x = factor(elevation), y = 0, label = paste0("n=", n)),
    vjust = 1.5, color = "black"
  ) +
  labs(
    title = "Distribución de la Abundancia por Elevación",
    x = "Elevación (m)",
    y = "Abundancia"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

####RIQUEZA DE ESPECIES
ggplot(RESUMEN, aes(x = factor(elevation), y = RIQUEZA, fill = factor(elevation))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_text(
    data = counts,
    aes(x = factor(elevation), y = 0, label = paste0("n=", n)),
    vjust = 1.5, color = "black"
  ) +
  labs(
    title = "Riqueza por Elevación",
    x = "Elevación (m)",
    y = "Riqueza"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


####gráfico SCATTERPLOT DAP-ABUNDANCIA
ggplot(RESUMEN, aes(x = dap, y = ABUNDANCIA, color = factor(elevation))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre DAP y Abundancia",
    x = "DAP (cm)",
    y = "Abundancia",
    color = "Elevación (m)"
  ) +
  theme_minimal()

####gráfico SCATTERPLOT DAP-RIQUEZA
ggplot(RESUMEN, aes(x = dap, y = RIQUEZA, color = factor(elevation))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(
    title = "Relación entre DAP y RIQUEZA",
    x = "DAP (cm)",
    y = "Riqueza",
    color = "Elevación (m)"
  ) +
  theme_minimal()

#################VER GENERO DEL FOROFITO
####gráfico SCATTERPLOT DAP-ABUNDANCIA
ggplot(RESUMEN, aes(x = dap, y = ABUNDANCIA, color = factor(genera_tree))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relación entre DAP y Abundancia",
    x = "DAP (cm)",
    y = "Abundancia",
    color = "Genera  tree"
  ) +
  theme_minimal()

####gráfico SCATTERPLOT DAP-RIQUEZA
ggplot(RESUMEN, aes(x = dap, y = RIQUEZA, color = factor(genera_tree))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relación entre DAP y RIQUEZA",
    x = "DAP (cm)",
    y = "Riqueza",
    color = "Genera  tree"
  ) +
  theme_minimal()





###revice OUTLIERS
Q1 <- quantile(RESUMEN$ABUNDANCIA, 0.25, na.rm = TRUE)
Q3 <- quantile(RESUMEN$ABUNDANCIA, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers_abund <- RESUMEN %>%
  filter(ABUNDANCIA < (Q1 - 1.5 * IQR) | ABUNDANCIA > (Q3 + 1.5 * IQR))

outliers_abund


