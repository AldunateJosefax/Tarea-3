
install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

install.packages("readxl")
library(readxl)

install.packages("dplyr")
library(dplyr)

# Cargar los datos
datos <- read.csv("nobel_prize_laureates_dataset.csv", sep = ";", stringsAsFactors = FALSE)

#Cambiar nombre de las columnas para poder identificarlas con mayor facilidad

datos1 <- datos %>%
  rename(
    id = Id,
    nombre = Firstname,
    apellido = Surname,
    nombre_completo = `Complete.Name`,
    fecha_nacimiento = Born,
    fecha_fallecimiento = Died,
    pais_nacimiento = `Born.country`,
    codigo_pais_nacimiento = `Born.country.code`,
    ciudad_nacimiento = `Born.city`,
    pais_fallecimiento = `Died.country`,
    codigo_pais_fallecimiento = `Died.country.code`,
    ciudad_fallecimiento = `Died.city`,
    genero = Gender,
    ano = Year,
    categoria = Category,
    motivacion_general = `Overall.motivation`,
    motivacion = Motivation,
    nombre_organizacion = `Organization.name`,
    ciudad_organizacion = `Organization.city`,
    pais_organizacion = `Organization.country`,
    geo_forma = `Geo.Shape`,
    geo_punto_2D = `Geo.Point.2D`
  )

#Limpiar base de datos
datos_limpios <- datos1 %>%
  select(nombre, apellido, categoria, ano, genero, pais_nacimiento)

# Ver el resultado
head(datos_limpios)

#Gráficos por género

datos_limpios %>%
  filter(genero %in% c("male", "female")) %>%  
  ggplot(aes(x = genero, fill = genero)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  scale_y_continuous(
    breaks = seq(0, 1000, by = 100), 
    name = "Cantidad de premios"    
  ) +
  scale_fill_manual(values = c("male" = "forestgreen", "female" = "orange")) +
  labs(title = "Premios Nobel por géneros",
       subtitle = "Comparación de premios obtenidos",
       x = "Género",
       y = "Cantidad de premios") +
  theme_minimal()

#Gráfico por categoría

datos_limpios %>%
  ggplot(aes(x = categoria, fill = categoria)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(title = "Distribución de Premios Nobel",
       subtitle = "Total histórico entregado",
       x = "Categoría",
       y = "Cantidad de premios") +
  theme_minimal() +
  theme(legend.position = "none")

#Gráfico género y años

datos_limpios %>%
  filter(genero %in% c("male", "female")) %>% 
  group_by(ano, genero) %>% 
  summarise(cantidad = n(), .groups = 'drop') %>% 
  ggplot(aes(x = ano, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 1.5) + 
  scale_color_manual(values = c("male" = "forestgreen", "female" = "orange")) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +       
  labs(
    title = "Evolución de Premios Nobel por año y género",
    subtitle = "Comparativa",
    x = "Año",
    y = "Cantidad de premios",
    color = "Género"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

conteo_paises <- datos_limpios %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento == "United States of America" ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom", 
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    TRUE ~ pais_nacimiento # Mantiene el resto igual
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total_premios = n())

mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")

mapa_final <- mapa_mundo %>%
  left_join(conteo_paises, by = c("name" = "pais_nacimiento"))

ggplot(data = mapa_final) +
  geom_sf(aes(fill = total_premios), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#e0f3db",
    high = "#0868ac",
    na.value = "gray90",
    name = "Cantidad"
  ) +
  labs(
    title = "Distribución global de Premios Nobel",
    subtitle = "Reconocimiento por países",
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() + 
  theme(
    legend.position = "bottom", 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 12)             
  )



