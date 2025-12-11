
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
    TRUE ~ pais_nacimiento 
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




ciudades_usa <- datos1 %>% 
  filter(pais_nacimiento %in% c("USA", "United States", "United States of America")) %>%
  separate(geo_punto_2D, into = c("lat", "lon"), sep = ",", convert = TRUE) %>%
  
 
  group_by(ciudad_nacimiento, lat, lon) %>%
  summarise(total_premios = n(), .groups = "drop") %>%
  filter(!is.na(lat) & !is.na(lon) & ciudad_nacimiento != "") %>%
  

  arrange(total_premios)

mapa_eeuu <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))


ggplot() +
  geom_sf(data = mapa_eeuu, fill = "#F5F5F5", color = "gray80") +
  geom_point(data = ciudades_usa, 
             aes(x = lon, y = lat, size = total_premios, color = total_premios), 
             alpha = 0.5) +
  
  geom_text(data = ciudades_usa %>% filter(total_premios > 10),
            aes(x = lon, y = lat, label = ciudad_nacimiento),
            vjust = -0.2, size = 5, fontface = "bold", color = "black") +
  scale_size_continuous(range = c(2, 10), name = "Cantidad") + 
  scale_color_viridis_c(option = "plasma", name = "Cantidad") + 
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(
    title = "Ciudades de EE.UU. con más Premios Nobel",
    subtitle = "Tamaño del punto representa cantidad de nacimientos de laureados",
    x = "", y = ""
  ) +
  theme_void() +
  theme(legend.position = "right")
