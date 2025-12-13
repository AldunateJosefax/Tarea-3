# Premios y diferencias

## Los premios Nobel, a lo largo de la historia han sido sumamente significativos, y sobre todo para aquellos profesionales que se dedican en alguna de las disciplinas que este premia. Sin embargo, este se ha presentado como un premio de acceso universal, que refleja y felicita a personas destacadas y así un sin fin de cosas, pensando que este puede llegar a ser un reflejo de avances de nuestra sociedad. PERO, ¿es realmente así?. Y aqui es donde observaremos como los premios han demostrado ciertas tendencias o no. 

![f elconfidencial com-original-7c9-985-2ce-7c99852ce2bc3e77a8e7509ec7bdde1b](https://github.com/user-attachments/assets/68e55140-6583-44c1-8f7e-0e7048e4af9b)

Para ello, lo primero será instalar los paquetes

```{r}
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readxl")
install.packages("readr")
install.packages("dplyr")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
```

Para luego cargar las librerías correspondientes

```{r}
library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
```

Posteriormente cargaremos los datos que fueron entregados por https://github.com/moonshinerd/analise-premios-nobel/blob/main/Trabalho_APC_Nobel_Prizes.ipynb


```{r}
datos <- read_delim(
  "nobel_prize_laureates_dataset.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)
```

mbiar nombre de las columnas para poder identificarlas con mayor facilidad

```{r}
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
```

Limpiar base

```{r}
datos_limpios <- datos1 %>%
  select(nombre, apellido, categoria, ano, genero, pais_nacimiento)
```

Revisar la base de datos

```{r}
head(datos_limpios)
```

Gráficar por género

```{r}
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
```

Gráficar por categoría

```{r}
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
```

Gráfico por género y año

```{r}
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

```

**Mapas...**

```{r}
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
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
```

```{r}
mapa_final <- mapa_mundo %>%
  left_join(conteo_paises, by = c("name" = "pais_nacimiento"))
```

```{r}
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
```

**Mapa por categoría**

***Mapa medicina***

```{r}
datos_filtrados <- datos_limpios %>% 
  filter(categoria == "Medicine") %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento %in% c("United States of America", "USA") ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom",
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    pais_nacimiento == "Union of Soviet Socialist Republics" ~ "Russia",
    pais_nacimiento == "Netherlands" ~ "Netherlands",
    TRUE ~ pais_nacimiento
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total = n(), .groups = "drop")
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

```{r}
ggplot(data = mapa_final_individual) +
  geom_sf(aes(fill = total), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F5F5F5",   
    high = "#00BFC4",  
    na.value = "#F0F0F0",
    name = "Premios"
  ) +
  labs(
    title = paste("Mapa de Premios Nobel:", "Medicina"),
    subtitle = paste("Premios por país en la categoría de Medicina"),
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
```

***Mapa Química***

```{r}
datos_filtrados <- datos_limpios %>% 
  filter(categoria == "Chemistry") %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento %in% c("United States of America", "USA") ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom",
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    pais_nacimiento == "Union of Soviet Socialist Republics" ~ "Russia",
    pais_nacimiento == "Netherlands" ~ "Netherlands",
    TRUE ~ pais_nacimiento
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total = n(), .groups = "drop")
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

```{r}
ggplot(data = mapa_final_individual) +
  geom_sf(aes(fill = total), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F5F5F5",   
    high = "#F8766D",  
    na.value = "#F0F0F0",
    name = "Premios"
  ) +
  labs(
    title = paste("Mapa de Premios Nobel:", "Química"),
    subtitle = paste("Premios por país en la categoría de Química"),
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
```

***Mapa física***

```{r}
datos_filtrados <- datos_limpios %>% 
  filter(categoria == "Physics") %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento %in% c("United States of America", "USA") ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom",
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    pais_nacimiento == "Union of Soviet Socialist Republics" ~ "Russia",
    pais_nacimiento == "Netherlands" ~ "Netherlands",
    TRUE ~ pais_nacimiento
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total = n(), .groups = "drop")
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

```{r}
ggplot(data = mapa_final_individual) +
  geom_sf(aes(fill = total), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F5F5F5",   
    high = "#F564E3",  
    na.value = "#F0F0F0",
    name = "Premios"
  ) +
  labs(
    title = paste("Mapa de Premios Nobel:", "Física"),,
    subtitle = paste("Premios por país en la categoría de fisica"),
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
```

***Mapa paz***

```{r}
datos_filtrados <- datos_limpios %>% 
  filter(categoria == "Peace") %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento %in% c("United States of America", "USA") ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom",
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    pais_nacimiento == "Union of Soviet Socialist Republics" ~ "Russia",
    pais_nacimiento == "Netherlands" ~ "Netherlands",
    TRUE ~ pais_nacimiento
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total = n(), .groups = "drop")
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

```{r}
ggplot(data = mapa_final_individual) +
  geom_sf(aes(fill = total), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F5F5F5",   
    high = "#619CFF",  
    na.value = "#F0F0F0",
    name = "Premios"
  ) +
  labs(
    title = paste("Mapa de Premios Nobel:", "Paz"),,
    subtitle = paste("Premios por país en la categoría de la paz"),
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
```

***Mapa Literatura***

```{r}
datos_filtrados <- datos_limpios %>% 
  filter(categoria == "Literature") %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento %in% c("United States of America", "USA") ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom",
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    pais_nacimiento == "Union of Soviet Socialist Republics" ~ "Russia",
    pais_nacimiento == "Netherlands" ~ "Netherlands",
    TRUE ~ pais_nacimiento
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total = n(), .groups = "drop")
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

```{r}
ggplot(data = mapa_final_individual) +
  geom_sf(aes(fill = total), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F5F5F5",   
    high = "#00BA38",  
    na.value = "#F0F0F0",
    name = "Premios"
  ) +
  labs(
    title = paste("Mapa de Premios Nobel:", "Literatura"),,
    subtitle = paste("Premios por país en la categoría de literatura"),
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
```

***Mapa Economía***

```{r}
datos_filtrados <- datos_limpios %>% 
  filter(categoria == "Economics") %>%
  filter(!is.na(pais_nacimiento)) %>%
  mutate(pais_nacimiento = case_when(
    pais_nacimiento %in% c("United States of America", "USA") ~ "United States",
    pais_nacimiento == "United Kingdom" ~ "United Kingdom",
    pais_nacimiento %in% c("Germany (Fed. Rep.)", "Germany (DR)") ~ "Germany",
    pais_nacimiento == "Union of Soviet Socialist Republics" ~ "Russia",
    pais_nacimiento == "Netherlands" ~ "Netherlands",
    TRUE ~ pais_nacimiento
  )) %>%
  group_by(pais_nacimiento) %>%
  summarise(total = n(), .groups = "drop")
```

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

```{r}
ggplot(data = mapa_final_individual) +
  geom_sf(aes(fill = total), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#F5F5F5",   
    high = "#B79F00",  
    na.value = "#F0F0F0",
    name = "Premios"
  ) +
  labs(
    title = paste("Mapa de Premios Nobel:", "Economía"),,
    subtitle = paste("Premios por país en la categoría de economía"),
    caption = "Fuente: Dataset Nobel"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
```
