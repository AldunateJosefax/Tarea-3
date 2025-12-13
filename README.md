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

Por temas de comodidad y debido a que la base de datos está en inglés, decidí cambiarle los nombres y dejarlos en español, de tal forma de poder identificarlas con mayor facilidad.

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

También, entendiendo que la base de datos es sumamente grande y tiene muchos datos, decidí sólo quedarme con el nombre, apellido, categoría, año, género y país de nacimiento, que eran variables de mi interés.

```{r}
datos_limpios <- datos1 %>%
  select(nombre, apellido, categoria, ano, genero, pais_nacimiento)
```

Revisamos la base

```{r}
head(datos_limpios)
```

### Gráficos

Primero comenzaremos haciendo la comparativa entre género, para observar cual ha recibido más premios a lo largo de la historia

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
<img width="1200" height="800" alt="Gráfico géneros" src="https://github.com/user-attachments/assets/6b542577-7dd9-41ab-a0a8-1d0fa6ee6388" />

A pesar, de que ya asumía que habría una diferencia muy alta entre hombres y mujeres, no pensé que era tanta.

Por otra parte, haremos un gráfico por las categorías de estos premios, para observar cómo es la dinámica.

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

<img width="1200" height="800" alt="Gráfico por categoría" src="https://github.com/user-attachments/assets/85c05482-0054-4e9e-813a-be424025f810" />

En base a mi ignorancia, pensaba que la entrega de estos premios era mayormente igual, es decir, que se entregaban todos los años las mismas cantidad de cada premio, pero vemos que no y que estaba erróneo mi pensamiento.

Luego en base a que tenemos más variables, veremos como a lo largo de los años ha cambiado la entrega de premios a hombres y a mujeres, para poder ver si ha habido algún avance significativo.

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

<img width="1200" height="800" alt="Gráfico género y años" src="https://github.com/user-attachments/assets/c4c60946-4144-4547-a48d-d8df56e27ea3" />

Bueno gente, asi como que cambiamos sumamente importantes no, se entregaron más premios a las mujeres en los últimos años, sí, se acerca a la cantidad entregada por hombres, no.

**Mapas...**

Entrando a la creación de mapas, esta es una forma de poder ver de manera más concreta como se dan los premios en el mundo y quienes dependiendo su país de origen han tenido mayor recibimiento de estos premios. En cuanto a esta parte, estuvo un poco complicada, debido a las dimensiones, los colores, poder visualizar todo a pesar de la concentración que tienen los premios hacia zonas específicas.

Primero tenemos que hacer una limpieza en cuanto a los países y sus nombres de tal manera que se contabilicen todos, de tal forma de entandarizarlos y que así coincidan con el mapa, además de agruparlos por país y categoría.

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

Descargamos el mapa mundial...

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
```
Y unimos nuestros datos con el mapa

```{r}
mapa_final <- mapa_mundo %>%
  left_join(conteo_paises, by = c("name" = "pais_nacimiento"))
```
Creamos el mapa...

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

<img width="1200" height="800" alt="Mapa de premios" src="https://github.com/user-attachments/assets/ec1b7dfa-fb7c-408e-849b-4a0d7d91c7cc" />

Finalmente queda este maravilloso mapa que presenta tendencias claritas.

Sin embargo, esto no es todo, ahora para poder observa con un poco más de detalle, realizaremos mapas por categoría.

**Mapa por categoría**

***Mapa medicina***

Repetimos pasos que utilizamos anteriomente.. Contamos premios por categoría y país, estandarizamos nombres para que coincidan entre la base y el mapa y agrupamos por país y categoría

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

Descargamos mapa mundial oficial

```{r}
mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
```

Unimos los datos con el mapa

```{r}
mapa_final_individual <- mapa_mundo %>%
  left_join(datos_filtrados, by = c("name" = "pais_nacimiento"))
```

Creamos nuestro mapa, aquí los colores utilicé los mismo con los que están en el gráfico de barra por categoría más arriba.

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

<img width="1200" height="800" alt="Mapa Medicina Oficial" src="https://github.com/user-attachments/assets/c8f6f156-4592-4d38-a224-ea904eefe5ae" /> 

Para seguir creando los mapas por categoría, copié el código y sólo cambiaba el nombre de la categoría, para que realizará el mismo procedimiento pero con otra categoría y es por ello, que no específicaré los códigos de las siguientes.

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
<img width="1200" height="800" alt="Mapa Química" src="https://github.com/user-attachments/assets/5b9f114f-e3fb-4d91-8705-f7f8b7751405" />


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
<img width="1200" height="800" alt="Mapa Física" src="https://github.com/user-attachments/assets/e1bd0044-9953-4034-a584-f4f5d06e40b1" />


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

<img width="1200" height="800" alt="Mapa Paz" src="https://github.com/user-attachments/assets/2128be17-0d78-4ecf-bbad-770ebe5f743f" />


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
<img width="1200" height="800" alt="Mapa literatura" src="https://github.com/user-attachments/assets/b71d793a-f276-4356-a832-baf6ab61858f" />


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
<img width="1200" height="800" alt="Mapa Economía" src="https://github.com/user-attachments/assets/32afbd99-79c4-472c-970b-d74cf0157e0c" />

Finalmente, luego de procesar los datos, mapear el mundo y pelear con el código, la conclusión puede ser clara, en cuanto a que el talento puede ser universal, pero las oportunidades no lo han sido tanto. Los gráficos nos contaron una historia de concentración de el recibimiento de los premios, donde  el "ganador promedio" histórico sigue teniendo un perfil geográfico y de género muy específico (occidental y masculino). Sin embargo, podemos pensar que las líneas de tiempo más recientes nos dan un spoiler esperanzador, en base a que la tendencia está cambiando (supuestamente) y que las curvas de género van hacia arriba y el mapa se va pintando de nuevos colores más allá de Europa y EE.UU.

Esperemos que en unos años más los gráficos y mapas sean totalmente distintos!
