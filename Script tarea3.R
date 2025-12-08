
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

# Verificar los nuevos nombrescolnames(datos)


datos_limpios <- datos1 %>%
  select(nombre, apellido, categoria, ano, genero, pais_nacimiento)

# Ver el resultado
head(datos_limpios)
