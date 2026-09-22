# ==============================================================================
# =================== Trabajo Práctico Integrador - Módulo R =================== 
# ========================= Alumna: María Belén Paisan =========================
# ======================= Maestría en Econometría - UTDT =======================
# =========================== Segundo Trimestre 2026 ===========================
# ==============================================================================

# Working Directory (insertar el propio)
setwd("...")

# Librerías a utilizar
library(tidyverse) # Manejo de datos
library(eph) # Datos de EPH
library(pROC) # Curva ROC
library(openxlsx) # Guardado de tablas en excel

# Desactivar notación científica
options(scipen = 999)

# Parte 1: Elección del dataset y de la técnica --------------------------------

# El dataset elegido es la Encuesta Permanente de Hogares (EPH).
# Se utilizarán las ultimas bases disponibles de individuos y hogares (1T- 2026)

# Carga de las bases con la librería eph.
eph_ind_base <- get_microdata(year = 2026, trimester = 1, type = "individual")
eph_hog_base <- get_microdata(year = 2026, trimester = 1, type = "hogar")

# Parte 2: Análisis exploratorio de datos (EDA) --------------------------------

# Tamaño de datasets
individuos <- nrow(eph_ind_base)
hogares <- nrow(eph_hog_base)
variables_ind <- ncol(eph_ind_base)
variables_hog <- ncol(eph_hog_base)
cat("Cantidad de individuos = ", individuos,
    "\nVariables de individuos = ", variables_ind,
    "\nCantidad de hogares = ", hogares,
    "\nVariables de hogares = ", variables_hog)

# Hay variables que son predictores perfectos del estado de ocupación.
# Se eliminan esas variables de la base de individuos, excepto el ingreso no laboral
eph_ind <- eph_ind_base |> select(CODUSU:ESTADO, T_VI) 

# Selección de variables de la base de hogares que se van a utilizar
eph_hog <- eph_hog_base |> select(CODUSU:IV1, IV2, IV3, IV5, IV6, IV10, IV12_3, II7, 
                             II8, IX_TOT, IX_MEN10)


# Población a analizar: personas mayores de 18 y menores de 65, jefes de hogar, 
# ocupados o desocupados y sin estudios universitarios
datos_ind <- eph_ind |> 
  filter(CH06 > 18) |> # Mayores de 18
  filter(CH06 < 65) |> # Menores de 65
  filter(COMPONENTE == 1) |> # Jefes de hogar
  filter(ESTADO == 1 | ESTADO == 2) |> # Ocupado o Desocupado
  filter(NIVEL_ED == 1 | NIVEL_ED == 2 | NIVEL_ED == 3 | NIVEL_ED == 4 | NIVEL_ED ==7)
  # Primario incompleto o completo, Secundario completo o incompleto o Sin instrucción

# Nuevo tamaño de datasets
individuos <- nrow(datos_ind)
hogares <- nrow(eph_hog)
variables_ind <- ncol(datos_ind)
variables_hog <- ncol(eph_hog)
cat("Cantidad de individuos = ", individuos,
    "\nVariables de individuos = ", variables_ind,
    "\nCantidad de hogares = ", hogares,
    "\nVariables de hogares = ", variables_hog)


# Variables de identificación (iguales en ambos datasets)
variables_comunes <- c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE", "REGION", 
                       "MAS_500", "AGLOMERADO", "PONDERA")
# Unión de ambas bases por las variables anteriores
datos <- datos_ind |> left_join(eph_hog, by = variables_comunes)

# Tamaño de dataset unido
individuos <- nrow(datos)
variables <- ncol(datos)
cat("Cantidad de individuos = ", individuos,
    "\nVariables = ", variables)

# Cambios de nombres
datos<- datos|> rename(genero = CH04,
                       edad = CH06,
                       e_civil = CH07,
                       ingreso_no_laboral = T_VI,
                       ambientes = IV2,
                       pisos = IV3,
                       techo = IV5,
                       agua = IV6,
                       baño = IV10,
                       v_emer = IV12_3,
                       propiedad = II7,
                       gas = II8,
                       cant_miembros = IX_TOT,
                       cant_menores = IX_MEN10) 

### Análisis de variables

## Ingreso no laboral

# Control de No Respuestas específico para ingresos (código -9)
datos|>filter(ingreso_no_laboral == -9)|> count()

# Son 140, el 2,5% de la muestra.

# Filtro según la condición anterior
datos <- datos|>filter( ingreso_no_laboral != -9)

summary(datos$ingreso_no_laboral)
# Hay valores muy grandes, por lo que se transforma en millones.
datos<- datos |> mutate(ingreso_no_laboral = ingreso_no_laboral/1000000)

# Gráfico de distribución
ggplot(datos, aes(x = ingreso_no_laboral, weight= PONDERA)) +
  geom_density(fill = "hotpink", alpha = 0.5, color = "hotpink") +
  labs(
    title    = "Distribución del ingreso no laboral \n",
    caption  = "Gráfico 2.1",
    y        = "Densidad \n",
    x        = "\n Ingreso no laboral (en millones) ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank())
#ggsave(filename = "gráfico2_1.png")

## Estado

# Recodificar ESTADO para que 1 (Ocupado) sea 0 y 2 (Desocupado) sea 1.
datos$ESTADO[datos$ESTADO == 1] <-0
datos$ESTADO[datos$ESTADO == 2] <-1

## Género

# Recodificar genero para que 2 (Mujer) sea 0.
datos$genero[datos$genero == 2] <-0

# Gráfico
ggplot(datos, aes(x = as.factor(genero), weight= PONDERA, fill = as.factor(ESTADO))) +
  geom_bar(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados")) +
  labs(
    title    = "Hombres y Mujeres según estado de ocupación ",
    subtitle = "0 = Mujeres, 1 = Hombres \n",
    caption  = "Gráfico 2.2",
    y        = "Cantidad \n",
    x        = "\n Género ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.6))
#ggsave(filename = "gráfico2_2.png")

## Estado civil

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(e_civil), weight= PONDERA, fill = as.factor(e_civil))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", "4" = "dodgerblue",
               "5" = "firebrick2"),
    labels = c("1" = "Unido", "2" = "Casado", "3" = "Separado/Divorciado", "4" = "Viudo",
               "5" = "Soltero")) +
  labs(
    title    = "Estado Civil \n",
    caption  = "Gráfico 2.3",
    y        = "Cantidad \n",
    x        = "\n Estado Civil ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.8))
#ggsave(filename = "gráfico2_3.png")

# Se agrupa estado civil de unido o casado en 1 y el resto de las opciones en 0
datos <- datos |> mutate(e_civil = case_when(
  e_civil %in% c(1, 2) ~ 1,
  e_civil %in% c(3, 4, 5) ~ 0))

# Ejemplo de como quedan variables reagrupadas
ggplot(datos, aes(x = as.factor(e_civil), weight= PONDERA, fill = as.factor(ESTADO))) +
  geom_bar(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados")) +
  labs(
    title    = "Estado Civil según estado de ocupación ",
    subtitle = "0 = Casados/Unidos, \n1 = Separados/Divorciados/Viudos/Solteros \n",
    caption  = "Gráfico 2.4",
    y        = "Cantidad \n",
    x        = "\n Estado Civil ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.6))
#ggsave(filename = "gráfico2_4.png")

## Nivel Educativo

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(NIVEL_ED), weight= PONDERA, fill = as.factor(NIVEL_ED))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", "4" = "dodgerblue",
               "7" = "firebrick2"),
    labels = c("1" = "Primario Incompleto", "2" = "Primario Completo", "3" =
              "Secundario Incompleto", "4" = "Secundario Completo", "7" = 
              "Sin Instrucción")) +
  labs(
    title    = "Nivel Educativo \n",
    caption  = "Gráfico 2.5",
    y        = "Cantidad \n",
    x        = "\n Nivel educativo máximo ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 2))
#ggsave(filename = "gráfico2_5.png")

# Creación de dummies para nivel educativo. Se deja "Sin instrucción" como base.
datos <- datos |> mutate(prim_inc = as.numeric(NIVEL_ED == 1),
                         prim_com = as.numeric(NIVEL_ED == 2),
                         sec_inc = as.numeric(NIVEL_ED == 3),
                         sec_com = as.numeric(NIVEL_ED == 4))

## Región

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(REGION), weight= PONDERA, fill = as.factor(REGION))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "40" = "maroon2", "41" = "goldenrod2", 
               "42" = "dodgerblue", "43" = "firebrick2", "44" = "orange"),
    labels = c("1" = "Gran Bs. As.", "40" = "Noroeste", "41" = "Noreste", 
               "42" = "Cuyo", "43" = "Pampeana", "44" = "Patagonia")) +
  labs(
    title    = "Región \n",
    caption  = "Gráfico 2.6",
    y        = "Cantidad \n",
    x        = "\n Regiones ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico2_6.png")

# Creación de dummies para regiones. Se deja "Gran Bs. As." como base.
datos <- datos |> mutate(nor_o = as.numeric(REGION == 40),
                         nor_e = as.numeric(REGION == 41),
                         cuyo = as.numeric(REGION == 42),
                         pampa = as.numeric(REGION == 43),
                         patag = as.numeric(REGION == 44))

## Mas_500

# Recodificar MAS_500 para que aquellos con menos de 500.000 habitantes sean 0 (y más 1)
datos <- datos |> mutate(MAS_500 = case_when(
  MAS_500 %in% c("S") ~ 1,
  MAS_500 %in% c("N") ~ 0))

# Gráfico
ggplot(datos, aes(x = as.factor(MAS_500), weight= PONDERA, fill = as.factor(ESTADO))) +
  geom_bar(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados")) +
  labs(
    title    = "Cantidad de habitantes en el aglomerado ",
    subtitle = "0 = Menos de 500.000, 1= Más de 500.000\n",
    caption  = "Gráfico 2.7",
    y        = "Cantidad \n",
    x        = "\n Habitantes en el aglomerado ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.6))
#ggsave(filename = "gráfico2_7.png")

## Vivienda

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(IV1), weight= PONDERA, fill = as.factor(IV1))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", 
               "4" = "dodgerblue", "5" = "firebrick2"),
    labels = c("1" = "Casa", "2" = "Departamento", "3" = "Pieza en inquilinato", 
               "4" = "Pieza en hotel/pensión", "5" = "Local")) +
  labs(
    title    = "Tipo de vivienda \n",
    caption  = "Gráfico 2.8",
    y        = "Cantidad \n",
    x        = "\n Tipos ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 2))
#ggsave(filename = "gráfico2_8.png")

# Creación de dummies para tipo de vivienda. Debido a que hay pocos casos
# de piezas en inquilinato/hotel/pensión o locales, se agrupan todas en una sola
# dummy. Se deja "Casa" como base.
datos <- datos |> mutate(depto = as.numeric(IV1 == 2),
                         pieza_local = as.numeric(IV1 == 3 |IV1 == 4|IV1 == 5|IV1 == 6 ))

## Pisos

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(pisos), weight= PONDERA, fill = as.factor(pisos))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", 
               "4" = "dodgerblue"),
    labels = c("1" = "Mosaico/baldosa/madera\n/cerámica/alfombra", "2" = 
              "Cemento/ladrillo fijo", "3" = "Ladrillo suelto/tierra", 
               "4" = "Otros")) +
  labs(
    title    = "Material de pisos interiores \n",
    caption  = "Gráfico 2.9",
    y        = "Cantidad \n",
    x        = "\n Materiales ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 2))
#ggsave(filename = "gráfico2_9.png")

# Se agrupa el material de pisos interiores de mosaico/baldosa/madera/cerámica/alfombra
# en 1 y el resto de las opciones en 0.
datos <- datos |> mutate(pisos = case_when(
  pisos %in% c(1) ~ 1,
  pisos %in% c(2, 3, 4) ~ 0))

## Techo

# Recodificar techo para que 2 (sin cielorraso/revestimiento interior) sea 0.
datos$techo[datos$techo == 2] <-0

# Gráfico
ggplot(datos, aes(x = as.factor(techo), weight= PONDERA, fill = as.factor(ESTADO))) +
  geom_bar(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados")) +
  labs(
    title    = "Cielorraso o revestimiento en interior del techo ",
    subtitle = "0 = No, 1= Si\n",
    caption  = "Gráfico 2.10",
    y        = "Cantidad \n",
    x        = "\n Característica del techo ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.6))
#ggsave(filename = "gráfico2_10.png")


## Agua

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(agua), weight= PONDERA, fill = as.factor(agua))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2"),
    labels = c("1" = "Cañería dentro de la vivienda", "2" = 
            "Fuera de la vivienda pero en el terrerno", "3" = 
            "Fuera del terreno")) +
  labs(
    title    = "Característica de conexión de agua \n",
    caption  = "Gráfico 2.11",
    y        = "Cantidad \n",
    x        = "\n Tipo ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 4))
#ggsave(filename = "gráfico2_11.png")

# Se agrupa la característica de servicio de agua en 0 si es por cañería en vivienda
# y el resto de las opciones en 1.
datos <- datos |> mutate(agua = case_when(
  agua %in% c(1) ~ 0,
  agua %in% c(2, 3) ~ 1))   

## Baño

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(baño), weight= PONDERA, fill = as.factor(baño))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", 
               "4" = "dodgerblue"),
    labels = c("1" = "Inodoro con arrastre de agua", "2" = 
                 "A balde con arrastre de agua", "3" = "Letrina sin arrastre de agua", 
               "0" = "No tiene baño")) +
  labs(
    title    = "Tipo de baños \n",
    caption  = "Gráfico 2.12",
    y        = "Cantidad \n",
    x        = "\n Tipos ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 2.5))
#ggsave(filename = "gráfico2_12.png")

# Si el hogar tiene baño con inodoro la variable es igual a 0, y es igual a 1 si
# no tiene, es balde o letrina.
datos <- datos |> mutate(baño = case_when(
  baño %in% c(1) ~ 0,
  baño %in% c(0, 2, 3) ~ 1))

## Villa de emergencia

# Recodificar v_emer para que 2 (no está en villa de emergencia) sea 0.
datos$v_emer[datos$v_emer == 2] <-0

# Gráfico
ggplot(datos, aes(x = as.factor(v_emer), weight= PONDERA, fill = as.factor(ESTADO))) +
  geom_bar(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados")) +
  labs(
    title    = "Ubicación en una villa de emergencia ",
    subtitle = "0 = No, 1= Si\n",
    caption  = "Gráfico 2.13",
    y        = "Cantidad \n",
    x        = "\n Ubicación ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.6))
#ggsave(filename = "gráfico2_13.png")


## Propiedad de la vivienda

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(propiedad), weight= PONDERA, fill = as.factor(propiedad))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", 
               "4" = "dodgerblue", "5" = "firebrick2", "6" = "orange", 
               "7" = "darkmagenta", "8" = "royalblue4", "9" = "honeydew3"),
    labels = c("1" = "Propietario de vivienda y terreno", "2" = "Propietario de vivienda",
               "3" = "Inquilino", "4" = "Ocupante por pago de impuestos/expensas",
               "5" = "Ocupante en rel. de dependencia", "6" = "Ocupante gratuito (con permiso)",
               "7" = "Ocupante de hecho (sin permiso)", "8" = "En sucesión",
               "9" = "Otro")) +
  labs(
    title    = "Características de propiedad de la vivienda \n",
    caption  = "Gráfico 2.14",
    y        = "Cantidad \n",
    x        = "\n Tipo ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 1))
#ggsave(filename = "gráfico2_14.png", width = 12)

# Se agrupa si el individuo es propietario de la vivienda o de la vivienda 
# y el terreno en 1 y el resto de las opciones en 0. Se crea una dummy separada para
# los inquilinos (inq).
datos <- datos |> mutate(inq = as.numeric(propiedad == 3))
datos <- datos |> mutate(propiedad = case_when(
  propiedad %in% c(1, 2) ~ 1,
  propiedad %in% c(0, 3, 4, 5, 6, 7, 8, 9) ~ 0))   

## Gas

# Gráfico de variable original
ggplot(datos, aes(x = as.factor(gas), weight= PONDERA, fill = as.factor(gas))) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(
    values = c("1" = "olivedrab", "2" = "maroon2", "3" = "goldenrod2", 
               "4" = "dodgerblue"),
    labels = c("1" = "Gas de red", "2" = "Gas de tubo/garrafa", 
               "3" = "Kerosene/leña/carbón", "4" = "Otro")) +
  labs(
    title    = "Tipo de conexión de gas \n",
    caption  = "Gráfico 2.15",
    y        = "Cantidad \n",
    x        = "\n Tipos ",
    fill     = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 2))
#ggsave(filename = "gráfico2_15.png")

# Se agrupa en 1 si el gas es por tubo/garrafa o kerosene/leña/carbón, dejando
# gas de red como base.
datos <- datos |> mutate(gas = case_when(
  gas %in% c(1) ~ 0,
  gas %in% c(0, 2, 3, 4) ~ 1))   

## Edad

# Gráfico de distribución
ggplot() +
  geom_point(data = datos, aes(x = edad, y = as.factor(ESTADO), 
                               color = as.factor(ESTADO)), alpha = 0.1) +
  scale_color_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "1- Desocupados", "0" = "0- Ocupados")) +
  labs(
    title    = " Distribución de Edad \n ",
    caption  = "Gráfico 2.16",
    y        = "Estado \n",
    x        = "\n Edad ",
    color    = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico2_16.png")

## Cantidad de miembros

# Gráfico de distribución
ggplot() +
  geom_point(data = datos, aes(x = cant_miembros, y = as.factor(ESTADO), 
                               color = as.factor(ESTADO)), alpha = 0.1) +
  scale_color_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "1- Desocupados", "0" = "0- Ocupados")) +
  labs(
    title    = " Distribución de cantidad de miembros del hogar \n ",
    caption  = "Gráfico 2.17",
    y        = "Estado \n",
    x        = "\n Cantidad de miembros ",
    color    = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico2_17.png")

## Cantidad de miembros menores de 10 años

# Gráfico de distribución
ggplot() +
  geom_point(data = datos, aes(x = cant_menores, y = as.factor(ESTADO), 
                               color = as.factor(ESTADO)), alpha = 0.1) +
  scale_color_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "1- Desocupados", "0" = "0- Ocupados")) +
  labs(
    title    = " Distribución de cantidad de miembros del hogar \n menores de 10 años\n ",
    caption  = "Gráfico 2.18",
    y        = "Estado \n",
    x        = "\n Cantidad de miembros ",
    color    = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico2_18.png")

## Ambientes

# Gráfico de distribución
ggplot() +
  geom_point(data = datos, aes(x = ambientes, y = as.factor(ESTADO), 
                               color = as.factor(ESTADO)), alpha = 0.2) +
  scale_color_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "1- Desocupados", "0" = "0- Ocupados")) +
  labs(
    title    = " Distribución de cantidad de ambientes de la vivienda \n ",
    caption  = "Gráfico 2.19",
    y        = "Estado \n",
    x        = "\n Cantidad de ambientes ",
    color    = " ",
  ) +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    plot.subtitle      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico2_19.png")


# Control de NAS
colSums(is.na(datos))
# Solo hay en variables que no se van a utilizar (CH15_COD y CH16_COD)


# Parte 3: Aplicación de la técnica e interpretación --------------------------------

## Balance de la muestra

# Tabla de proporciones de ocupados y desocupados
prop.table(table(datos$ESTADO))

# Gráfico
balance <- as.data.frame(table(datos$ESTADO))
names(balance) <- c("ESTADO","Cantidad")
ggplot(balance, aes(x = ESTADO, y = Cantidad, fill = ESTADO)) +
  geom_col() +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados"))+
  labs(title = "Cantidad Ocupados vs Desocupados \ncon datos totales",
       x = "\n Estado de Ocupación",
       y = "Cantidad \n",
       caption = "Gráfico 3.1",
       fill = " ") +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico3_1.png")

# Aproximadamente hay 4,56% de desocupados y 95,44% de ocupados. Claramente está 
# desbalanceada. Deberemos tenerlo en cuenta para el umbral de clasificación.

## División de datos en train y test

# Semilla para replicación
set.seed(43)

# Selección aleatoria de ids de datos de train (70%)
id_train <- sample(1:nrow(datos), size = 0.7 * nrow(datos))

# Separación en train y test
datos_train <- datos[id_train, ]
datos_test <- datos[-id_train, ]

# Control de balance de submuestra de entrenamiento

# Tabla de proporciones de ocupados y desocupados
prop.table(table(datos_train$ESTADO))

# Gráfico
balance_train <- as.data.frame(table(datos_train$ESTADO))
names(balance_train) <- c("ESTADO","Cantidad")
ggplot(balance_train, aes(x = ESTADO, y = Cantidad, fill = ESTADO)) +
  geom_col() +
  scale_fill_manual(
    values = c("1" = "hotpink", "0" = "lightblue"),
    labels = c("1" = "Desocupados", "0" = "Ocupados"))+
  labs(title = "Cantidad Ocupados vs Desocupados \ncon datos de entrenamiento",
       x = "\n Estado de Ocupación",
       y = "Cantidad \n",
       caption = "Gráfico 3.2",
       fill = " ") +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.5))
#ggsave(filename = "gráfico3_2.png")

# Se mantiene la proporción.

## Primera estimación: modelo con variables del individuo

# Estimación del modelo logit con todas las variables del individuo sobre los 
# datos de entrenamiento
modelo_logit_1 <- glm(
  ESTADO ~ genero + edad + prim_inc + prim_com + sec_inc + sec_com + e_civil + MAS_500 + 
    nor_o + nor_e + cuyo + pampa + patag + ingreso_no_laboral,
  data = datos_train,
  family = binomial(link = "logit"))

# Generación de tabla de resultados
resumen <- summary(modelo_logit_1)
tabla1 <- as.data.frame(round(resumen$coefficients,3)) |> select(-c("z value"))
tabla1$cambio_odds<- c(round(exp(coef(modelo_logit_1)),3))
colnames(tabla1) <- c("Coeficiente", "S.E", "P_Value", "Cambio en odds")
aic <- c(round(resumen$aic,3), "", "", "")
tabla1 <- rbind(tabla1, "AIC" = aic)
print(tabla1)
#write.xlsx(tabla1, file = "Tabla3_1.xlsx", rowNames = TRUE)


# Predicción de probabilidades sobre los datos de testeo
probabilidades_base <- predict(modelo_logit_1, newdata = datos_test, type = "response")
# Clasificación según umbral de 0.5
prediccion_base <- ifelse(probabilidades_base > 0.5, 1, 0)

# Generación de matriz de confusión
matriz_base <- table(Predicción = prediccion_base, Real = datos_test$ESTADO)
print(matriz_base)

# Gráfico de la matriz
df_matriz_base <- as.data.frame(matriz_base)
ggplot(df_matriz_base, aes(x = Real, y = Predicción, fill = Freq)) +
  geom_tile(color = "beige") + 
  geom_text(aes(label = Freq), size = 10, fontface = "italic") +
  scale_fill_gradient(low = "palegreen",high = "limegreen") +
  labs(title = "Matriz de Estado predicho vs Estado Real \ncon umbral de 0.5",
       x = "Estado real", 
       y = "Estado predicho",
       caption = "Gráfico 3.3",
       fill = "Casos") +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.2))
#ggsave(filename = "gráfico3_3.png")

# Hay que ajustar por el desbalance de la muestra.

# Se repite el procedimiento con el umbral de 0.04 (parecido a la proporción de
# desocupados de la muestra)
probabilidades <- predict(modelo_logit_1, newdata = datos_test, type = "response")
prediccion <- ifelse(probabilidades > 0.04, 1, 0)

matriz <- table(Predicción = prediccion, Real = datos_test$ESTADO)
print(matriz)

df_matriz <- as.data.frame(matriz)
ggplot(df_matriz, aes(x = Real, y = Predicción, fill = Freq)) +
  geom_tile(color = "beige") + 
  geom_text(aes(label = Freq), size = 10, fontface = "italic") +
  scale_fill_gradient(low = "palegreen",high = "limegreen") +
  labs(title = "Matriz de Estado predicho vs Estado Real \ncon umbral modificado",
       x = "Estado real", 
       y = "Estado predicho",
       caption = "Gráfico 3.4",
       fill = "Casos") +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.2))
#ggsave(filename = "gráfico3_4.png")

# La clasificación es mejor.

# Curva ROC
roc <- roc(datos_test$ESTADO, probabilidades, levels = c("0", "1"), direction = "<")

# Gráfico
#png("curva_roc_1.png")
plot(roc, main = "Curva ROC", print.auc = TRUE)
#dev.off()


## Segunda estimación: modelo con variables del individuo y de la vivienda

# Estimación del modelo logit con todas las variables del individuo y todas las 
# variables de la vivienda sobre los datos de entrenamiento
modelo_logit_2 <- glm(
  ESTADO ~ genero + edad + prim_inc + prim_com + sec_inc + sec_com + e_civil + MAS_500 + 
    nor_o + nor_e + cuyo + pampa + patag + ingreso_no_laboral+ ambientes + pisos +
    techo + agua + baño + v_emer + propiedad + gas + cant_miembros + cant_menores + 
    depto + pieza_local + inq,
  data = datos_train,
  family = binomial(link = "logit"))

# Generación de tabla de resultados
resumen <- summary(modelo_logit_2)
tabla2 <- as.data.frame(round(resumen$coefficients,3)) |> select(-c("z value"))
tabla2$cambio_odds<- c(round(exp(coef(modelo_logit_2)),3))
colnames(tabla2) <- c("Coeficiente", "S.E", "P_Value", "Cambio en odds")
aic <- c(round(resumen$aic,3), "", "", "")
tabla2 <- rbind(tabla2, "AIC" = aic)
print(tabla2)
#write.xlsx(tabla2, file = "Tabla3_2.xlsx", rowNames = TRUE)

# Predicción de probabilidades sobre los datos de testeo
probabilidades_2 <- predict(modelo_logit_2, newdata = datos_test, type = "response")
# Clasificación según umbral de 0.04
prediccion_2 <- ifelse(probabilidades_2 > 0.04, 1, 0)

# Generación de matriz de confusión
matriz_2 <- table(Predicción = prediccion_2, Real = datos_test$ESTADO)
print(matriz_2)

# Gráfico de la matriz
df_matriz_2 <- as.data.frame(matriz_2)
ggplot(df_matriz_2, aes(x = Real, y = Predicción, fill = Freq)) +
  geom_tile(color = "beige") + 
  geom_text(aes(label = Freq), size = 10, fontface = "italic") +
  scale_fill_gradient(low = "palegreen",high = "limegreen") +
  labs(title = "Matriz de Estado predicho vs Estado Real \ncon umbral modificado",
       x = "Estado real", 
       y = "Estado predicho",
       caption = "Gráfico 3.6",
       fill = "Casos") +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.2))
#ggsave(filename = "gráfico3_6.png")

# Curva ROC
roc_2 <- roc(datos_test$ESTADO, probabilidades_2, levels = c("0", "1"), direction = "<")

# Gráfico
#png("curva_roc_2.png")
plot(roc_2, main = "Curva ROC", print.auc = TRUE)
#dev.off()

# Mejora la clasificación y la curva ROC. 


## Tercera estimación: modelo con variables del individuo y de la vivienda, elegidas
## automáticamente con método stepwise.

# Modelo mínimo (con intercepto) con datos de entrenamiento
modelo_min <- glm(ESTADO ~ 1, data = datos_train, family = binomial(link = "logit"))
# Modelo máximo (todas las variables) con datos de entrenamiento
modelo_max <- glm(
  ESTADO ~ genero + edad + prim_inc + prim_com + sec_inc + sec_com + e_civil + MAS_500 + 
    nor_o + nor_e + cuyo + pampa + patag + ingreso_no_laboral + ambientes + pisos +
    techo + agua + baño + v_emer + propiedad + gas + cant_miembros + cant_menores + 
    depto + pieza_local + inq,
  data = datos_train,
  family = binomial(link = "logit"))

# Selección stepwise hacia adelante y hacia atrás
modelo_elegido <- step(
  object    = modelo_min,  
  scope     = list(lower = modelo_min, upper = modelo_max),
  direction = "both",
  trace     = 1 )

# Modelo elegido
print(formula(modelo_elegido))

# Estimación del modelo logit con las variables seleccionadas sobre los datos de 
# entrenamiento
modelo_logit_step <- glm(
  ESTADO ~ ingreso_no_laboral + pampa + pisos + MAS_500 + cuyo + nor_o + edad + sec_com,
  data = datos_train,
  family = binomial(link = "logit"))

# Generación de tabla de resultados
resumen <- summary(modelo_logit_step)
tabla_s <- as.data.frame(round(resumen$coefficients,3)) |> select(-c("z value"))
tabla_s$cambio_odds<- c(round(exp(coef(modelo_logit_step)),3))
colnames(tabla_s) <- c("Coeficiente", "S.E", "P_Value", "Cambio en odds")
aic <- c(round(resumen$aic,3), "", "", "")
tabla_s <- rbind(tabla_s, "AIC" = aic)
print(tabla_s)
#write.xlsx(tabla_s, file = "Tabla3_3.xlsx", rowNames = TRUE)

# Predicción de probabilidades sobre los datos de testeo
probabilidades_step <- predict(modelo_logit_step, newdata = datos_test, type = "response")
# Clasificación según umbral de 0.04
prediccion_step <- ifelse(probabilidades_step > 0.04, 1, 0)

# Generación de matriz de confusión
matriz_step <- table(Predicción = prediccion_step, Real = datos_test$ESTADO)
print(matriz_step)

# Gráfico de la matriz
df_matriz_step <- as.data.frame(matriz_step)
ggplot(df_matriz_step, aes(x = Real, y = Predicción, fill = Freq)) +
  geom_tile(color = "beige") + 
  geom_text(aes(label = Freq), size = 10, fontface = "italic") +
  scale_fill_gradient(low = "palegreen",high = "limegreen") +
  labs(title = "Matriz de Estado predicho vs Estado Real \ncon umbral modificado",
       x = "Estado real", 
       y = "Estado predicho",
       caption = "Gráfico 3.8",
       fill = "Casos") +
  theme_minimal() + theme(
    plot.title      = element_text(face = "italic", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1.2))
#ggsave(filename = "gráfico3_8.png")

# Curva ROC
roc_step <- roc(datos_test$ESTADO, probabilidades_step, levels = c("0", "1"), direction = "<")

# Gráfico
#png("curva_roc_3.png")
plot(roc_step, main = "Curva ROC", print.auc = TRUE)
#dev.off()

# No funciona tan bien como la segunda estimación.

### Conclusión: la especificación con mejor capacidad predictiva fue la segunda,
### que incluía todas las variables del individuo y de la vivienda. 
### Para mayor detalle, dirigirse al informe adjunto

#-------------------------------------------------------------------------------