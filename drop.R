# BIOFREELANCER 2021
# con datos de: https://smn.conagua.gob.mx/es/climatologia/temperaturas-y-lluvias/resumenes-mensuales-de-temperaturas-y-lluvias 

## Instalar paquetes
install.packages("dplyr")
install.packages("vroom")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lubridate")

## Cargamos paquetes
library("dplyr")
library("vroom")
library("tidyr")
library("ggplot2")
library("lubridate")

# Cargamos datos
lluvias <- vroom( file = "https://data.biofreelancer.com/lluviasmx" )

# transformamos a formato largo
largo <- pivot_longer( data = lluvias,                   # a partir de los datos de lluvias
                       cols = -Entidad,                  # vamos a pivotear todas las columnas EXCEPTO (-) Entidad
                       names_to = "mes",                 # como se va a llamar la columna que guarde los meses?
                       values_to = "precipitacion_mm" )  # como se va a llamar la columna que guardara los valores de precipitacion

# Ponemos el mes en formato date
fix <- largo %>%                                # desde la data larga 
  mutate( mes = as.numeric( mes ),              # convertimos el mes a formato numero, para despues transformar a formato fecha
          fecha = month( x = mes,               # creamos una nueva columna
                         label = TRUE ) ) %>%   # la funcion month convierte un numero de 01 a 12 en su mes correspondiente
  filter( precipitacion_mm != 0 )               # eliminamos las observaciones donde no hubo lluvia

# Crear grafico de gotas
gotas1 <- ggplot( data = fix,                             # Partimos de la data fix
                  mapping = aes( x = fecha,               # en el eje x van los meses
                                 y = Entidad  ) ) +       # en el eje Y van los estados del pais
  geom_point( mapping = aes( size = precipitacion_mm ),   # La geometria de punto activa su propio aes para el tamanio del punto
              fill = "skyblue",                           # color azul
              shape = 21,                                 # Forma 21 de punto relleno
              alpha = 0.5 )                               # los puntos tienen una transparencia de 50% (0.5)

# Vis
gotas1

# Reorganizamos el eje Y
# Calculamos el promedio anual por estado
ordenados <- fix %>%                                     # a partir del fix
  group_by( Entidad ) %>%                                # agrupamos por estado
  summarize( promedio = mean( precipitacion_mm ) ) %>%   # calculamos el promedio por grupo (por estado)
  arrange( -promedio  )                                  # Ordenamos por la columna promedio en orden descendiente (-promedio)

# Sacamos el orden
el_orden <- ordenados %>%     
  pull( Entidad )          # Sacamos el vector de los nombres ordenados

gotas2 <- gotas1 +
  scale_y_discrete( limits = el_orden ) +                 # le pasamos el vector con los nombres de los estados en el orden que queremos
  labs( title = "Promedio mensual de lluvia en Mexico",   # ponemos titulo
        x = "2021",                                       # cambiamos nombre del eje x
        y = "",                                           # Borramos el nombre del eje y
        size = "Precipitacion (mm)" )                     # cambiamos el titulo de la leyenda

# Vis
gotas2

# Ajustamos temas
gotas3 <- gotas2 +                                        
  theme_classic( ) +                                      # usamos el tema clasico para rapido
  theme( axis.text.x = element_text( angle = 90 ) )       # giramos 90 grados los nombres de los meses

# Vis
gotas3

#
ggsave( filename = "drops.png",    # El nombre del archivo resultante
        plot = gotas3,             # guardamos la ultima version del plot
        width = 5,                 # ancho de 5 pulgadas
        height = 7,                # alto de 7 pulgadas
        dpi = 300 )                # resolucion de 300 puntos por pulgada

# FIN DEL EJERCICIO
# BIOFREELANCER 2021
