# Funciones para creación de gráficas en ggplot2 con formato oficial del
# Instituto Mexicano del Seguro Social

# Arturo Andrés Morales Barrios
# Febrero 2022

# Descarga de la fuente oficial del IMSS
require(showtext)
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

# Códigos de colores del IMSS
verde_oscuro <- "#13322B"
gris <- "#404041"
dorado <- "#B38E5D"

# Definicón del tema del IMSS
require(ggplot2)
tema_imss <- theme(plot.title = element_text(size = 18),
                   axis.title = element_text(size = 12),
                   text = element_text(family = "Montserrat"),
                   panel.grid.major.y = element_line(size = 0.2,
                                                     color = gris),
                   panel.background = element_blank())

# Cálculo de totales y porcentajes para variables categóricas
require(scales)
totales_porcentajes <- function(datos,
                                x,
                                porcentaje = FALSE){
  x <- enquo(x)
  datos_conteo <- datos %>% 
                    group_by(!!x) %>%
                    count(name = "Total")
  if(porcentaje == TRUE){
    return(datos_conteo %>% 
            mutate(Porcentaje = Total/nrow(datos),
                   Porcentaje_Etiqueta = scales::percent(Porcentaje)))
  }
  return(datos_conteo)
}

# Histograma para la distribución de una variable numérica
grafimss_histogram <- function(datos, 
                               x, 
                               titulo = NULL, 
                               etiqueta_x = NULL, 
                               bins = NULL,
                               binwidth = NULL, ...){
  x <- enquo(x)
  ggplot(datos, aes(!!x)) +
    geom_histogram(binwidth = binwidth,
                   bins = bins,
                   fill = verde_oscuro) +
    labs(x = etiqueta_x, 
         y = "Frecuencia", 
         title = titulo) +
    tema_imss
}

# Gráfica de columnas para una variable categórica
require(dplyr)
grafimss_col <- function(datos,
                         x,
                         titulo = NULL,
                         etiqueta_x = NULL, 
                         porcentaje = FALSE, ...){
  x <- enquo(x)
  # Cálculo de totales y porcentajes
  datos_totales_porcentajes <- totales_porcentajes(datos,
                                                   !!x,
                                                   porcentaje = porcentaje)
  
  graf_col <- ggplot(datos_totales_porcentajes, aes(!!x, Total, label = Total))
  
  if(porcentaje == TRUE){
    graf_col <- ggplot(datos_totales_porcentajes, aes(!!x, Porcentaje, label = Porcentaje_Etiqueta))
  }
  
  graf_col <- graf_col +  
                geom_col(fill = verde_oscuro,
                         width = 0.75) +
                geom_text(vjust = -0.2) +
                labs(x = etiqueta_x,
                     title = titulo) +
                tema_imss
  
  if(porcentaje == TRUE){
    return(graf_col + scale_y_continuous(labels = scales::percent))
  }
  
  return(graf_col)
}

# Gráfica de pastel
grafimss_pastel <- function(){
  
}