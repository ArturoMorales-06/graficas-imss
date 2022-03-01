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
tema_imss <- theme(plot.title = element_text(size = 18, hjust = 0.5),
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

# Cálculo de posiciones para las etiquetas de la gráfica de pastel
posiciones_pastel <- function(datos_totales_porcentajes){
  datos_totales_porcentajes %>%
    arrange(Porcentaje) %>% 
    ungroup() %>% 
    mutate(Pos_Y = cumsum(Porcentaje) - 0.5*Porcentaje)
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
    scale_y_continuous(expand = c(0,0)) + 
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
  
  graf_col <- ggplot(datos_totales_porcentajes, 
                     aes(!!x, Total, label = Total))
  
  if(porcentaje == TRUE){
    graf_col <- ggplot(datos_totales_porcentajes, 
                       aes(!!x, Porcentaje, label = Porcentaje_Etiqueta))
  }
  
  graf_col <- graf_col +  
                geom_col(fill = verde_oscuro,
                         width = 0.75) +
                geom_text(vjust = -0.2,
                          family = "Montserrat") +
                labs(x = etiqueta_x,
                     title = titulo) +
                tema_imss
  
  if(porcentaje == TRUE){
    return(graf_col + scale_y_continuous(labels = scales::percent, 
                                         expand = expansion(mult = c(0, .1))))
  }
  
  return(graf_col + scale_y_continuous(expand = expansion(mult = c(0, .2))))
}

# Gráfica de pastel para variables binarias
grafimss_pastel_bin <- function(datos,
                                x,
                                titulo = NULL,
                                etiqueta_color = NULL, ...){
  x <- enquo(x)
  # Cálculo de porcentajes
  datos_porcentajes <- totales_porcentajes(datos, 
                                           !!x,
                                           porcentaje = TRUE)
  # Cálculo de posiciones para las etiquetas
  datos_porcentajes_pos <- posiciones_pastel(datos_porcentajes)
  
  # Gráfica de pastel
  ggplot(datos_porcentajes_pos, aes(x = "", y = Porcentaje, fill = !!x)) +
    geom_bar(width = 1, stat = "identity") +
    scale_fill_manual(values = c(verde_oscuro, dorado)) +
    # HACER UN DEGRADADO ENTRE EL VERDE Y EL DORADO
    # Coordenadas polares
    coord_polar("y", start = 0) +
    labs(title = titulo,
         fill = etiqueta_color) +
    # Etiquetas textuales sobre la gráfica
    geom_text(aes(y = Pos_Y, label = Porcentaje_Etiqueta),
              family = "Montserrat",
              color = "white", 
              size = 6) +
    tema_imss +
    # Tema para gráfica de pastel
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks  = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank())
}
