library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(DT)
library(ggrepel)
library(leaflet)
library(leaflet.extras)


# Cargamos los datos 
options(timeout = 600)
Sys.setlocale("LC_TIME", "C")
# LAcrime0 = fread("Crime_Data_from_2020_to_2024.csv")
LAcrime0 = fread("https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD")

# Procesamos los datos 
LAcrime0[, `Date Rptd` := as.IDate(lubridate::mdy_hms(`Date Rptd`))]
LAcrime0[, Fecha := as.IDate(lubridate::mdy_hms(`DATE OCC`))]
LAcrime0[, Diff_Date := as.numeric(`Date Rptd` - Fecha)]
LAcrime0[, `TIME OCC` := as.ITime(sprintf("%04d", `TIME OCC`), format = "%H%M")]
LAcrime0[, Año := factor(year(Fecha))]
LAcrime0[, Mes := factor(lubridate::month(Fecha, label=TRUE, abbr=FALSE))]
LAcrime0[, `Día del Mes` := mday(Fecha)]
LAcrime0[, `Día de la Semana` := lubridate::wday(Fecha, label = TRUE, abbr = FALSE, week_start = 1)]
LAcrime0[, LAT := as.numeric(LAT)]
LAcrime0[, LON := as.numeric(LON)]
LAcrime0[, Edad := `Vict Age`]
LAcrime0[, Género := factor(`Vict Sex`,
                               levels = c("M", "F", "X", "H", "-", ""), 
                               labels = c("Male", "Female", "Unknown", "NA", "NA", "NA"))]
LAcrime0[, Etnia := factor(`Vict Descent`,
                          levels = c("H", "", "W", "O", "Z",  
                                     "V", "B", "C", "J", "X", 
                                     "K", "F", "I", "A", "L", 
                                     "G", "P", "D", "U", "S", 
                                     "-"), 
                          labels = c("Hispanic", "NA", "White", "Other", "Asian Indian",
                                     "Vietnamese", "Black", "Chinese", "Japanese", "Unknown",
                                     "Korean", "Filipino", "American Indian", "Other Asian", "Laotian", 
                                     "Guamanian", "Pacific Islander", "Cambodian", "Hawaiian", "Samoan", 
                                     "NA"))]
LAcrime0[, Crímenes := `Crm Cd Desc`]
LAcrime0[, `Tipo de Crimen` := fcase(
  grepl("ASSAULT|BATTERY|ROBBERY|HOMICIDE|KIDNAPPING|THREATS", `Crm Cd Desc`), "Crimes Against Persons",
  grepl("THEFT|BURGLARY|VEHICLE|SHOPLIFTING|STOLEN|ROBO", `Crm Cd Desc`), "Property Crimes",
  grepl("SEX|RAPE|SODOMY|LEWD|INDECENT|EXPOSURE", `Crm Cd Desc`), "Sexual Offenses",
  grepl("CHILD|CHLD|MINOR|Pornography", `Crm Cd Desc`, ignore.case = TRUE), "Child Protection/Crimes",
  grepl("VANDALISM|ARSON|DAMAGE", `Crm Cd Desc`), "Vandalism & Property Damage",
  grepl("FORGERY|EMBEZZLEMENT|FRAUD|IDENTITY|COUNTERFEIT|BUNCO", `Crm Cd Desc`), "Financial Crimes",
  grepl("COURT ORDER|RESTRAINING|TRESPASSING|WEAPON|DRUNK|DUMPING|DISTURBING", `Crm Cd Desc`), 
  "Public Order & Judicial Violations",
  rep(TRUE, .N), "Other / Miscellaneous")]

LAcrime0[, c("DR_NO", "Date Rptd", "Vict Descent", "Vict Age", "Vict Sex", "Crm Cd Desc") := NULL]


# Definimos variables

fech_max = max(LAcrime0$Fecha, na.rm = TRUE)
fech_min = min(LAcrime0$Fecha, na.rm = TRUE)
edad_min <- min(LAcrime0$Edad, na.rm = TRUE)
edad_max <- max(LAcrime0$Edad, na.rm = TRUE)
choices_temp = c("Año","Mes", "Día de la Semana", "Día del Mes")

# Definimos unas paletas de colores con las que trabajar

color_31 <- c(
  "#264653", "#e76f51", "#2a9d8f", "#b56576", "#457b9d",
  "#666666", "#f4a261", "#66A61E", "#c1121f", "#6d597a", 
  "#CAB2D6", "#0096c7", "#1d3557", "#e9c46a", "#e63946", 
  "#81b29a", "#f2cc8f", "#3d405b", "#9d0208", "#bc6c25", 
  "#dda15e", "#606c38", "#283618", "#588157", "#3a5a40", 
  "#003049", "#d62828", "#f77f00", "#fcbf49", "#b8860b", 
  "#4a4e69")

color_8 <- c(
  "#F5FFBF", "#f4a261", "#9d0208", "#55FFFF",
  "#66A61E", "#666666", "#CAB2D6", "#E7298A")


# Definimos funciones

controles_estetica = function(id, v_label = 10, min_label = 5, max_label = 20) {
  tagList(
    if (id != "0"){hr()},
    selectInput(inputId = paste0('TipoPaleta', id),
                label = 'Tipo de Paleta:',
                choices = c("Predeterminada (Recomendada)", "Hue", "Viridis", 
                            "ColorBrewer", "Grises"),
                selected = "Predeterminada (Recomendada)"),
    uiOutput(paste0("selector_detalle_color", id)),
    fluidRow(
      column(4, sliderInput(paste0("size_label", id), "Etiqueta", 
                            value = v_label, min = min_label, max = max_label, step = 0.5)),
      column(4, sliderInput(paste0("size_axis", id), "Ejes", 
                            value = 12, min = 5, max = 20)),
      column(4, sliderInput(paste0("size_titles", id), "Títulos", 
                            value = 16, min = 10, max = 25))))
}

generar_selector_paleta = function(tipo_seleccionado, ns_id) {
  if (tipo_seleccionado == "Viridis") {
    selectInput(ns_id, "Variante Viridis:", 
                choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
                selected = "viridis")
  } else if (tipo_seleccionado == "ColorBrewer") {
    selectInput(ns_id, "Paleta Brewer:", 
                choices = rownames(RColorBrewer::brewer.pal.info),
                selected = "Paired")
  } else {NULL}
}

aplicar_color_custom = function(p, tipo, detalle, paleta_manual, estetica = "fill") {
  base = paste0("scale_", estetica, "_")
  if (tipo == "Hue") {
    p = p + get(paste0(base, "hue"))()
  } else if (tipo == "Grises") {
    p = p + get(paste0(base, "manual"))(values = rep("grey", 100))
  } else if (tipo == "Viridis" && !is.null(detalle)) {
    p = p + get(paste0(base, "viridis_d"))(option = detalle)
  } else if (tipo == "ColorBrewer" && !is.null(detalle)) {
    p = p + get(paste0(base, "brewer"))(palette = detalle)
  } else {
    p = p + get(paste0(base, "manual"))(values = paleta_manual)
  }
  return(p)
}

aplicar_estilo_texto = function(p, s_titles, s_axis, s_label, angulo = 0) {
  h_just = ifelse(angulo > 0, 1, 0.5)
  p = p + theme(
    plot.title = element_text(size = s_titles, face = "bold"),
    axis.title = element_text(size = s_titles, face = "bold"),
    axis.text.y = element_text(size = s_axis, face = "bold"),
    axis.text.x = element_text(size = s_axis, face = "bold", 
                               angle = angulo, hjust = h_just),
    strip.text = element_text(size = s_label, face = "bold")
  )
  return(p)
}


# Establecemos el UI

ui = navbarPage("Los Ángeles: Crímenes",
                
                tabPanel("Dataset",
                         fluidRow(
                           column(3, 
                                  h2("Práctica: Módulo 8", align = "center"),
                                  p(strong("Alumno:"), "Diego Zambrano Tipán", align = "center", style = "font-size: 20px;"),
                                  p(tags$small("Datos: Registro de crímenes en LA | Fuente: "), 
                                    a("Data LACity", 
                                      href="https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-2024/2nrs-mtv8/about_data", 
                                      target="_blank"), 
                                    align = "center"),
                                  hr(),
                                  checkboxInput("check0", "Descripción", value = FALSE)),
                           column(9,
                                  wellPanel(
                                    fluidRow(
                                      column(4, 
                                             dateRangeInput('dateRange', "Fecha (Filtro Global)", start = fech_min, end = fech_max),
                                             sliderInput("rango_edad", "Edad (Filtro Global)", min = edad_min, max = edad_max, 
                                                         value = c(edad_min, edad_max))),
                                      column(2, 
                                             pickerInput("filtro_genero", "Género (Filtro Global)", choices = unique(LAcrime0$Género), 
                                                         selected = unique(LAcrime0$Género), multiple = TRUE, 
                                                         options = list(`actions-box` = TRUE)),
                                             pickerInput("filtro_etnia", "Etnia (Filtro Global)", choices = unique(LAcrime0$Etnia), 
                                                         selected = unique(LAcrime0$Etnia), multiple = TRUE, 
                                                         options = list(`actions-box` = TRUE))),
                                      column(2, 
                                             pickerInput("filtro_crimen", "Crímenes  (Filtro Global)", choices = unique(LAcrime0$Crímenes), 
                                                         selected = unique(LAcrime0$Crímenes), multiple = TRUE, 
                                                         options = list(`actions-box` = TRUE)),
                                             pickerInput("filtro_tipoCrimen", "Tipo de Crimen (Filtro Global)", 
                                                         choices = unique(LAcrime0$`Tipo de Crimen`), 
                                                         selected = unique(LAcrime0$`Tipo de Crimen`), 
                                                         multiple = TRUE, options = list(`actions-box` = TRUE))),
                                      column(4, 
                                             controles_estetica("0")))))),
                         fluidRow(
                           column(6, 
                                  h4(strong("Resumen de Datos Filtrados")),
                                  DT::DTOutput("tabla_resumen_global")),
                           column(6,
                                  plotOutput(outputId = 'plot0', height = 680, , width = "100%"))),
                         fluidRow(
                           column(10, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check0 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("En este dataset encontramos información relativa a:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Información temporal (Fecha en la que se produce el crimen, fecha en la que se reporta…)"),
                                            tags$li("Información demográfica de la víctima (edad, sexo, origen étnico, …)"),
                                            tags$li("Información geográfica (coordenadas, …)"),
                                            tags$li("Tipo de delito, …")),
                                    p("Cuando hacemos una primera observacion de nuestros datos observamos que:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Hay un menor número de delitos cometidos en 2024"),
                                            tags$li("Se intuye que los registros totales de este año aún no están disponibles"),
                                            tags$li("Se recomienda filtrar los datos hasta la fecha: 2023-12-31"),
                                            tags$li("Al inicio de cada mes hay un pico en el número de delitos")))))),
                
                tabPanel("Número de Incidentes",
                         sidebarPanel(
                           selectInput(inputId = 'Agrupacion1', 'Agrupar por',
                                       choices = choices_temp,
                                       selected = "Mes"),
                           selectInput(inputId = 'Relleno1','Rellenar por',
                                       choices = choices_temp,
                                       selected = "Año"),
                           selectInput(inputId = 'Desglose1', 'Desglosar por',
                                       choices = c("Ninguno", "Año", "Mes", "Día de la Semana"),
                                       selected = "Ninguno"),
                           controles_estetica("1", v_label = 5, min_label = 1, max_label = 10),
                           checkboxInput("ver_porce", "Mostrar porcentajes", value = TRUE),
                           checkboxInput("rotar", "Rotar gráfico", value = FALSE),
                           checkboxInput("check1", "Descripción", value = FALSE)),
                         mainPanel(
                           plotOutput(outputId = 'plot1', width = 1200, height = 900)),
                         fluidRow(
                           column(10, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check1 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("En esta sección analizamos los datos en función del número total de incidentes, en las conclusiones que saquemos debemos tener en cuenta que no todos los meses tienen el mismo número de días"),
                                    p("Número de Incidencias Anuales (Agrupar por año):"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Se observa que en general ha habido un incremento de la delincuencia desde 2020 (Año de la Pandemia de Covid-19))"),
                                            tags$li("Si mantenemos 2024, observamos como el número de crímenes registrados es mucho menor")),
                                    p("Número de Incidencias Mensuales (Agrupar por mes):"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Según avanzan los meses en 2024 el porcentaje de delitos es menor, reforzando la hipótesis de que el registro total de delitos de este año aún no ha sido actualizado"),
                                            tags$li("En general se observa un aumento de la delincuencia en todos los meses con respecto al año 2020"),
                                            tags$li("Hay una menor incidencia de delitos en los meses de 2020 con respecto a Enero de ese mismo año")))))),
                
                tabPanel("Horas Pico",
                         sidebarPanel(
                           selectInput(inputId = 'Desglose3', 'Desglosar por',
                                       choices = c("Ninguno", "Año", "Mes", "Día de la Semana", "Tipo de Crimen")),
                           controles_estetica("3"),
                           checkboxInput("check3", "Descripción", value = FALSE)),
                         mainPanel(
                           plotOutput(outputId = 'plot3', width = 1200, height = 900)),
                         fluidRow(
                           column(10, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check3 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("Horas Pico:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Se observa que  la hora pico en la que se producen los delitos es a las 12 del mediodía"),
                                            tags$li("Si desglosamos por día de la semana, observamos que esta tendencia se mantiene"),
                                            tags$li("Si desglosamos por tipo de crimen, observamos que en general esta tendencia se mantiene")))))),
                
                tabPanel("Tasa de Resolución",
                         sidebarPanel(
                           selectInput(inputId = 'Agrupacion4', 'Agrupar por',
                                       choices = colnames(LAcrime0),
                                       selected = "Crímenes"),
                           selectInput(inputId = 'SubAgrupacion4','Subagrupar por',
                                       choices = colnames(LAcrime0),
                                       selected = "Tipo de Crimen"),
                           controles_estetica("4", v_label = 3, min_label = 2, max_label = 5),
                           checkboxInput("ver_etiquetas", "Mostrar etiquetas", value = FALSE),
                           checkboxInput("ver_logX", "Eje X: Escala logarítmica", value = FALSE),
                           checkboxInput("ver_logY", "Eje Y: Escala logarítmica", value = FALSE),
                           checkboxInput("check4", "Descripción", value = FALSE)),
                         mainPanel(
                           plotOutput(outputId = 'plot4', width = 1200, height = 900)),
                         fluidRow(
                           column(12, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check4 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("En esta sección representamos los tipos de delitos según el tiempo de demora en la denuncia y su tasa de resolución."),
                                    p("Si agrupamos por crímenes y subagrupamos por tipo de crimen:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Encontramos que los delitos contra la infancia y los delitos sexuales son los que más se tardan en reportar"),
                                            tags$li("Los crímenes contra la propiedad y de vandalismo son los que peor tasa de resolución tienen"),
                                            tags$li("La mayor proporción de tipo de delitos son los crímenes contra la propiedad y contra las personas")))))),
                
                tabPanel("Demografía Víctimas",
                         sidebarPanel(
                           selectInput(inputId = 'Agrupacion5', 'Agrupar por',
                                       choices = c("Crímenes", "Tipo de Crimen", "Género", "Etnia", "Edad")),
                           selectInput(inputId = 'Relleno5','Rellenar por',
                                       choices = c("Género", "Etnia", "Edad", "Tipo de Crimen", "Crímenes")),
                           sliderInput(inputId= 'TopN', "Top", value = 20, min = 1, max = 100, step = 1),
                           controles_estetica("5", v_label = 4, min_label = 2, max_label = 5),
                           tags$div(style = "display: flex; align-items: center;",
                                    tags$span("Mostrar porcentajes superiores a:",
                                              style = "margin-right: 10px; font-weight: bold;"),
                                    numericInput(inputId = "Filt_porc", label = NULL,
                                                 value = 0.95, min = 1, max = 100, width = '70px')),
                           checkboxInput("ver_porce5", "Mostrar etiquetas", value = TRUE),
                           checkboxInput("rotar5", "Rotar gráfico", value = FALSE),
                           checkboxInput("check5", "Descripción", value = FALSE)),
                         mainPanel(
                           plotOutput(outputId = 'plot5', width = 1200, height = 900)),
                         fluidRow(
                           column(10, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check5 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("Si atendemos al género de las víctimas nos encontramos que estas son principalemente hombres."),
                                    p("Si seleccionamos los 20 crimenes con mayor incidencia y calculamos su distribución en función del género, encontramos que:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("En el robo de vehículos no se tiene registro de la víctima, además de ser este el principal delito"),
                                            tags$li("El robo de objetos de moto presenta un alto número de víctimas sin información del género"),
                                            tags$li("En la intimidación a la pareja o la vioalción de órdenes de alejamiento la víctima es principalmente mujer")),
                                    p("Si atendemos al tipo de delito en función del género, nos encontramos que:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("El porcentaje de víctimas hombre es mayor en crímenes a la propiedad"),
                                            tags$li("En los delitos sexuales y de la infancia la mayor proporción de víctimas es mujer")),
                                    p("Si atendemos al origen étnico de las víctimas que:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Las víctimas son principalmente población hispana, seguida de población blanca y negra"),
                                            tags$li("Sin datos de la distribución étnica de Los Ángeles no podemos sacar más conclusiones")),
                                    p("Si seleccionamos los 20 crimenes con mayor incidencia y calculamos su distribución en función de la etnia, encontramos que:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Para el caso particular del robo de vehículos, tenemos que no se tienen registros del origen étnico de la víctima"),
                                            tags$li("Tampoco se tienen registros del origen étnico para un porcentaje importante de los robos de objetos de moto"),
                                            tags$li("Hay una mayor proporción de población hispana que sufre intimidación por la pareja")),
                                    p("Cuando antendemos al tipo de delito, observamos que hay un gran porcentaje de población hispana que es víctima de crímenes contra la persona."))))),
                
                tabPanel("Pirámide Demográfica",
                         sidebarPanel(
                           selectInput(inputId = 'Agrupacion6', 'Agrupar por',
                                       choices = c("Crímenes", "Tipo de Crimen", "Género", "Etnia", "Edad"),
                                       selected = "Edad"),
                           selectInput(inputId = 'Relleno6','Rellenar por',
                                       choices = c("Género", "Etnia", "Edad", "Tipo de Crimen", "Crímenes"),
                                       selected = "Etnia"),
                           sliderInput(inputId= 'TopN6', "Top ", value = 81, min = 1, max = 120, step = 1),
                           controles_estetica("6", v_label = 4, min_label = 2, max_label = 5),
                           checkboxInput("check6", "Descripción", value = FALSE)),
                         mainPanel(
                           plotOutput(outputId = 'plot6', width = 1200, height = 900)),
                         fluidRow(
                           column(10, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check6 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("Los datos de víctimas de las que se desconocen el sexo o la edad han sido descartados."),
                                    p("Pirámide demográfica en función de la edad, el sexo y la etnia:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Hay 2 datos atípicos: varones de 35 años de etnia desconocida y varones blancos de 50 años"),
                                            tags$li("Probablemente esta sobrerrepresentación corresponda a perfiles genéricos para víctimas")))))),
                
                tabPanel("Estadística",
                         sidebarPanel(
                           selectInput(inputId = 'Evaluacion2', 'Evaluar por',
                                       choices = c("Número de Incidentes", "Edad"),
                                       selected = "Número de Incidentes"),
                           selectInput(inputId = 'Agrupacion2', 'Agrupar por',
                                       choices = c(choices_temp, "Género", "Etnia"),
                                       selected = "Día de la Semana"),
                           selectInput(inputId = 'Desglose', 'Desglosar por',
                                       choices = c("Ninguno", "Año", "Mes", "Día de la Semana", "Género", "Etnia")),
                           controles_estetica("2"),
                           selectInput(inputId = 'scales2', 'Escalas',
                                       choices = c("Fijas" = "fixed", "Y = Independiente" = "free_y", "X = Independiente" = "free_x", "Independientes" = "free"),
                                       selected = "fixed"),
                           checkboxInput("violin", "Estilo Violin", value = FALSE),
                           checkboxInput("check2", "Descripción", value = FALSE)),
                         mainPanel(
                           plotOutput(outputId = 'plot2', width = 1200, height = 900)),
                         fluidRow(
                           column(10, offset = 1,
                                  conditionalPanel(
                                    condition = "input.check2 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("Distribución de los crímenes por día de la semana (Agrupar por día de la semana):"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Se observa que los Viernes y los Sábados son días de gran incidencia."),
                                            tags$li("Curiosamente si desglosamos por año, observamos que en 2021 en los Lunes también hay una alta incidencia")),
                                    p("Distribución de los crímenes mensuales (Agrupar por mes):"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("En general los meses de Enero y Febrero y en los meses de Verano y Otoño son en los que se produce un mayor número de delitos")),
                                    p("Si hacemos la evaluación por edad:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("En general los hombres que son víctimas presentan una edad superior que las mujeres")),
                                    
                                  )))),
                
                tabPanel("Mapa de Incidencias",
                         sidebarPanel(
                           pickerInput("filtro_area", "Seleccionar Áreas", choices = unique(LAcrime0$`AREA NAME`),
                                       selected = unique(LAcrime0$`AREA NAME`), multiple = TRUE,
                                       options = list(`actions-box` = TRUE)),
                           pickerInput("filtro_crimen7", "Seleccionar Crímenes", choices = unique(LAcrime0$Crímenes),
                                       selected = unique(LAcrime0$Crímenes), multiple = TRUE,
                                       options = list(`actions-box` = TRUE)),
                           pickerInput("filtro_tipoCrimen7", "Seleccionar Tipo de Crimen", choices = unique(LAcrime0$`Tipo de Crimen`),
                                       selected = unique(LAcrime0$`Tipo de Crimen`), multiple = TRUE,
                                       options = list(`actions-box` = TRUE)),
                           sliderInput("heat_max", "Sensibilidad (Max):", min = 0.1, max = 10, value = 5, step = 0.1),
                           sliderInput("heat_radius", "Radio del punto:", min = 1, max = 30, value = 9),
                           sliderInput("heat_blur", "Suavizado (Blur):", min = 1, max = 50, value = 5),
                           checkboxInput("check7", "Descripción", value = FALSE)),
                         mainPanel(
                           leafletOutput("mapa_crimenes", width = 1200, height = 900)),
                         fluidRow(
                           column(10, offset = 1, 
                                  conditionalPanel(
                                    condition = "input.check7 == true",
                                    h2("Descripción",
                                       style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                    p("Distribución geográfica según los tipos de delito:"),
                                    tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                            tags$li("Los delitos se concentran principalmente en las áreas de Central, Newton y Hollywood"),
                                            tags$li("Al filtrar los tipos de delito desde Dataset no se observan diferencias en los puntos calientes")))))),
                
                tabPanel("Conclusiones",
                         fluidRow(
                           column(10, offset = 1, 
                                  h2("Conclusiones del Análisis", 
                                     style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                  p("El análisis del dataset de los delitos llevados a cabo en Los Ángeles nos permite determinar perfiles",
                                    br(), "demográficos de las víctimas, momentos de más riesgo, así como zonas calientes de riesgo.", 
                                    style = "font-size: 16px; margin-top: 20px;"),
                                  h3(strong("Hallazgos principales:")),
                                  tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                          tags$li("Ha habido un aumento en los delitos desde el año 2020"),
                                          tags$li("Los Meses de Verano e Inicios de Otoño son los de mayor criminalidad"),
                                          tags$li("Al inicio de cada mes es cuando más delitos se producen"),
                                          tags$li("En los días Viernes se producen más delitos"),
                                          tags$li("La hora pico de incidencia delictiva es las 12 del mediodía"),
                                          tags$li("Los delitos sexuales son los que más tardan en reportarse"),
                                          tags$li("En algunos delitos de robo existe una correlación negativa entre la demora en el reporte y su resolución"),
                                          tags$li("Las principales víctimas son las mujeres"),
                                          tags$li("En los delitos sexuales hay una mayor proporción de víctimas mujeres"),
                                          tags$li("El delito más frecuente es el robo de vehículos"),
                                          tags$li("El mayor número de víctimas es de población hispana"),
                                          tags$li("Hay un aumento en la proporción de hispanos víctimas de crímenes contra las personas"),
                                          tags$li("El perfil de edad de las víctimas mujeres es inferior al de hombres"),
                                          tags$li("El perfil de edad de las víctimas de la población hispana es inferior al resto"),
                                          tags$li("Los delitos se concentran en las Áreas Central, Newton y Hollywood")),
                                  h3(strong("Limitaciones del análisis:")),
                                  tags$ul(
                                    style = "font-size: 15px; line-height: 1.8;",
                                    tags$li("Los datos de delitos del año 2024 aún no han sido actualizados"),
                                    tags$li("Faltan datos de la demografía de Los Ángeles para hacer un análisis más completo"),
                                    tags$li("En los delitos de robo de vehículos no hay información demográfica de la víctima")),
                                  h3(strong("Próximos pasos:")),
                                  tags$ul(
                                    style = "font-size: 15px; line-height: 1.8;",
                                    tags$li("Analizar el resto de variables, en particular la variable Premis Desc")))))
                
)

# Establecemos el server

server = function(input, output, session) {
  
  LAcrime1 = reactive({
    d = LAcrime0[Fecha >= input$dateRange[1] & Fecha <= input$dateRange[2] &
                   Etnia %in% input$filtro_etnia &
                   Género %in% input$filtro_genero &
                   Edad >= input$rango_edad[1] & Edad <= input$rango_edad[2] &
                   Crímenes %in% input$filtro_crimen &
                   `Tipo de Crimen` %in% input$filtro_tipoCrimen]
    return(d)})
  
  output$selector_detalle_color0 = renderUI({
    generar_selector_paleta(input$TipoPaleta0, "PaletaFinal0")})
  output$selector_detalle_color1 = renderUI({
    generar_selector_paleta(input$TipoPaleta1, "PaletaFinal1")})
  output$selector_detalle_color2 = renderUI({
    generar_selector_paleta(input$TipoPaleta2, "PaletaFinal2")})
  output$selector_detalle_color3 = renderUI({
    generar_selector_paleta(input$TipoPaleta3, "PaletaFinal3")})
  output$selector_detalle_color4 = renderUI({
    generar_selector_paleta(input$TipoPaleta4, "PaletaFinal4")})
  output$selector_detalle_color5 = renderUI({
    generar_selector_paleta(input$TipoPaleta5, "PaletaFinal5")})
  output$selector_detalle_color6 = renderUI({
    generar_selector_paleta(input$TipoPaleta6, "PaletaFinal6")})
  
  output$tabla_resumen_global = DT::renderDT({
    DT::datatable(LAcrime1(), 
                  options = list(pageLength = 4, scrollX = TRUE, dom = 'ltip'),
                  selection = 'none')})  
  
  output$plot0 <- renderPlot({
    LAcrime = LAcrime1()
    Distr_Inc0 = LAcrime[, .(Total = .N), by = .(Fecha, `Día del Mes`, `Día de la Semana`, Mes, Año)]
    p0 = ggplot(Distr_Inc0, aes(x = Fecha, y = Total)) +
      geom_point(aes(colour = Año), alpha = 1, size = 1) +
      facet_wrap(~ Año, scales = "free", ncol = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Registro de Crímenes en el dataset", x = "", y = "Número de Incidentes") +
      theme_bw() +
      theme(legend.position = "none")
    p0 = aplicar_estilo_texto(p0, input$size_titles0, input$size_axis0, input$size_label0)
    p0 = aplicar_color_custom(p0, input$TipoPaleta0, input$PaletaFinal0, color_31, "color")    
    print(p0)
  })  
  
  output$plot1 <- renderPlot({
    LAcrime = LAcrime1()
    Distr_Inc1 = LAcrime[, .(Total = .N), by = .(`Día del Mes`, `Día de la Semana`, Mes, Año)]
    p1 = ggplot(Distr_Inc1, aes(x = Total, y = factor(get(input$Agrupacion1)),
                                fill = factor(get(input$Relleno1)))) +
      geom_col(position = position_stack(reverse = TRUE)) +
      labs(title = "Distribución Total de Incidentes",
           x = "Número of Incidentes", y = input$Agrupacion1, fill = input$Relleno1) +
      theme_minimal()
    if (input$Desglose1 != "Ninguno") {
      p1 = p1 + facet_wrap(~ get(input$Desglose1), scales = "free_y")}
    if (input$ver_porce) {
      p1 = p1 + stat_summary(
        fun = sum, geom = "text",
        aes(label = scales::percent(after_stat(x) / sum(Distr_Inc1$Total), accuracy = 0.01)),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        color = "white", fontface = "bold", size = input$size_label1)}
    if (input$rotar) {
      p1 = p1 + coord_flip()
      p1 = aplicar_estilo_texto(p1, input$size_titles1, input$size_axis1, input$size_label1, 45)
    } else {p1 = aplicar_estilo_texto(p1, input$size_titles1, input$size_axis1, input$size_label1)}
    p1 = aplicar_color_custom(p1, input$TipoPaleta1, input$PaletaFinal1, color_31)
    
    tryCatch({
      print(p1)
    }, error = function(e) {
      message("Error detectado en escala manual, forzando escala Hue...")
      base = "scale_fill_"
      print(p1 + scale_fill_hue())
    })
  })
  
  output$plot3 <- renderPlot({
    LAcrime = LAcrime1()
    Distr_Inc3 = LAcrime
    if (input$Desglose3 != "Ninguno") {
      p3 = ggplot(Distr_Inc3, aes(x = as.numeric(`TIME OCC`) / 3600, fill = factor(get(input$Desglose3))))
    } else {p3 = ggplot(Distr_Inc3, aes(x = as.numeric(`TIME OCC`) / 3600, fill = "Grey"))}
    p3 = p3 + geom_density(alpha = 0.5, color = "darkblue") +
      scale_x_continuous(breaks = seq(0, 24, by = 2)) +
      labs(title = "Distribución Diaria de Incidentes", x = "", y = "Número de Incidentes") +
      theme_bw() +
      theme(legend.position = "none")
    if (input$Desglose3 != "Ninguno") {
      p3 = p3 + facet_wrap(~ get(input$Desglose3))}
    p3 = aplicar_estilo_texto(p3, input$size_titles3, input$size_axis3, input$size_label3)
    p3 = aplicar_color_custom(p3, input$TipoPaleta3, input$PaletaFinal3, color_31)
    
    tryCatch({
      print(p3)
    }, error = function(e) {
      message("Error detectado en escala manual, forzando escala Hue...")
      base = "scale_fill_"
      print(p3 + scale_fill_hue())
    })
  })
  
  output$plot4 <- renderPlot({
    LAcrime = LAcrime1()
    Distr_Inc4_1 = LAcrime
    Distr_Inc4_2 = Distr_Inc4_1[, .(AvgRetraso = mean(Diff_Date, na.rm = TRUE),
                                    Resueltos = mean(Status != "IC", na.rm = TRUE),
                                    Total = .N), by = c(input$Agrupacion4, input$SubAgrupacion4)
    ][Total > 100][order(-AvgRetraso)]
    if(input$ver_logX) {
      var_X4 = log10(Distr_Inc4_2$AvgRetraso)
    } else {var_X4 = Distr_Inc4_2$AvgRetraso}
    if(input$ver_logY) {
      var_Y4 = log10(Distr_Inc4_2$Resueltos)
    } else {var_Y4 = Distr_Inc4_2$Resueltos}
    p4 = ggplot(Distr_Inc4_2, aes(x = var_X4, y = var_Y4, size = Total, fill = factor(get(input$SubAgrupacion4)))) +
      geom_point(alpha = 1, shape = 21, color = "black", stroke = 0.5) +
      scale_size(range = c(2, 25)) +
      theme_minimal() +
      labs(title = "Análisis de la Resolución de Casos ", x = "Tiempo transcurrido hasta el reporte", y = "Tasa de Resolución",
           size = "Incidentes Totales", fill = input$SubAgrupacion4) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 5))) +
      theme(legend.position = "bottom")
    if (input$ver_etiquetas) {
      p4 = p4 + geom_text_repel(aes(label = get(input$Agrupacion4)), size = input$size_label4)}
    p4 = aplicar_estilo_texto(p4, input$size_titles4, input$size_axis4, input$size_label4)
    p4 = aplicar_color_custom(p4, input$TipoPaleta4, input$PaletaFinal4, color_8)
    
    tryCatch({
      print(p4)
    }, error = function(e) {
      message("Error detectado en escala manual, forzando escala Hue...")
      base = "scale_fill_"
      print(p4 + scale_fill_hue())
    })
  })
  
  output$plot5 <- renderPlot({
    LAcrime = LAcrime1()
    num_TopN = as.integer(input$TopN)
    num_porc= as.numeric(input$Filt_porc)
    top = LAcrime[, .N, by = c(input$Agrupacion5)][order(-N)][1:num_TopN, get(input$Agrupacion5)]
    Distr_Inc5 = LAcrime[Fecha >= input$dateRange[1] & Fecha <= input$dateRange[2],
                         .(Total = .N), by = c(input$Agrupacion5, input$Relleno5)
    ][get(input$Agrupacion5) %in% top]
    p5 = ggplot(Distr_Inc5, aes(x = Total, y = reorder(get(input$Agrupacion5), Total, FUN = sum),
                                fill = factor(get(input$Relleno5))))
    p5 = p5 + geom_col(position = position_stack(reverse = FALSE)) +
      labs(title = "Top Incidentes",
           x = "Número de Incidentes", y = input$Agrupacion5, fill = input$Relleno5) +
      theme_minimal()
    if (input$rotar5) {
      p5 = p5 + coord_flip()
      p5 = aplicar_estilo_texto(p5, input$size_titles5, input$size_axis5, input$size_label5, 45)
    } else {p5 = aplicar_estilo_texto(p5, input$size_titles5, input$size_axis5, input$size_label5)}
    if (input$ver_porce5) {
      p5 = p5 + stat_summary(
        fun = sum, geom = "text",
        aes(label = ifelse((after_stat(x) / sum(Distr_Inc5$Total)) * 100 > num_porc,
                           scales::percent(after_stat(x) / sum(Distr_Inc5$Total), accuracy = 0.01), "")),
        position = position_stack(vjust = 0.5, reverse = FALSE),
        color = "white", fontface = "bold", size = input$size_label5)}
    p5 = aplicar_color_custom(p5, input$TipoPaleta5, input$PaletaFinal5, color_31)
    
    tryCatch({
      print(p5)
    }, error = function(e) {
      message("Error detectado en escala manual, forzando escala Hue...")
      base = "scale_fill_"
      print(p5 + scale_fill_hue())
    })
  })
  
  output$plot6 <- renderPlot({
    LAcrime = LAcrime1()
    num_TopN6 = as.integer(input$TopN6)
    top6 = LAcrime[Edad > 0 & Género %in% c("Male", "Female"), .N,
                   by = c(input$Agrupacion6)][order(-N)][1:num_TopN6, get(input$Agrupacion6)]
    Distr_Inc6 = LAcrime[Fecha >= input$dateRange[1] & Fecha <= input$dateRange[2] &
                           Edad > 0 & Género %in% c("Male", "Female"), .(Total = .N),
                         by = .(Edad, Género, Etnia, Crímenes, `Tipo de Crimen`)
    ][Género == "Male", Total := Total * -1][get(input$Agrupacion6) %in% top6]
    if(input$Agrupacion6 == "Edad") {
      p6 = ggplot(Distr_Inc6, aes(x = get(input$Agrupacion6), y = Total, fill = factor(get(input$Relleno6)))) +
        scale_x_continuous(breaks = seq(0, 100, by = 5))
    } else {p6 = ggplot(Distr_Inc6, aes(x = reorder(get(input$Agrupacion6), -abs(Total), FUN = sum),
                                        y = Total, fill = factor(get(input$Relleno6))))}
    p6 = p6 + geom_col(position = "stack") +
      coord_flip() +
      scale_y_continuous(labels = abs) +
      theme_minimal() +
      labs(title = "", x = input$Agrupacion6, y = "Número de Incidentes", fill = "Victim Descent") +
      annotate("text", x = Inf, y = -Inf, label = "MALE", hjust = "inward", vjust = "inward",
               fontface = "bold", size = 6) +
      annotate("text", x = Inf, y = Inf, label = "FEMALE", hjust = "inward", vjust = "inward",
               fontface = "bold", size = 6) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(legend.position = "bottom")
    p6 = aplicar_estilo_texto(p6, input$size_titles6, input$size_axis6, input$size_label6)
    p6 = aplicar_color_custom(p6, input$TipoPaleta6, input$PaletaFinal6, color_31)
    
    tryCatch({
      print(p6)
    }, error = function(e) {
      message("Error detectado en escala manual, forzando escala Hue...")
      base = "scale_fill_"
      print(p6 + scale_fill_hue())
    })
  })
  
  output$plot2 <- renderPlot({
    LAcrime = LAcrime1()
    if (input$Evaluacion2 == "Edad") {
      Distr_Inc2 = LAcrime
    } else{
      if (input$Desglose != "Ninguno") {
        Distr_Inc2 = LAcrime[, .(`Número de Incidentes` = .N), by = c("Fecha", input$Agrupacion2, input$Desglose)]
      } else {
        Distr_Inc2 = LAcrime[, .(`Número de Incidentes` = .N), by = c("Fecha", input$Agrupacion2)]}}
    p2 = ggplot(Distr_Inc2, aes(x = factor(get(input$Agrupacion2)), y =get(input$Evaluacion2)))
    if (input$violin) {
      p2 = p2 + geom_violin(aes(fill = factor(get(input$Agrupacion2)), alpha = 1)) + geom_boxplot(width = 0.07)
    } else {p2 = p2 + geom_boxplot(aes(fill = factor(get(input$Agrupacion2)), alpha = 1))}
    p2 = p2 + labs(title = "Distribuciones estadísticas", x = "", y = input$Evaluacion2) +
      theme_bw() +
      theme(legend.position = "none")
    if (input$Desglose != "Ninguno") {
      p2 = p2 + facet_wrap(~ get(input$Desglose), scales = input$scales2)}
    p2 = aplicar_estilo_texto(p2, input$size_titles2, input$size_axis2, input$size_label2, 45)
    p2 = aplicar_color_custom(p2, input$TipoPaleta2, input$PaletaFinal2, color_31)
    
    tryCatch({
      print(p2)
    }, error = function(e) {
      message("Error detectado en escala manual, forzando escala Hue...")
      base = "scale_fill_"
      print(p2 + scale_fill_hue())
    })
  })  
  
  output$mapa_crimenes <- renderLeaflet({
    datos_mapa0 <- LAcrime1()
    datos_mapa = datos_mapa0[!is.na(LAT) & LAT > 33.6 & LAT < 34.4 &
                               !is.na(LON) & LON < -117.5 & LON > -118.7 &
                               `AREA NAME` %in% input$filtro_area &
                               Crímenes %in% input$filtro_crimen7 &
                               `Tipo de Crimen` %in% input$filtro_tipoCrimen7]
    leaflet(datos_mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addHeatmap(lng = ~LON, lat = ~LAT, blur = input$heat_blur,
                 max = input$heat_max, radius = input$heat_radius)})
  
}

shinyApp(ui, server)