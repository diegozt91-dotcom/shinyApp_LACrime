Análisis de Crímenes en la ciudad de Los Ángeles
================
Autor: Diego Zambrano Tipán

- [—](#section)
- [title: “Análisis de Crímenes en la ciudad de Los
  Ángeles”](#title-análisis-de-crímenes-en-la-ciudad-de-los-ángeles)
- [author: “Autor: Diego Zambrano
  Tipán”](#author-autor-diego-zambrano-tipán)
- [output:](#output)
- [html_document:](#html_document)
- [toc: true](#toc-true)
- [toc_float: true](#toc_float-true)
- [toc_depth: 3](#toc_depth-3)
- [code_folding: hide](#code_folding-hide)
- [theme: cosmo](#theme-cosmo)
- [highlight: tango](#highlight-tango)
- [df_print: paged](#df_print-paged)
- [self_contained: true](#self_contained-true)
- [mathjax:
  “https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js”](#mathjax-httpscdnjsdelivrnetnpmmathjax3es5tex-mml-chtmljs)
- [—](#section-1)

# —

# title: “Análisis de Crímenes en la ciudad de Los Ángeles”

# author: “Autor: Diego Zambrano Tipán”

# output:

# html_document:

# toc: true

# toc_float: true

# toc_depth: 3

# code_folding: hide

# theme: cosmo

# highlight: tango

# df_print: paged

# self_contained: true

# mathjax: “<https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js>”

# —

<style>
body {
  text-align: justify;
}
</style>

<h1 style="color: #006400;">

1.  Introducción al Dataset

    </h1>

Primeramente en esta sección procederemos a cargar las librerías del
entorno de `R` con las que trabajaremos, importaremos el dataset
proporcionado por la `UNED` y procederemos a su posterior tratamiento.

<h2 style="color: #228B22;">

1.1 Carga de Librerías

</h2>

En este apartado cargaremos las principales librerías con las que
realizaremos el análisis, estas librerías han sido seleccionadas
siguiendo los lineamientos de la práctica.

``` r
library(data.table)
library(tidytable) 
library(ggplot2)
library(ggeasy)
library(forcats)
library(lubridate)
library(knitr)
library(broom)
```

<h2 style="color: #228B22;">

1.2 Importación del dataset

</h2>

Lo primero que hacemos es importar el archivo
`Crime_Data_from_2020_to_2024_20260309.csv` aportado por la UNED.
Verificamos si el formato de las columnas es el más idóneo. En este
dataset encontramos información relativa a:

- Información temporal (Fecha en la que se produce el crimen, fecha en
  la que se reporta…)
- Información demográfica de la víctima (edad, sexo, origen étnico, …)
- Información geográfica (coordenadas, …)
- Tipo de delito, …

``` r
data6 = fread(paste0(path, "Crime_Data_from_2020_to_2024_20260309.csv"))
head(data6)
```

    ## # A tidytable: 6 × 28
    ##       DR_NO `Date Rptd`    `DATE OCC` `TIME OCC`  AREA `AREA NAME` `Rpt Dist No`
    ##       <int> <chr>          <chr>           <int> <int> <chr>               <int>
    ## 1 211507896 2021 Apr 11 1… 2020 Nov …        845    15 N Hollywood          1502
    ## 2 201516622 2020 Oct 21 1… 2020 Oct …       1845    15 N Hollywood          1521
    ## 3 240913563 2024 Dec 10 1… 2020 Oct …       1240     9 Van Nuys              933
    ## 4 210704711 2020 Dec 24 1… 2020 Dec …       1310     7 Wilshire              782
    ## 5 201418201 2020 Oct 03 1… 2020 Sep …       1830    14 Pacific              1454
    ## 6 240412063 2024 Dec 11 1… 2020 Nov …       1210     4 Hollenbeck            429
    ## # ℹ 21 more variables: `Part 1-2` <int>, `Crm Cd` <int>, `Crm Cd Desc` <chr>,
    ## #   Mocodes <chr>, `Vict Age` <int>, `Vict Sex` <chr>, `Vict Descent` <chr>,
    ## #   `Premis Cd` <int>, `Premis Desc` <chr>, `Weapon Used Cd` <int>,
    ## #   `Weapon Desc` <chr>, Status <chr>, `Status Desc` <chr>, `Crm Cd 1` <int>,
    ## #   `Crm Cd 2` <int>, `Crm Cd 3` <int>, `Crm Cd 4` <int>, LOCATION <chr>,
    ## #   `Cross Street` <chr>, LAT <chr>, LON <chr>

<h2 style="color: #228B22;">

1.3 Preparación de los Datos

</h2>

El tratamiento que damos a los datos es el siguiente:

- Cambio de formato de las variables Date Rpd, Date OCC, TIME OCC, LAT y
  LON
- Factorización de las variables: Vict Sex y Vict Descend
- Creación de nuevas variables: Diff_Date, Año_OCC, Mes_OCC, Dia_OCC y
  Crm_Group

``` r
data6[, `Date Rptd` := as.IDate(`Date Rptd`, format = "%Y %b %d")]
data6[, `DATE OCC` := as.IDate(`DATE OCC`, format = "%Y %b %d")]
data6[, Diff_Date := as.numeric(`Date Rptd` - `DATE OCC`)]
data6[, `TIME OCC` := as.ITime(sprintf("%04d", `TIME OCC`), format = "%H%M")]
data6[, Año_OCC := factor(year(`DATE OCC`))]
data6[, Mes_OCC := factor(lubridate::month(`DATE OCC`, label=TRUE, abbr=FALSE))]
data6[, Dia_OCC := mday(`DATE OCC`)]
data6[, Dia_Semana := lubridate::wday(`DATE OCC`, label = TRUE, abbr = FALSE, week_start = 1)]
data6[, LAT := as.numeric(gsub(",", ".", LAT))]
data6[, LON := as.numeric(gsub(",", ".", LON))]
data6[, `Vict Sex` := factor(`Vict Sex`, 
                             levels = c("M", "F", "X", "H", "-", ""), 
                             labels = c("Male", "Female", "Unknown", "NA", "NA", "NA"))]
data6[, `Vict Descent` := factor(`Vict Descent`, 
                             levels = c("H", "W", "A", "B", "",  "X", "O", "C", "J", "V", "K", "F", "I", "Z",
                                        "L", "G", "P", "D", "U", "S", "-"), 
                             labels = c("Hispanic", "White", "Other Asian", "Black", "NA",  "Unknown", "Other",
                                        "Chinese", "Japanese", "Vietnamese", "Korean", "Filipino", 
                                        "American Indian", "Asian Indian", "Laotian", "Guamanian", 
                                        "Pacific Islander", "Cambodian", "Hawaiian", "Samoan", "NA"))]
data6[, Crm_Group := fcase(
  grepl("ASSAULT|BATTERY|ROBBERY|HOMICIDE|KIDNAPPING|THREATS", `Crm Cd Desc`), "Crimes Against Persons",
  grepl("THEFT|BURGLARY|VEHICLE|SHOPLIFTING|STOLEN|ROBO", `Crm Cd Desc`), "Property Crimes",
  grepl("SEX|RAPE|SODOMY|LEWD|INDECENT|EXPOSURE", `Crm Cd Desc`), "Sexual Offenses",
  grepl("CHILD|CHLD|MINOR|Pornography", `Crm Cd Desc`, ignore.case = TRUE), "Child Protection/Crimes",
  grepl("VANDALISM|ARSON|DAMAGE", `Crm Cd Desc`), "Vandalism & Property Damage",
  grepl("FORGERY|EMBEZZLEMENT|FRAUD|IDENTITY|COUNTERFEIT|BUNCO", `Crm Cd Desc`), "Financial Crimes",
  grepl("COURT ORDER|RESTRAINING|TRESPASSING|WEAPON|DRUNK|DUMPING|DISTURBING", `Crm Cd Desc`), 
  "Public Order & Judicial Violations",
  rep(TRUE, .N), "Other / Miscellaneous")]

head(data6[, .(`Date Rptd`, `DATE OCC`, Diff_Date, Año_OCC, Mes_OCC, Dia_OCC, Dia_Semana, 
               `TIME OCC`, Crm_Group, `Vict Sex`, `Vict Descent`, LAT, LON)])
```

    ## # A tidytable: 6 × 13
    ##   `Date Rptd` `DATE OCC` Diff_Date Año_OCC Mes_OCC Dia_OCC Dia_Semana `TIME OCC`
    ##   <IDate>     <IDate>        <dbl> <fct>   <ord>     <int> <ord>      <ITime>   
    ## 1 2021-04-11  2020-11-07       155 2020    Novemb…       7 Saturday   08:45:00  
    ## 2 2020-10-21  2020-10-18         3 2020    October      18 Sunday     18:45:00  
    ## 3 2024-12-10  2020-10-30      1502 2020    October      30 Friday     12:40:00  
    ## 4 2020-12-24  2020-12-24         0 2020    Decemb…      24 Thursday   13:10:00  
    ## 5 2020-10-03  2020-09-29         4 2020    Septem…      29 Tuesday    18:30:00  
    ## 6 2024-12-11  2020-11-11      1491 2020    Novemb…      11 Wednesday  12:10:00  
    ## # ℹ 5 more variables: Crm_Group <chr>, `Vict Sex` <fct>, `Vict Descent` <fct>,
    ## #   LAT <dbl>, LON <dbl>

<h1 style="color: #006400;">

2.  Análisis temporal de los delitos

    </h1>

<h2 style="color: #228B22;">

2.1 Distribución anual

</h2>

``` r
Crime_Anual = data6[, .(Total = .N), by = .(Año_OCC)]
ggplot(Crime_Anual, aes(x = Año_OCC, y = Total, fill = Año_OCC)) +
  geom_col() +
  geom_text(aes(label = scales::percent(Total/nrow(data6), accuracy = 0.01)), 
            vjust = -0.5, size = 3.3, fontface = "bold") +
  scale_fill_manual(values = color_A) +
  labs(title = "Crime Distribution by Year",
       x = "", y = "Number of Incidents", fill = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),                          
    axis.title = element_text(size = 12, face = "bold")
    )
```

<img src="Examen_6_files/figure-gfm/CrimeAnual-1.png" alt="" style="display: block; margin: auto;" />

Lo primero que analizamos es el total de crimenes que se producen por
año. Además encima de la barra del histograma correspondiente a cada año
se encuentra el porcentaje que representa ese año con respecto a los
crimenes totales registrados en esa ventana temporal. Como podemos
observar hay un menor número de delitos cometidos en 2024. Este número
es muy inferior con respecto al resto de los años anteriores. Por lo que
se intuye que los registros totales de este año aún no están
disponibles. Obviando 2024, se observa que en genral ha habido un
incremento de la delincuencia desde 2020 (Año de la Pandemia de
Covid-19).

<h2 style="color: #228B22;">

2.2 Distribución mensual

</h2>

``` r
Crime_Monthly = data6[, .(Total = .N), by = .(Año_OCC, Mes_OCC)]
ggplot(Crime_Monthly, aes(x = Total, y = fct_rev(Mes_OCC), fill = fct_rev(Año_OCC))) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = scales::percent(Total / nrow(data6), accuracy = 0.01)),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = rev(color_A)) +
  labs(title = "Crime Distribution by Month", x = "Number of Incidents", y = "", fill="Year") +
  theme_minimal() +
  theme(
  plot.title = element_text(size = 18, face = "bold"),
  axis.text.y = element_text(size = 12, face = "bold"),
  axis.text.x = element_text(size = 12, face = "bold"),                          
  axis.title = element_text(size = 15, face = "bold")
  )
```

![](Examen_6_files/figure-gfm/CrimeMonth-1.png)<!-- -->

A continuación procedemos a analizar la distribución mensual de los
delitos. Como era de esperar seg’un avanzan los meses en 2024 el
porcentaje de delitos es menor, reforzando la hipótesis de que el
registro total de delitos de este año aún no ha sido actualizado. En
general se observa un aumento de la delincuencia en todos los meses con
respecto al año 2020. Además observamos como hay una menor incidencia de
delitos en los meses de 2020 con respecto a Enero de ese mismo año.
Reforzando la idea de que este descenso pudiera ser producto del
confinamiento.

``` r
data6 = data6[year(`DATE OCC`) != 2024]
Crime_Daily = data6[, .(Total = .N), by = .(`DATE OCC`, Dia_OCC, Dia_Semana, Mes_OCC, Año_OCC)]
ggplot(Crime_Daily, aes(x = Mes_OCC, y = Total)) +
  geom_boxplot(aes(fill = Mes_OCC, alpha = 1)) +
  labs(title = "Monthly Crime Distribution",
       x = "", y = "Number of Incidents") +
  theme_bw() +
  easy_remove_legend() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeDayMonthBP-1.png)<!-- -->

Debido al sesgo de los datos en 2024, procedemos a su descarte antes de
continuar con el resto del análisis. Al analizar la distribución mensual
de los delitos a lo largo del año, observamos que en general los meses
de Verano y Otoño son en los que se produce un mayor número de delitos.

<h2 style="color: #228B22;">

2.3 Distribución diaria

</h2>

``` r
ggplot(Crime_Daily, aes(x = `DATE OCC`, y = Total)) +
  geom_point(aes(colour = Año_OCC), alpha = 1, size = 1) +
  geom_smooth() +
  facet_wrap(~ Año_OCC, scales = "free", ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = color_A) +
  labs(title = "Crime Evolution",
       x = "Year", y = "Daily Crime Incidents") +
  theme_bw() +
  easy_remove_legend() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeDay-1.png)<!-- -->

Cuando analizamos la evolución diaria de los delitos en la ventana
temporal dada, notamos que al inicio de cada mes hay un pico en el
número de delitos.

``` r
Crime_DayMonth = Crime_Daily[, .(AvgNDay = mean(Total)), by = .(Dia_OCC, Año_OCC)]
ggplot(Crime_DayMonth, aes(x = Dia_OCC, y = AvgNDay)) +
  geom_point(aes(colour = Año_OCC), alpha = 1, size = 3) +
  geom_smooth() +
  facet_wrap(~ Año_OCC, scales = "free", ncol = 1) +
  scale_x_continuous(breaks = seq(1, 31, by = 1)) +
  scale_color_manual(values = color_A) +
  labs(title = "Crime Evolution",
       x = "Year", y = "Mean Incidents per Day of Month") +
  theme_bw() +
  easy_remove_legend() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeDayMonth-1.png)<!-- -->

Si observamos la cantidad de delitos en las fechas por mes, encontramos
que hay un aumento notorio de estos al inicio de cada mes. Para esta
representación es importante calcular el promedio por número de día en
el mes, ya que no todos los meses tienen el mismo número de días, y nos
encontrariamos con una subrepresentación del número total de delitos en
las fechas, 29, 30 y 31 del mes.

``` r
ggplot(Crime_Daily, aes(x = Dia_Semana, y = Total)) +
  geom_boxplot(aes(fill = Dia_Semana, alpha = 1)) +
  labs(title = "Weekly Crime Distribution",
       x = "", y = "Number of Incidents") +
  theme_bw() +
  easy_remove_legend() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeDayWeek-1.png)<!-- -->

Por otro lado, queremos observar si hay un mayor número de crimenes en
función del día de la semana en que nos encontremos. En el siguiente
gráfico se nos indicaría que los Viernes y los Sábados son días de gran
incidencia.

``` r
ggplot(data6, aes(x = as.numeric(`TIME OCC`) / 3600)) +
  geom_density(fill = "darkgray", alpha = 0.5, color = "darkblue") +
  scale_x_continuous(breaks = seq(0, 24, by = 2)) + 
  theme_minimal() +
  labs(title = "CRIME PEAK HOURS", x = "Hour of Day", y = "Density of Incidents") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeTime-1.png)<!-- -->

En el siguiente gráfico nos encontramos que la hora pico en la que se
producen los delitos es a las 12 del mediodía.

``` r
ggplot(data6, aes(x = as.numeric(`TIME OCC`) / 3600)) +
  geom_density(aes(fill = Dia_Semana), alpha = 0.5, color = "darkblue") + 
  facet_wrap(~ Dia_Semana, scales = "free", ncol = 1) +
  scale_x_continuous(breaks = seq(0, 23, by = 4)) +
  theme_bw() +
  easy_remove_legend() +
  labs(title = "CRIME PEAK HOURS", x = "Hour of Day", y = "Density of Incidents") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeTimeWeek-1.png)<!-- -->

Además queremos saber si esta hora pico varía en función del día de la
semana. En función de las gráfias representadas observamos que la
tendencia es la misma.

``` r
ggplot(data6, aes(x = as.numeric(`TIME OCC`) / 3600, fill = Crm_Group)) +
  geom_density(alpha = 0.5, color = "darkblue") +
  scale_x_continuous(breaks = seq(0, 24, by = 2)) + 
  facet_wrap(~ Crm_Group, ncol = 1) + 
  theme_bw() +
  labs(title = "CRIME PEAK HOURS", x = "Hour of Day", y = "Density of Incidents") +
  easy_remove_legend() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeTimeType-1.png)<!-- -->

Atendiendo al tipo de crimen cometido se observa que en general el pico
horario se mantiene.

<h2 style="color: #228B22;">

2.4 Tiempo transcurrido desde el crimen hasta la denuncia

</h2>

``` r
Crime_Rates = data6[, .(AvgRetraso = mean(Diff_Date, na.rm = TRUE),
                       Resueltos = mean(Status != "IC", na.rm = TRUE), 
                       Total = .N), 
                   by = .(`Crm Cd Desc`, Crm_Group)
                   ][Total > 100][order(-AvgRetraso)]
Crime_Rates_lm = lm(Resueltos ~ AvgRetraso, data = Crime_Rates, weights = Total)
Crime_Rates_lm_dt = as.data.table(tidy(Crime_Rates_lm))
Crime_Rates_lm_dt[, `:=`(
  p.value = ifelse(p.value < 2.2e-16, "< 2.2e-16", formatC(p.value, format = "e", digits = 2)),
  estimate = round(estimate, 3),
  std.error = round(std.error, 3),
  statistic = round(statistic, 3)
)]

kable(Crime_Rates_lm_dt, 
      caption = "Análisis de Regresión: Tasa de Resolución",
      col.names = c("Variable", "Estimación", "Error Est.", "Estadístico t", "p-valor"),
      align = "lrrrr")
```

| Variable    | Estimación | Error Est. | Estadístico t |  p-valor |
|:------------|-----------:|-----------:|--------------:|---------:|
| (Intercept) |      0.234 |      0.024 |         9.583 | 1.28e-15 |
| AvgRetraso  |     -0.002 |      0.001 |        -1.645 | 1.03e-01 |

Análisis de Regresión: Tasa de Resolución

A continuación queremos calcular si hay algún efecto en la resolución
del caso atendiendo al tiempo que pasa desde que se produce el delito
hasta que se denuncia. Haciendo un modelo de regresión lineal obtenemos
que para el conjunto global de los delitos no hay una relación
significativa entre el tiempo de demora en la denuncia con la resolución
del caso.

``` r
ggplot(Crime_Rates, aes(x = AvgRetraso, y = Resueltos, size = Total, fill = Crm_Group)) +
  geom_point(alpha = 1, shape = 21, color = "black", stroke = 0.5) +
  scale_size(range = c(2, 25)) + 
  scale_fill_manual(values = color_D) +
  theme_minimal() +
  labs(title = "Reporting Delay vs. Clearance Rate", x = "Average Delay (Days)", y = "Clearance Rate",
       size = "Total Incidents", fill = "Crime Category") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 5))) + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/CrimeRateb-1.png)<!-- -->

Cuando representamos los tipos de delitos según el tiempo de demora en
la denuncia y su tasa de resolución. Por un lado encontramos que los
delitos contra la infancia y los delitos sexuales son los que más se
tardan en reportar. Por otro lado los crímenes contra la propiedad y de
vandalismo son los que peor tasa de resolución tienen. Por otro lado la
mayor proporción de tipo de delitos son los crímenes contra la propiedad
y contra las personas.

``` r
CrimeRate_cor = data6[, .(diff_hours_cor = cor(Diff_Date, Status != "IC", method = "spearman"),
                            Total = .N), by = .(`Crm Cd Desc`)][Total > 500][order(-diff_hours_cor)]
separador = list("Crm Cd Desc" = "...", "diff_hours_cor" = "...", "Total" = "...")
kable(rbind(head(CrimeRate_cor, 6), separador, tail(CrimeRate_cor, 6)))
```

| Crm Cd Desc | diff_hours_cor | Total |
|:---|:---|:---|
| VIOLATION OF TEMPORARY RESTRAINING ORDER | 0.187247406618868 | 874 |
| VIOLATION OF COURT ORDER | 0.176026433082339 | 5979 |
| BATTERY WITH SEXUAL CONTACT | 0.174229487798974 | 3885 |
| CHILD ANNOYING (17YRS & UNDER) | 0.159558804703309 | 984 |
| VIOLATION OF RESTRAINING ORDER | 0.14402323338186 | 10967 |
| CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT | 0.134497281049796 | 3294 |
| … | … | … |
| BURGLARY | -0.174240458638657 | 53411 |
| STALKING | -0.182742453118664 | 633 |
| VANDALISM - FELONY (\$400 & OVER, ALL CHURCH VANDALISMS) | -0.244438328143946 | 53141 |
| VEHICLE - ATTEMPT STOLEN | -0.255785552939168 | 3291 |
| BURGLARY FROM VEHICLE, ATTEMPTED | -0.276709314058912 | 640 |
| ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER | -0.410503937084274 | 1021 |

A continuación comprobamos si, en función del tipo de crimen, exista
correlación entre la resolución del caso y el tiempo que pasa desde que
se produce el delito hasta que se reporta. El hecho de que encontremos
una correlación positiva en los delitos sexuales y contra la infancia es
contraintuitivo, pero si atendemos a que si en primera instancia no se
ha hecho la denuncia y posteriormente se decide denunciar es porque
también se conoce a la persona victimaria. Por otro lado tenemos
correlación negativa en delitos de acoso y determinados tipo de robo y
agresión a oficiales de policía, indicándonos que cuanto más se tarda en
hacer la denuncia peor es la tasa de resolución del delito.

<h1 style="color: #006400;">

3.  Análisis demográfico de las víctimas

</h1>

<h2 style="color: #228B22;">

3.1 Sexo de las Víctimas

</h2>

``` r
Crime_by_Sex = data6[, .(Total = .N), by = .(`Vict Sex`)]
ggplot(Crime_by_Sex, aes(x = "", y = Total, fill = `Vict Sex`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = color_A) +
  geom_text(aes(label = scales::percent(Total/nrow(data6), accuracy = 0.01)), 
            position = position_stack(vjust = 0.5),
            colour = "white", size = 4, fontface = "bold") +
  labs(title = "Victim Distribution by Sex", fill = "Victim Sex") +
  theme_void() +
  theme(plot.title = element_text(size = 13, face = "bold"))
```

<img src="Examen_6_files/figure-gfm/Victim Sex-1.png" alt="" style="display: block; margin: auto;" />

A continuación procedmos a analizar el porcentaje de víctimas en función
del sexo. Dando como resultado que el porcentaje de víctimas hombres es
mayor que el resto. Además también encontramos que hay un porcentaje
importante de víctimas sobre la que no se tiene datos de su sexo,
13,17%.

``` r
top_crime_20_by_Sex = data6[, .(Total = .N), by = .(`Crm Cd Desc`, `Vict Sex`)
                       ][`Crm Cd Desc` %in% data6[, .N, by = `Crm Cd Desc`][order(-N)][1:20, `Crm Cd Desc`]]
ggplot(top_crime_20_by_Sex, aes(x = Total, y = reorder(`Crm Cd Desc`, Total, FUN = sum), fill = `Vict Sex`)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse((Total / nrow(data6)) > 0.006, 
                       scales::percent(Total / nrow(data6), accuracy = 0.01), "")),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = color_A) +
  labs(title = "Top 20 Crimes", x = "Number of Incidents", y = "", fill="Victim Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Victim%20Sex%20Top%20Crime-1.png)<!-- -->

Continuando con el análisis, seleccionamos los 20 crimenes con mayor
incidencia y calculamos su distribución en función del sexo. En este
análisis nos encontramos en que en el robo de vehículos no se tiene
registro de la víctima, además de ser este el principal delito con un
10,2% dobre el total de delitos. De igual manera el robo de objetos de
moto presenta un alto número de víctimas sin información del sexo. Por
otro lado nos encontramos que en la intimidación a la pareja o la
vioalción de órdenes de alejamiento la víctima es principalmente mujer.

``` r
crime_type_by_Sex = data6[, .(Total = .N), by = .(Crm_Group, `Vict Sex`)
                       ][Crm_Group %in% data6[, .N, by = Crm_Group][order(-N)][, Crm_Group]]
ggplot(crime_type_by_Sex, aes(x = Total, y = reorder(Crm_Group, Total, FUN = sum), fill = `Vict Sex`)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse((Total / nrow(data6)) > 0.02, 
                       scales::percent(Total / nrow(data6), accuracy = 0.01), "")),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = color_A) +
  labs(title = "Crime Category by Victim Sex", x = "Number of Incidents", y = "", fill="Victim Sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Victim%20Sex%20Type%20Crime-1.png)<!-- -->

Cuando antendemos al tipo de delito, nos encontramos que el porcentaje
de víctimas hombre es mayor en crímenes a la propiedad,mientras que en
los delitos de sexuales y de la infancia la mayor proporción de víctimas
es mujer.

<h2 style="color: #228B22;">

3.2 Origen étnico de las Víctimas

</h2>

``` r
Crime_by_Descent = data6[, .(Total = .N), by = .(`Vict Descent`)]
ggplot(Crime_by_Descent, aes(x = reorder(`Vict Descent`, -Total), y = Total, fill = `Vict Descent`)) +
  geom_col() +
  geom_text(aes(label = scales::percent(Total/nrow(data6), accuracy = 0.01)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = color_B) +
  labs(title = "Victim Distribution by Descent", x = "", y = "Number of Incidents", fill = "Victim Descent") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Examen_6_files/figure-gfm/Victim%20Descent-1.png)<!-- -->

Atendiendo al origen étnico de las víctimas encontramos que estas son
principalmente población hispana, seguida de población blanca y negra.
Sin datos de la distribución étnica de Los Ángeles no podemos sacar más
conclusiones.

``` r
top_crime_20_by_Desc = data6[, .(Total = .N), by = .(`Crm Cd Desc`, `Vict Descent`)
                       ][`Crm Cd Desc` %in% data6[, .N, by = `Crm Cd Desc`][order(-N)][1:20, `Crm Cd Desc`]]

ggplot(top_crime_20_by_Desc, aes(x = Total, y = reorder(`Crm Cd Desc`, Total, FUN = sum), 
                                 fill = `Vict Descent`)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse((Total / nrow(data6)) > 0.006, scales::percent(Total / nrow(data6), accuracy = 0.01), "")),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = color_B) +
  labs(title = "Top 20 Crimes", x = "Number of Incidents", y = "", fill="Victim Descent") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Victim%20Descent%20Top%20Crime-1.png)<!-- -->

Al igual que en el caso anterior seleccionamos los 20 crímenes con mayor
incidencia. Para el caso particular del robo de vehículos, tenemos que
no se tienen registros del origen étnico de la víctima, de igual manera
para un porcentaje importante de los robos de objetos de moto. Con
respecto a las tendencias del resto de delitos, observamos que hay una
mayor proporción de población hispana que sufre intimidación por la
pareja.

``` r
crime_type_by_Desc = data6[, .(Total = .N), by = .(Crm_Group, `Vict Descent`)
                       ][Crm_Group %in% data6[, .N, by = Crm_Group][order(-N)][, Crm_Group]]
ggplot(crime_type_by_Desc, aes(x = Total, y = reorder(Crm_Group, Total, FUN = sum), fill = `Vict Descent`)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse((Total / nrow(data6)) > 0.02, 
                       scales::percent(Total / nrow(data6), accuracy = 0.01), "")),
    position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = color_B) +
  labs(title = "Crime Category by Victim Descent", x = "Number of Incidents", y = "", fill="Victim Descent") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Victim%20Descent%20Type%20Crime-1.png)<!-- -->

Cuando antendemos al tipo de delito, observamos que hay un gran
porcentaje de población hispana que es víctima de crímenes contra la
persona.

<h2 style="color: #228B22;">

3.3 Pirámide demográfica de las Víctimas

</h2>

``` r
Crime_Pyramid = data6[`Vict Age` > 0 & `Vict Sex` %in% c("Male", "Female"), 
                   .(Total = .N), by = .(`Vict Age`, `Vict Sex`, `Vict Descent`)
                   ][`Vict Sex` == "Male", Total := Total * -1]
ggplot(Crime_Pyramid, aes(x = `Vict Age`, y = Total, fill = `Vict Descent`)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  scale_fill_manual(values = color_B) +
  theme_minimal() +
  labs(title = "Victim Age-Sex-Descent Pyramid", 
       x = "Victim Age", y = "Number of Incidents", fill = "Victim Descent") +
  annotate("text", x = 95, y = -5000, label = "MALE", fontface = "bold", size = 6) +
  annotate("text", x = 95, y = 5000, label = "FEMALE", fontface = "bold", size = 6) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
  )
```

![](Examen_6_files/figure-gfm/Victim%20Demo%20Pyramid-1.png)<!-- -->

En esta sección procedemos analiar la pirámide demográfica de los
delitos en función de la edad, el sexo y la etnia, por lo que los datos
de victimas de las que se desconocen el sexo o la edad han sido
descartados. Como podemos observar hay 2 datos atípicos. Estos se
corresponde con varones de 35 años de etnia desconocida y varones
blancos de 50 años. Probablemente esta sobrerrepresentación corresponda
a perfiles genéricos para víctimas.

``` r
Crime_Desc4 = data6[`Vict Age` > 0 & `Vict Sex` %in% c("Male", "Female")
                    &`Vict Descent` %in% c("Hispanic", "White", "Black", "Other")]
Crime_Desc4$`Vict Descent` = droplevels(Crime_Desc4$`Vict Descent`)
Crime_Desc4$`Vict Sex` = droplevels(Crime_Desc4$`Vict Sex`)
tabla = table(Crime_Desc4$`Vict Sex`, Crime_Desc4$`Vict Descent`)
residuos = chisq.test(tabla)$residuals
kable(round(residuos, 2), caption = "Residuos del test Chi-cuadrado:")
```

|        | Hispanic |  White |  Black |  Other |
|:-------|---------:|-------:|-------:|-------:|
| Male   |    -9.05 |  24.68 | -33.88 |  26.84 |
| Female |     9.16 | -24.97 |  34.28 | -27.15 |

Residuos del test Chi-cuadrado:

De la anterior pirámide seleccionamos los datos de las víctimas con
mayor número de incidencia de delitos en función del origen étnico,
estos se corresponden a las poblaciones hispanas, blancas, negras o de
otro origen étnico al del registro. Evaluamos si hay algún componente
demográfico en función del sexo y el origen étnico que tenga mayor
porbabilidad de ser víctima. Para ello realizamos un test Chi-Cuadrado,
enla siguiente tabla se muestran los residuos de este test, donde se nos
indica que hay un sesgo en el sexo de las víctimas. Siendo más probable
ser víctima si eres hombre blanco o mujer latina o negra.

<h2 style="color: #228B22;">

3.4 Edad de las Víctimas

</h2>

``` r
ggplot(Crime_Desc4, aes(x = interaction(`Vict Sex`, `Vict Descent`, sep = " "), y = `Vict Age`, 
                        fill = `Vict Descent`, color = `Vict Sex`)) +
  geom_boxplot(alpha = 0.6, linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_fill_manual(values = color_C) +
  scale_color_manual(values = c("Male" = "#264653", "Female" = "violetred4")) +
  labs(title = "Victim Age Distribution by Descent and Sex", x = "", y = "Victim Age") +
  theme_bw() +
  easy_remove_legend() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Victim%20Age%20Desc4-1.png)<!-- -->

Para finalizar esta sección, atendiendo a los 4 origenes étnicos
seleccionados, queremos saber cual es la distribución de las edades de
las víctimas. En los siguientes `boxplots` observamos que hay un sesgo
en la edad de las víctimas, siendo más jóvenes los mujeres dentro de
cada grupo étnico.

``` r
modelo_ASD = lm(`Vict Age` ~ `Vict Sex` + `Vict Descent`, data = Crime_Desc4)
modelo_ASD_dt = as.data.table(tidy(modelo_ASD))
modelo_ASD_dt[, `:=`(
  p.value = ifelse(p.value < 2.2e-16, "< 2.2e-16", formatC(p.value, format = "e", digits = 2)),
  estimate = round(estimate, 3),
  std.error = round(std.error, 3),
  statistic = round(statistic, 3),
  term = gsub("Vict Sex|Vict Descent|`", "", term)
)]

kable(modelo_ASD_dt, 
      caption = "Análisis de Regresión: Patrones de Edad",
      col.names = c("Variable", "Estimación", "Error Est.", "Estadístico t", "p-valor"),
      align = "lrrrr")
```

| Variable    | Estimación | Error Est. | Estadístico t |    p-valor |
|:------------|-----------:|-----------:|--------------:|-----------:|
| (Intercept) |     37.902 |      0.036 |      1056.173 | \< 2.2e-16 |
| Female      |     -1.919 |      0.039 |       -48.718 | \< 2.2e-16 |
| White       |      6.590 |      0.048 |       137.095 | \< 2.2e-16 |
| Black       |      2.609 |      0.053 |        49.258 | \< 2.2e-16 |
| Other       |      4.813 |      0.070 |        68.531 | \< 2.2e-16 |

Análisis de Regresión: Patrones de Edad

Haciendo una regresion lineal multiple, obtenemos que en promedio las
victimas de población hispano son más jóvenes, de igual manera ocurre
para el sexo femenino. Haciendo una regresion lineal multiple, obtenemos
que en promedio las victimas de población hispana son más jóvenes, de
igual manera ocurre para el sexo femenino.

<h1 style="color: #006400;">

4.  Análisis geográfico de los delitos

</h1>

En el `dataset` disponemos de las coordenadas de donde se originaron los
delitos, por lo que si los representamos podremos tener una aproximación
del mapa de trabajo.

<h2 style="color: #228B22;">

4.1 ¿Dónde se comenten delitos?

</h2>

``` r
Crime_Map = data6[!is.na(LAT) & LAT > 33.6 & LAT < 34.4 & 
                   !is.na(LON) & LON < -117.5 & LON > -118.7]

ggplot(Crime_Map, aes(x = LON, y = LAT, color = factor(`AREA NAME`))) +
  geom_point(size = 0.01) +
  scale_color_manual(values = color_B) +
  coord_quickmap() +
  theme_minimal() +
  labs(title = "Crime Geographic Area", x = "Longitude", y = "Latitude", color = "AREA NAME") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Mapa%20Area-1.png)<!-- -->

Procedemos a representar cada delito por sus coordenadas en función del
área en el que se origina. Este gráfico nos permite hacernos una idea
visual de la localización de las áreas en las que estamos trabajando. Y
usarla como referencia para localizar posibles puntos calientes de
incidencia criminal.

<h2 style="color: #228B22;">

4.2 Distribución geográfica de los delitos

</h2>

``` r
ggplot(Crime_Map, aes(x = LON, y = LAT)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.3) +
  geom_point(size = 0.01, alpha = 0.008, color = "black") +
  scale_fill_viridis_c(option = "magma") +
  coord_quickmap() + 
  theme_minimal() +
  labs(title = "Crime Geographic Distribution", x = "Longitude", y = "Latitude", fill = "Intensity") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Mapa%20Calor-1.png)<!-- -->

A continuación creamos un mapa de la densidad de delitos. Como
observamos estos se concentran principalmente en Central, Newton y
Hollywood.

<h2 style="color: #228B22;">

4.3 Distribución geográfica según los tipos de delito

</h2>

``` r
ggplot(Crime_Map, aes(x = LON, y = LAT)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.3) +
  geom_point(size = 0.01, alpha = 0.008, color = "black") +
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~ Crm_Group, ncol = 3) + 
  coord_quickmap() + 
  theme_minimal() +
  labs(title = "Crime Geographic Distribution by Crime Category", 
       x = "Longitude", y = "Latitude", fill = "Intensity") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),                          
    axis.title = element_text(size = 15, face = "bold")
    )
```

![](Examen_6_files/figure-gfm/Mapa%20Calor%20por%20Categoria-1.png)<!-- -->

Por último, representamos los mapas de densidad en función del tipo de
delito. No observamos que la densidad cambie con respecto a la norma
anterior.

<h1 style="color: #006400;">

5.  Conclusiones

</h1>

El análisis del dataset de los delitos llevados a cabo en Los Ángeles
nos permite determinar perfiels demográficos de las víctimas, momentos
de más riesgo, así como zonas calientes de riesgo.

**Hallazgos principales:**

- Ha habido un aumento en los delitos desde el año 2020
- Los Meses de Verano y e Inicios de Otoño son los de mayor criminalidad
- Al inicio de cada mes es cuando más delitos se producen
- En los días Viernes se producen más delitos
- La hora pico de incidencia delictiva es las 12 del mediodía
- Los delitos sexuales son los que más tardan en reportarse
- En algunos delitos de robo existe una correlación negativa entre la
  demora en el reporte y su resolución
- Las principales víctimas son las mujeres
- En los delitos sexuales hay una mayor proporción de víctimas mujeres
- El delito más frecuente es el robo de vehículos
- El mayor número de víctimas es de población hispana
- Hay un aumento en la proporción de hispanos víctimas de crimenes
  contra las personas
- El perfil de edad de las víctimas mujeres es inferior al de hombres
- El perfil de edad de las víctimas de la población hispana es inferior
  al resto
- Los delitos se concentran en las Áreas Central, Newton y Hollywood

**Limitaciones del análisis:**

- Los datos de delitos del año 2024 aún no han sido actualizados
- Faltan datos de la demografía de los Ángeles para hacer un análisis
  más completo
- En los delitos de robo de vehículos no hay información demográfica de
  la víctima

**Próximos pasos:**

- Analizar el resto de variables, en particular la variable Premis Desc
