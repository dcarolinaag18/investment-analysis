library(shiny)


# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        theme = "cerulean",
        title = 'Prueba Inversiones - Sura',
        # Parte 1: TRM y Petroleo
        
        tabPanel('Parte 1: TRM y Petróleo',
                 fluidPage(
                     titlePanel("Exploración de datos de TRM y de precio del Petróleo"),
                     fluidRow(
                         column(12, 
                                wellPanel(h5("Seleccione una ventana de tiempo del 2008-01-01 al 2020-10-15"),
                                          dateRangeInput("inDateRange", "Rango de fechas:",
                                                         start = "2008-01-01",
                                                         end = "2020-10-15",
                                                         min = "2008-01-01",
                                                         max = "2020-10-15"),
                                          actionButton("renderButton", "Render"),
                                          p("Presionar el botón de Render para aplicar cambios de rangos de fechas en las visualizaciones")
                                )
                         ),
                         h3("Comportamiento del TRM", align="center"),
                         fluidRow(
                             column(6, plotOutput("total_periodo_trm")),
                             column(6, plotOutput("total_mes_trm"))
                         ),
                         fluidRow(
                             column(12, plotOutput("total_dia_trm"))
                         ),
                         h3("Comportamiento del Precio de Petróleo", align="center"),
                         fluidRow(
                             column(6, plotOutput("total_periodo_pet")),
                             column(6, plotOutput("total_mes_pet"))
                         ),
                         fluidRow(
                             column(12, plotOutput("total_dia_pet"))
                         ),
                         h3("Boxplots discretizados mensualmente para la TRM", align="center"),
                         fluidRow(
                             column(12, plotOutput("boxplot_trm"))
                         ),
                         h3("Boxplots discretizados mensualmente para el petróleo", align="center"),
                         fluidRow(
                             column(12, plotOutput("boxplot_pet"))
                         ),
                         h3("Correlación entre la TRM y el precio del petróleo", align="center"),
                         fluidRow(
                             column(12, plotOutput("correlation"))
                         ),
                         h3("Tabla descriptiva por año de la TRM", align="center"),
                         fluidRow(
                             column(12, align="center", tableOutput("table_trm"))
                         ),
                         h3("Análisis de resultados y conclusiones", align="center"),
                         fluidRow(
                             column(12,
                                    # , align="center", 
                                    p("- El mercado petrolero es sensible a muchos factores que van desde decisiones políticas, económicas, diplomáticas o sociales. 
                                      Este es el caso del año 2014 donde hubo un desplome de los precios internacionales de los commodities y uno de los más afectados
                                      fue el precio del petróleo descolgándose de un valor por encima de los $90 USD a llegar a valores del $50 o $40 USD, lo que generó
                                      una crisis económica en todo el mundo. Una de las principales razones de la caída del precio en este año obedeció a que en USA 
                                      (uno de los principales compradores de petróleo) con la extracción no convencional de petróleo (fracking) tuvieran 
                                      grandes volúmenes de inventario de petróleo. Así mismo, China (otro de los grandes importadores de petróleo) vivió un proceso de 
                                      estancamiento o desaceleración económica lo que generó una reducción de la demanda. Estos 2 factores adversos de demanda, en conjunto
                                      con fricciones políticas sobre los niveles de producción mundial dentro de los países OPEP, hizo que se generara un efecto de sobreoferta
                                      de petróleo, lo que generó la caída internacional del precio."),
                                    p("- Otro escenario parecido se repitió en este 2020, pero con un agente nuevo: *COVID-19*, cuando muchos de los países industrializados 
                                      (USA y casi todo Europa) vivían sus picos más altos de contagios y muertes de pacientes, el precio del petróleo se desplomó a valores 
                                      históricos nunca antes vistos (sendas negativas) esto con ocasión que los precios de los futuros a corto plazo (previendo una mejora en 
                                      la economía mundial) alentaban un mejor precio, sin embargo, lo que mostraba el pasar de los días era que esto no era así, por lo que los
                                      precios de los futuros se desplomaron dramáticamente."),
                                    p("- Lo anterior sirve para postular una tesis sobre la compleja predicción del precio del petróleo. Si bien en algunos casos se pueden 
                                      observar comportamientos estacionales (semanales) para todos los meses."),
                                    p("- Así mismo, la relación inversa del precio del petróleo y la TRM sigue persistiendo: Tanto para ventanas de tiempo cortas (días o semanas) 
                                      como para rangos de tiempo más amplios (trimestre, semestres o años)."),
                                    p("- Entre tanto, la devaluación del peso colombiano frente al dólar es tremendamente marcado y fuerte, lo cual hace que se presenten ciertos
                                      riesgos para varios agentes internos y restándole competitividad al mercado colombiano. Para el caso colombiano la alta volatilidad de la TRM
                                      si bien está condicionada parcialmente al mercado petrolero, también es sensible a las decisiones monetarias de la Reserva Federal (FED), 
                                      pero, también al ambiente geopolítico del mundo (tensiones económicas y arancelarias en China y USA). En resumen, Colombia al optar por una
                                      flotación controlada de la tasa de cambio puede concluir que esta última está fuertemente afectada por el sector externo.")
                             )
                         )
                         
                     )
                 )
        ),
        tabPanel('Parte 2: índice S&P500', 
                 fluidPage(
                     titlePanel("Principales compañías que componen el índice S&P500"),
                     fluidRow(
                         column(12, 
                                wellPanel(h5("Seleccione una acción del índice S&P500"),
                                          selectInput("selectStock", "Acciones:",
                                                      c('APPLE', 'AMAZON', 'GOOGLE', 'MICROSOFT', 'FACEBOOK'), "solid"),
                                          actionButton("stockButton", "Stock"),
                                          p("Presionar el botón de Stock para aplicar el filtro de la acción en las visualizaciones")
                                )
                         ),
                         h3("Comportamiento de la acción", align="center"),
                         fluidRow(
                             column(6, plotOutput("apertura")),
                             column(6, plotOutput("maximo"))
                         ),
                         fluidRow(
                             column(6, plotOutput("minimo")),
                             column(6, plotOutput("cierre"))
                         ),
                         fluidRow(
                             column(6, plotOutput("volumen")),
                             column(6, plotOutput("ajuste"))
                         )
                     )
                 )
        )
                         
    )
)

