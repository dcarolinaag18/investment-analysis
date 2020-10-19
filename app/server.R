library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(leaflet)
library(readr)
library(tidyr)
library(DT)
library(ggcorrplot)

# Cargado de datos
## Parte 1
df <- fread('data_app/datos_parte1.csv', sep = ",")

## Parte 2
df2 <- fread('data_app/datos_parte2.csv', sep = ",")


## Funciones para el analisis del TRM y petróleo

# --------------- funciones para capa de visualización -----------------------
## Graficos TRM
graph_number_classes_trm <- function(start_date, end_date, nivel="mes", campo='TRM'){
    
    start_date = as_date(start_date)
    end_date = as_date(end_date)
    
    if (nivel == "periodo"){
        tmp_df <- df %>%
            filter(FECHA >= start_date, FECHA <= end_date) %>% 
            group_by(ANO) %>%
            summarise(total = mean(TRM), .groups="drop")
        
        ggplot(tmp_df, aes(x=factor(ANO), y=total, color=total)) +
            geom_line(aes(x=ANO, y=total)) + 
            #theme_classic() + 
            ggtitle(paste("Promedio de", campo, 'por año', sep=" ")) + 
            labs(y=paste("Promedio", campo), x="Año") + 
            scale_x_discrete(name ="Año", limits=array(2008:2020)) +
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.text.x = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=12),
                  legend.position = "none")

    } else if (nivel == "mes"){
        tmp_df <- df %>%
            filter(FECHA >= start_date, FECHA <= end_date) %>% 
            group_by(ANO, MES) %>% 
            summarise(total = mean(TRM), .groups="drop")
        
        ggplot(tmp_df) +
            geom_line(aes(x=MES, y=total, colour=factor(ANO))) + 
            scale_x_discrete(name ="Mes", limits=factor(1:12)) + 
            labs(colour="Año", y=paste("Promedio", campo), title=paste("Promedio de", campo, "por año y mes", sep=" ")) + 
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.text.x = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=12))
    } else {
        tmp_df <- df %>%
            filter(FECHA >= start_date, FECHA <= end_date) %>% 
            group_by(MES, DIA) %>% 
            summarise(total = mean(TRM), .groups="drop")
        
        ggplot(tmp_df) +
            geom_line(aes(x=DIA, y=total, group=factor(MES),  color=factor(MES))) + 
            facet_wrap(~MES,scales = "free_y") +
            labs(x='Dia', y=paste("Promedio", campo),
                 title = paste("Promedio de", campo, "por mes y día de la semana"),
                 color="Mes") + 
            #theme_classic() +
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(vjust = 0.7))
    }
}

## Graficos PETROLEO
graph_number_classes_pet <- function(start_date, end_date, nivel="mes", campo='PETROLEO'){
    
    start_date = as_date(start_date)
    end_date = as_date(end_date)
    
    if (nivel == "periodo"){
        tmp_df <- df %>%
            filter(FECHA >= start_date, FECHA <= end_date) %>% 
            group_by(ANO) %>%
            summarise(total = mean(PETROLEO), .groups="drop")
        
        ggplot(tmp_df, aes(x=factor(ANO), y=total, color=total)) +
            geom_line(aes(x=ANO, y=total)) + 
            scale_x_discrete(name ="Año", limits=array(2008:2020)) +
            ggtitle(paste("Promedio de", campo, 'por año', sep=" ")) + 
            labs(y=paste("Promedio", campo), x="Año") + 
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.text.x = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=12),
                  legend.position = "none")
        
    } else if (nivel == "mes"){
        tmp_df <- df %>%
            filter(FECHA >= start_date, FECHA <= end_date) %>% 
            group_by(ANO, MES) %>% 
            summarise(total = mean(PETROLEO), .groups="drop")
        
        ggplot(tmp_df) +
            geom_line(aes(x=MES, y=total, colour=factor(ANO))) + 
            scale_x_discrete(name ="Mes", limits=factor(1:12)) + 
            labs(colour="Año", y=paste("Promedio", campo), title=paste("Promedio de", campo, "por año y mes", sep=" ")) + 
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.text.x = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=12))
    } else {
        tmp_df <- df %>%
            filter(FECHA >= start_date, FECHA <= end_date) %>% 
            group_by(MES, DIA) %>% 
            summarise(total = mean(PETROLEO), .groups="drop")
        ggplot(tmp_df) +
            geom_line(aes(x=DIA, y=total, group=factor(MES),color=factor(MES))) + 
            facet_wrap(~MES,scales = "free_y") +
            labs(x='Dia', y=paste("Promedio", campo),
                 title = paste("Promedio de", campo, "por mes y día de la semana"),
                 color="Mes") +  
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(vjust = 0.7))
    }
}

### Graficos de boxplots discretizados mensualmente para la TRM y el petróleo

## TRM
graph_boxplot_trm <- function(start_date, end_date, campo='TRM'){
    
    start_date = as_date(start_date)
    end_date = as_date(end_date)
    
    tmp_df <- df %>%
        filter(FECHA >= start_date, FECHA <= end_date) 
    
    ggplot(tmp_df) +
        geom_boxplot(aes(x=factor(MES), y=TRM, color=factor(MES))) + 
        # facet_wrap(~MES) +
        labs(x='Mes', y=paste(campo),
             color="Mes") + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(vjust = 0.7))
}

## Petroleo
graph_boxplot_pet <- function(start_date, end_date, campo='PETROLEO'){
    
    start_date = as_date(start_date)
    end_date = as_date(end_date)
    
    tmp_df <- df %>%
        filter(FECHA >= start_date, FECHA <= end_date) 
    
    ggplot(tmp_df) +
        geom_boxplot(aes(x=factor(MES), y=PETROLEO, color=factor(MES))) + 
        # facet_wrap(~MES) +
        labs(x='Mes', y=paste(campo),
             color="Mes") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(vjust = 0.7))
}


#### Correlacion entre la TRM y el precio del petroleo desde el año 2010 a la fecha
graph_correlation <- function(start_date, end_date){
    
    start_date = as_date(start_date)
    end_date = as_date(end_date)
    
    tmp_df <- df %>%
        filter(FECHA >= start_date, FECHA <= end_date)
    
    tmp_df <- select(tmp_df, TRM, PETROLEO)
    
    correlation <- round(cor(tmp_df), 1)
    ggcorrplot(correlation, hc.order = TRUE, 
               lab = TRUE,
               outline.col = "white",
               ggtheme = ggplot2::theme_gray,
               color = c("#999999", "#FFFFFF", "#E69F00")##"#6D9EC1", "white", "#E46726")
                   )
    # corrplot(correlacion, method="number", type="upper")
}

#### Tabla de la TRM con valor mínimo, máximo y promedio
table_trm <- function(start_date, end_date){
    
    start_date = as_date(start_date)
    end_date = as_date(end_date)
    
    tmp_df <- df %>%
        filter(FECHA >= start_date, FECHA <= end_date) %>% 
        group_by(ANO) %>% 
        summarise(MINIMO = min(TRM), MAXIMO = max(TRM), PROMEDIO = mean(TRM), .groups="drop")
}


###************* Parte 2 ********************
plot_open <- function(accion){
    tmp_df2 <- df2 %>%
        filter(ACCION == accion)

    ggplot(tmp_df2, aes(x=factor(DATE), y=OPEN, colour="#E69F00")) +
        geom_line(aes(x=DATE, y=OPEN)) + 
        ggtitle(paste("Apertura de", accion, sep=" ")) +
        labs(y=paste("Apertura", accion), x="Fecha") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12),
              legend.position = "none")
}

plot_max <- function(accion){
    tmp_df2 <- df2 %>%
        filter(ACCION == accion)
    
    ggplot(tmp_df2, aes(x=factor(DATE), y=HIGH, colour=HIGH)) +
        geom_line(aes(x=DATE, y=HIGH)) + 
        ggtitle(paste("Maximo de", accion, sep=" ")) +
        labs(y=paste("Maximo", accion), x="Fecha") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12),
              legend.position = "none")
}

plot_low <- function(accion){
    tmp_df2 <- df2 %>%
        filter(ACCION == accion)
    
    ggplot(tmp_df2, aes(x=factor(DATE), y=LOW)) +
        geom_line(aes(x=DATE, y=LOW)) + 
        ggtitle(paste("Minimo de", accion, sep=" ")) +
        labs(y=paste("Minimo", accion), x="Fecha") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12),
              legend.position = "none")
}

plot_close <- function(accion){
    tmp_df2 <- df2 %>%
        filter(ACCION == accion)
    
    ggplot(tmp_df2, aes(x=factor(DATE), y=CLOSE, colour="#E69F00")) +
        geom_line(aes(x=DATE, y=CLOSE)) + 
        ggtitle(paste("Cierre de", accion, sep=" ")) +
        labs(y=paste("Cierre", accion), x="Fecha") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12),
              legend.position = "none")
}

plot_vol <- function(accion){
    tmp_df2 <- df2 %>%
        filter(ACCION == accion)
    
    ggplot(tmp_df2, aes(x=factor(DATE), y=VOLUME, colour=VOLUME)) +
        geom_line(aes(x=DATE, y=VOLUME)) + 
        ggtitle(paste("Volumen de", accion, sep=" ")) +
        labs(y=paste("Volumen", accion), x="Fecha") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12),
              legend.position = "none")
}

plot_adj <- function(accion){
    tmp_df2 <- df2 %>%
        filter(ACCION == accion)
    
    ggplot(tmp_df2, aes(x=factor(DATE), y=ADJUSTED)) +
        geom_line(aes(x=DATE, y=ADJUSTED)) + 
        ggtitle(paste("Ajuste de", accion, sep=" ")) +
        labs(y=paste("Ajuste", accion), x="Fecha") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=12),
              axis.title.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12),
              legend.position = "none")
}

# DATE,OPEN,HIGH,LOW,CLOSE,VOLUME,ADJUSTED,ACCION

function(input, output, session) {
    
    # --------------- Tab de visualización ----------------------------------
    
    ## TRM año, mes, día
    output_total_periodo_trm <- eventReactive(input$renderButton, {
        graph_number_classes_trm(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            nivel = "periodo", 
            campo = 'TRM')} , ignoreNULL = FALSE)
    
    output_total_mes_trm <- eventReactive(input$renderButton, {
        graph_number_classes_trm(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            nivel = "mes", 
            campo = 'TRM')}, ignoreNULL = FALSE)
    
    output_total_dia_trm <- eventReactive(input$renderButton, {
        graph_number_classes_trm(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            nivel = "dia", 
            campo = 'TRM')}, ignoreNULL = FALSE)
    
    ## Petroleo año, mes, día
    output_total_periodo_pet <- eventReactive(input$renderButton, {
        graph_number_classes_pet(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            nivel = "periodo",
            campo = 'PETROLEO')} , ignoreNULL = FALSE)
    
    output_total_mes_pet <- eventReactive(input$renderButton, {
        graph_number_classes_pet(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            nivel = "mes",
            campo = 'PETROLEO')}, ignoreNULL = FALSE)
    
    output_total_dia_pet <- eventReactive(input$renderButton, {
        graph_number_classes_pet(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            nivel = "dia",
            campo = 'PETROLEO')}, ignoreNULL = FALSE)
    
    ## Boxplot
    output_boxplot_trm <- eventReactive(input$renderButton, {
        graph_boxplot_trm(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            campo = 'TRM')}, ignoreNULL = FALSE)
    
    output_boxplot_pet <- eventReactive(input$renderButton, {
        graph_boxplot_pet(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2],
            campo = 'PETROLEO')}, ignoreNULL = FALSE)
    
    ## Correlacion
    output_correlation <- eventReactive(input$renderButton, {
        graph_correlation(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2])}, ignoreNULL = FALSE)
    
    ## Tabla TRM
    output_table_trm <- eventReactive(input$renderButton, {
        table_trm(
            start_date = input$inDateRange[1], 
            end_date = input$inDateRange[2])}, ignoreNULL = FALSE)
    
    
    ####****** Parte 2
    ## Apertura
    output_plot_open <- eventReactive(input$stockButton, {
        plot_open(
            accion = input$selectStock)
        }, ignoreNULL = FALSE)
    
    ## Maximo
    output_plot_max <- eventReactive(input$stockButton, {
        plot_max(
            accion = input$selectStock)
    }, ignoreNULL = FALSE)
    
    ## Minimo
    output_plot_low <- eventReactive(input$stockButton, {
        plot_low(
            accion = input$selectStock)
    }, ignoreNULL = FALSE)
    
    ## Cierre
    output_plot_clo <- eventReactive(input$stockButton, {
        plot_close(
            accion = input$selectStock)
    }, ignoreNULL = FALSE)
    
    ## Volumen
    output_plot_vol <- eventReactive(input$stockButton, {
        plot_vol(
            accion = input$selectStock)
    }, ignoreNULL = FALSE)
    
    ## Ajuste
    output_plot_adj <- eventReactive(input$stockButton, {
        plot_adj(
            accion = input$selectStock)
    }, ignoreNULL = FALSE)
    
    
    #**********************************************
    # renderización de las gráficas
    
    ## TRM
    output$total_periodo_trm <- renderPlot({
        output_total_periodo_trm()
    })
    
    output$total_mes_trm <- renderPlot({
        output_total_mes_trm()
    })
    
    output$total_dia_trm <- renderPlot({
        output_total_dia_trm()
    })
    
    ## Petroleo
    output$total_periodo_pet <- renderPlot({
        output_total_periodo_pet()
    })
    
    output$total_mes_pet <- renderPlot({
        output_total_mes_pet()
    })
    
    output$total_dia_pet <- renderPlot({
        output_total_dia_pet()
    })
    
    ## Boxplot
    output$boxplot_trm <- renderPlot({
        output_boxplot_trm()
    })
    output$boxplot_pet <- renderPlot({
        output_boxplot_pet()
    })
    
    ## Correlacion
    output$correlation <- renderPlot({
        output_correlation()
    })
    
    ## Tabla TRM
    output$table_trm <- renderTable({
        output_table_trm()
    })
    
    ## Parte 2
    # Apertura
    output$apertura <- renderPlot({
        output_plot_open()
    })
    # Maximo
    output$maximo <- renderPlot({
        output_plot_max()
    })
    # Minimo
    output$minimo <- renderPlot({
        output_plot_low()
    })
    # Cierre
    output$cierre <- renderPlot({
        output_plot_clo()
    })
    # Volumen
    output$volumen <- renderPlot({
        output_plot_vol()
    })
    # Ajuste
    output$ajuste <- renderPlot({
        output_plot_adj()
    })

}

