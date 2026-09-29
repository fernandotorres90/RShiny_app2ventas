###############################################################################
# NOMBRE DEL ARCHIVO: uiNano.R
# AUTOR: Fernando Torres Vázquez
# PROYECTO: Análisis y visualización de producción científica (NanoMx_hasta2024)
# INSTITUCIÓN: UNAM
# FECHA: 15/03/2026
#
# =============================================================================
# FUNCIONALIDADES PRINCIPALES DEL ARCHIVO
# =============================================================================
# Este archivo define la interfaz gráfica (UI) de la aplicación Shiny enfocada
# en el análisis de datos bibliométricos sobre nanotecnologias
#
# Aquí se construye todo lo que el usuario ve e interactúa:
# - Controles de filtrado, botones (tipo de documento, años)
# - Opciones de visualización, menús desplegables (tipo de gráfica, colores)
# - Tablas más gráficas interactivas
# - Funciones de descarga
#
# Este archivo NO procesa datos directamente
# El procesamiento de datos se realiza en el archivo serverNano.R
#
# =============================================================================
# INSTRUCCIONES PARA REPLICAR O MODIFICAR LA INTERFAZ
# =============================================================================
# 1. Puedes modificar:
#    - Títulos
#    - Etiquetas de botones
#    - Tipos de gráficos disponibles
#    - Colores por defecto
#    - Cambia textos descriptivos
#
# 2. Asegúrate de tener una base de datos con al menos:
#    - PY (año de publicación)
#    - DT (tipo de documento)
#
# 3. Para agregar nuevas funcionalidades:
#    - Añade nuevos inputs (selectInput, sliderInput, actionButton, etc.)
#    - Ubícalos dentro de sidebarPanel() o mainPanel()
#    - Conéctalos con el archivo server correspondiente
#
# 4. Para agregar nuevas variables o filtros:
#    - Usa selectInput, numericInput o botones (actionButton)
#    - Asegúrate de procesarlos en el server
#
# 5. Para agregar nuevas pestañas:
#    - Usa tabPanel() dentro de tabsetPanel()
#
# 6. Para modificar la visualización:
#    - Ajusta títulos, etiquetas (labels) y textos descriptivos
#    - Cambia colores o tipos de gráficos según necesidad
#
# 7. IMPORTANTE:
#    - Mantén consistencia entre:
#        input IDs (input$...)
#        output IDs (output$...)
#    - Los nombres deben coincidir exactamente con los definidos en server
#
# =============================================================================
# INSTRUCCIONES PARA EJECUTAR LA APP
# =============================================================================
# REQUISITOS:
# - Tener instalado R y RStudio
# - Instalar paquetes:
#     install.packages(c("shiny","readxl","dplyr","ggplot2","DT","colourpicker","openxlsx","scales"))
#
# - Archivos requeridos:
#     "NanoMx_hasta2024_v2.xlsx"
#     (Debe estar en la misma carpeta)
#
#     "serverNano.R"
#     (Debe estar en la misma carpeta)
#
# PASOS:
# 1. Abrir este archivo (uiNano.R) y serverNano.R en RStudio
# 2. Ejecutar este archivo junto con serverNano.R
# 3. Ejecutar la app con:
#       shinyApp(ui = ui, server = server)
#
###############################################################################

# CARGA DE LIBRERÍAS ----------------------------------------------------------
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(colourpicker)
library(openxlsx)
library(scales)

# Aumenta el tamaño máximo permitido para subir archivos grandes
options(shiny.maxRequestSize = 2000 * 1024^2)

# DEFINICIÓN DE LA INTERFAZ ---------------------------------------------------
ui <- fluidPage(
  
  # Título principal
  titlePanel("Frecuencia de datos financieros"),
  
  # CONTENEDOR DE PESTAÑAS
  tabsetPanel(
    
    # -------------------------------------------------------------------------
    # PESTAÑA 1
    # -------------------------------------------------------------------------
    tabPanel("Tablas y gráficas",
             sidebarLayout(
               
               # Panel lateral: controles de selección y visualización
               sidebarPanel(
                 uiOutput("var_frecuencia"),
                 uiOutput("slider_tf"),
                 
                 # Tipo de gráfico: barras o pastel
                 selectInput("tipo_grafico_tf", "Tipo de gráfico:",
                             choices = c("Barras", "Pastel"),
                             selected = "Barras"),
                 
                 # Color de barras
                 colourInput("color_barras_tf", "Color de las barras:", value = "#1f77b4"),
                 
                 # Formato de descarga
                 selectInput("formato_descarga_tf", "Formato de descarga:",
                             choices = c("PNG", "JPEG", "JPG", "PDF", "CSV", "XLSX", "TXT"),
                             selected = "PNG"),
                 
                 # Botones de descarga y ampliación
                 downloadButton("descargar_tf", "Descargar gráfica / datos"),
                 actionButton("ampliar_tf", "Ampliar gráfica"),
                 br(), br(), br(),
                 
                 # Número de datos a mostrar y modo (Top, Bottom, Todos)
                 numericInput("n_datos_tf", "Número de datos a mostrar:", value = 10, min = 1),
                 selectInput("modo_tf", "Modo de visualización:",
                             choices = c("Top", "Bottom", "Todos"),
                             selected = "Top"),
                 
                 # Botón para actualizar la gráfica
                 actionButton("actualizar_tf", "Actualizar gráfica", class = "btn-primary")
               ),
               
               # Panel principal: visualización de gráfica y tabla
               mainPanel(
                 plotOutput("grafica_tf", height = "500px"),
                 br(),
                 DTOutput("tabla_tf")
               )
             )
    ),
    
    
    # -------------------------------------------------------------------------
    # PESTAÑA 2: INFORMACIÓN
    # -------------------------------------------------------------------------
    tabPanel("Información",
             
             br(),
             
             tags$p(
               "Esta aplicación web fue desarrollada para visualizar frecuencias de datos financieros a partir de una base de datos en formato Excel."
             ),
             
             br(),
             
             tags$p(
               "El sistema permite seleccionar columnas y generar gráficas de barras o pastel para resumir la información."
             ),
             
             br(),
             
             tags$p(
               "Para la visualización de datos financieros se ha usado el libro de Excel de ejemplo financiero, disponible en ",
               
               tags$a(
                 "Microsoft Build 2026",
                 href = "https://learn.microsoft.com/es-es/power-bi/create-reports/sample-financial-download",
                 target = "_blank"
               )
             ),
             
             br(),
             
             tags$p(
               "Esta aplicación web fue desarrollada por Fernando Torres.",
               
               tags$a(
                 "Ver sitio web del portafolio",
                 href = "https://fernandotorres90.github.io/portafolio/",
                 target = "_blank"
               )
             )
    )
  )
)
###############################################################################
# FIN DEL ARCHIVO
###############################################################################