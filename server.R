###############################################################################
# NOMBRE DEL ARCHIVO: serverVentas.R
# AUTOR: Fernando Torres Vázquez
# PROYECTO: Dashboard de Producción científica (NanoMx_hasta2024)
# INSTITUCIÓN: UNAM
# FECHA: 15/03/2026
#
# =============================================================================
# FUNCIONALIDADES PRINCIPALES DEL ARCHIVO
# =============================================================================
# Esta aplicación desarrollada en Shiny permite analizar datos bibliométricos
# (publicaciones científicas) a partir de un archivo Excel o similar.
#
# 1. Panel de visualización de productividad científica por año.
#    - Permite filtrar por tipo de documento y rango de años.
#    - Selección de tipo de gráfico (barras o línea).
#    - Configuración del color de las barras o líneas.
#    - Descarga de gráfica y datos en múltiples formatos.
#    - Ampliación de gráfica en modal emergente.
#
# 2. Panel de Tablas de frecuencia de variables:
#    - Selección de variable (autores, instituciones, países, revistas, temas, etc.).
#    - Rango de años filtrable.
#    - Tipo de gráfico: barras o pastel.
#    - Número de datos a mostrar y modo de visualización (Top, Bottom, Todos).
#    - Descarga de gráfica y tabla.
#    - Ampliación de gráfica en modal emergente.
#
# 3. Panel de Referencia:
#    - Información sobre la fuente de datos.
#    - Créditos del proyecto y responsables.
#
# Esta app está diseñada para trabajar con datos provenientes de OpenAlex u
# otras fuentes similares, donde los campos pueden tener múltiples valores.
#
# =============================================================================
# INSTRUCCIONES PARA REPLICAR O CREAR UNA APP SIMILAR
# =============================================================================
# 1. Asegúrate de tener una base de datos con al menos:
#    - PY: Año de publicación
#    - DT: Tipo de documento
#
# 2. Puedes agregar más variables como:
#    - AU (Autores)
#    - AU_UN (Instituciones)
#    - SO (Revistas)
#    - language, temas, eventos, etc.
#
# 3. Puedes adaptar variables de análisis (cambiando nombres de columnas)
#
# 4. Si tus datos contienen múltiples valores en una celda (ej. autores),
#    sepáralos con ";" para que la app los procese correctamente.
#
# =============================================================================
# INSTRUCCIONES PARA EJECUTAR LA APP
# =============================================================================
# REQUISITOS:
# - Tener instalado R y RStudio
# - Instalar los paquetes necesarios (solo la primera vez):
#     install.packages(c("shiny","readxl","dplyr","ggplot2","DT",
#                        "colourpicker","openxlsx","scales","tidyr"))
#
# - Colocar el archivo de datos en el mismo directorio del proyecto:
#     "NanoMx_hasta2024_v2.xlsx"
#
# PASOS:
# 1. Abrir este archivo en RStudio
# 2. Ejecutar todo el script o hacer clic en "Run App"
# 3. La app se abrirá en el navegador o visor de RStudio
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
library(tidyr)

# FUNCIÓN PRINCIPAL DEL SERVIDOR ----------------------------------------------
server <- function(input, output, session) {
  
  # ===========================================================================
  # 1. LECTURA Y VALIDACIÓN DE DATOS
  # ===========================================================================
  datos_reactivo <- reactive({
    # Carga del archivo desde el directorio de trabajo
    archivo <- "Financial_Sample.xlsx" #🟥🟥🟥ANTES archivo <- file.path(getwd(), "NanoMx_hasta2024_v2.xlsx")
    
    # Detección automática del formato de archivo
    ext <- tools::file_ext(archivo)
    if (ext == "xlsx") df <- read_excel(archivo)
    else if (ext == "csv") df <- read.csv(archivo, stringsAsFactors = FALSE)
    else if (ext == "txt") df <- read.table(archivo, header = TRUE, sep = "\t")
    else validate("Formato de archivo no soportado")
    
    df#🟧🟧🟧  en esta segunda ronda de cambios fue para universalizar la app, sin validar columnas obligatorias
  })

  
  # ===========================================================================
  # 8. TABLAS DE FRECUENCIA (Autores, Países, Revistas, etc.)
  # ===========================================================================
  datos_tf <- reactive({
    req(datos_reactivo())
    datos_reactivo()
  })
  
  # Selector de variable para tabla de frecuencia
  output$var_frecuencia <- renderUI({
    
    req(datos_tf())
    
    selectInput(
      "variable_tf",
      "Selecciona variable:",
      choices = names(datos_tf())
    )
  })
  
  # Slider de rango de años para frecuencia
  output$slider_tf <- renderUI({
    req(datos_tf())
    if (!"PY" %in% names(datos_tf())) return(NULL)
    sliderInput("rango_tf", "Rango de años:",
                min=min(datos_tf()$PY,na.rm=TRUE),
                max=max(datos_tf()$PY,na.rm=TRUE),
                value=c(min(datos_tf()$PY,na.rm=TRUE), max(datos_tf()$PY,na.rm=TRUE)),
                step=1, sep="")
  })
  
  
  # ===========================================================================
  # 9. PROCESAMIENTO DE DATOS PARA TABLAS DE FRECUENCIA
  # ===========================================================================
  datos_filtrados_tf <- eventReactive(input$actualizar_tf, {
    
    df <- datos_tf()
    
    # Filtro por rango de años (solo si existe PY)
    if ("PY" %in% names(df) & !is.null(input$rango_tf)) {
      df <- df %>%
        filter(PY >= input$rango_tf[1],
               PY <= input$rango_tf[2])
    }
    
    req(input$variable_tf)
    
    # Nombre de la variable seleccionada
    var_sel <- input$variable_tf
    
    # Detectar si la columna tiene múltiples valores separados por ";"
    contiene_multi <- any(grepl(";", df[[var_sel]]), na.rm = TRUE)
    
    # ==========================================================
    # CASO 1: Variables con múltiples valores (;)
    # ==========================================================
    if (contiene_multi) {
      
      df <- df %>%
        filter(!is.na(.data[[var_sel]])) %>%
        tidyr::separate_rows(all_of(var_sel), sep = ";") %>%
        mutate(Valor = trimws(.data[[var_sel]])) %>%
        group_by(Valor) %>%
        summarise(Frecuencia = n(), .groups = "drop")
      
    } else {
      
      # ==========================================================
      # CASO 2: Variables normales
      # ==========================================================
      df <- df %>%
        filter(!is.na(.data[[var_sel]])) %>%
        group_by(.data[[var_sel]]) %>%
        summarise(Frecuencia = n(), .groups = "drop")
      
      colnames(df)[1] <- "Valor"
    }
    
    # Agregar ID
    df <- df %>%
      mutate(id = row_number()) %>%
      select(id, Variable = Valor, Frecuencia)
    
    # ==========================================================
    # TOP / BOTTOM / TODOS
    # ==========================================================
    if (input$modo_tf != "Todos") {
      
      if (input$modo_tf == "Top") {
        df <- df %>% arrange(desc(Frecuencia))
      }
      
      if (input$modo_tf == "Bottom") {
        df <- df %>% arrange(Frecuencia)
      }
      
      if (!is.null(input$n_datos_tf) && input$n_datos_tf > 0) {
        df <- head(df, input$n_datos_tf)
      }
    }
    
    df
  })
  
  
  # ===========================================================================
  # 10. GRÁFICA DE FRECUENCIAS
  # ===========================================================================
  grafica_tf <- reactive({
    df <- datos_filtrados_tf()
    if (nrow(df)==0) return(NULL)
    if (input$tipo_grafico_tf=="Barras") {
      ggplot(df, aes(x=reorder(Variable,Frecuencia), y=Frecuencia)) +
        geom_col(fill=input$color_barras_tf) +
        coord_flip() +
        scale_y_continuous(breaks=pretty(c(0,max(df$Frecuencia)), n=20)) +
        labs(x="Variable", y="Frecuencia") +
        theme_minimal() +
        theme(axis.text.y=element_text(hjust=1), axis.text.x=element_text(angle=45,hjust=1))
    } else {
      # Gráfica tipo pastel
      ggplot(df, aes(x="", y=Frecuencia, fill=Variable)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void()
    }
  })
  
  output$grafica_tf <- renderPlot({ grafica_tf() })
  
  
  # ===========================================================================
  # 11. TABLA DE FRECUENCIAS Y DESCARGAS
  # ===========================================================================
  output$tabla_tf <- renderDT({
    df <- datos_filtrados_tf()
    datatable(df, options=list(pageLength=10,
                               lengthMenu=list(c(10,25,50,100,-1), c('10','25','50','100','Todos'))),
              rownames=FALSE)
  })
  
  output$descargar_tf <- downloadHandler(
    filename = function() paste0("frecuencia_", Sys.Date(), ".", tolower(input$formato_descarga_tf)),
    content = function(file){
      formato <- tolower(input$formato_descarga_tf)
      if (formato %in% c("png","jpeg","jpg")) ggsave(file, plot=grafica_tf(), width=10,height=6,dpi=300,bg="white", device=formato)
      else if (formato=="pdf") ggsave(file, plot=grafica_tf(), width=10,height=6,bg="white")
      else {
        df <- datos_filtrados_tf()
        if (formato=="csv") write.csv(df, file,row.names=FALSE)
        else if (formato=="xlsx") write.xlsx(df, file)
        else if (formato=="txt") write.table(df,file,sep="\t",row.names=FALSE)
      }
    }
  )
  
  
  # ===========================================================================
  # 12. MODAL PARA GRÁFICA AMPLIADA DE FRECUENCIAS
  # ===========================================================================
  observeEvent(input$ampliar_tf, { showModal(modalDialog(title="Gráfica ampliada", plotOutput("grafica_tf_grande", height="700px"), size="l", easyClose=TRUE)) })
  output$grafica_tf_grande <- renderPlot({ grafica_tf() })
}

###############################################################################
# FIN DEL ARCHIVO
###############################################################################


# ============================
#        EJECUCIÓN APP
# ============================
#shinyApp(ui = ui, server = server) #🟥🟥🟥ESCONDÍ ESTO 
