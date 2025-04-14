library(shiny)
library(readxl)
library(ggplot2)
library(bslib)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),  # tema estético
  
  titlePanel("Comparación ANOVA + Tukey (Excel)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Sube tu archivo Excel (.xlsx)", accept = ".xlsx"),
      uiOutput("var_dep_ui"),
      uiOutput("var_grp_ui"),
      actionButton("run", "Ejecutar ANOVA"),
      width = 3
    ),
    mainPanel(
      h4("Resumen ANOVA"),
      verbatimTextOutput("anova"),
      h4("Tukey HSD"),
      verbatimTextOutput("tukey"),
      h4("Boxplot por Grupo"),
      plotOutput("boxplot"),
      h4("Gráfico Tukey HSD"),
      plotOutput("tukey_plot")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath, sheet = 1)
  })
  
  output$var_dep_ui <- renderUI({
    df <- datos()
    nums <- names(df)[sapply(df, is.numeric)]
    selectInput("var_dep", "Variable dependiente:", choices = nums)
  })
  
  output$var_grp_ui <- renderUI({
    df <- datos()
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    selectInput("var_grp", "Variable de grupo:", choices = cats)
  })
  
  resultado <- eventReactive(input$run, {
    df  <- datos()
    dep <- input$var_dep
    grp <- input$var_grp
    
    df[[dep]] <- as.numeric(df[[dep]])
    df[[grp]] <- as.factor(df[[grp]])
    
    modelo <- aov(stats::as.formula(paste0("`", dep, "` ~ `", grp, "`")), data = df)
    
    list(
      data = df,
      dep = dep,
      grp = grp,
      modelo = modelo,
      anova = summary(modelo),
      tukey = TukeyHSD(modelo)
    )
  })
  
  output$anova <- renderPrint({
    req(resultado())
    resultado()$anova
  })
  
  output$tukey <- renderPrint({
    req(resultado())
    resultado()$tukey
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    req(resultado())
    df <- resultado()$data
    dep <- resultado()$dep
    grp <- resultado()$grp
    
    ggplot(df, aes_string(x = grp, y = dep, fill = grp)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      labs(x = grp, y = dep, title = "Distribución de la variable por grupo") +
      theme(legend.position = "none")
  })
  
  # Gráfico Tukey 
  output$tukey_plot <- renderPlot({
    req(resultado())
    tukey <- resultado()$tukey
    plot(tukey, las = 1, col = "darkorange", cex.axis = 1.1)
  })
}

shinyApp(ui, server)
