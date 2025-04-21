library(shiny)
library(readxl)
library(ggplot2)
library(bslib)
library(stats)
library(nortest)
library(dplyr)
library(multcompView)
library(emmeans)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  titlePanel("STATSEASE"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Sube tu archivo de datos (.xlsx)", accept = ".xlsx"),
      textAreaInput("manual_data", "O ingresa datos manualmente (CSV):", rows = 5),
      uiOutput("var_dep_ui"),
      uiOutput("var_grp_ui"),
      selectInput("prueba", "Selecciona una prueba estadística:", 
                  choices = c("Chi-cuadrado", "McNemar", "Kolmogorov-Smirnov", 
                              "Shapiro-Wilk", "Anderson-Darling", "t de Student", 
                              "ANOVA", "Wilcoxon", "Kruskal-Wallis")),
      actionButton("run", "Ejecutar Prueba"),
      width = 3
    ),
    mainPanel(
      h4("Resultado de la Prueba"),
      verbatimTextOutput("resultado"),
      h4("Interpretación"),
      verbatimTextOutput("interpretacion"),
      plotOutput("boxplot"),
      h4("Gráfico de Resultados (según prueba)"),
      plotOutput("prueba_plot")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    if (!is.null(input$file1)) {
      read_excel(input$file1$datapath, sheet = 1)
    } else if (input$manual_data != "") {
      read.csv(text = input$manual_data, sep = ",", stringsAsFactors = TRUE)
    } else {
      NULL
    }
  })
  
  output$var_dep_ui <- renderUI({
    req(datos())
    nums <- names(datos())[sapply(datos(), is.numeric)]
    selectInput("var_dep", "Variable dependiente:", choices = nums)
  })
  
  output$var_grp_ui <- renderUI({
    req(datos())
    cats <- names(datos())[sapply(datos(), function(x) is.factor(x) || is.character(x))]
    selectInput("var_grp", "Variable de grupo:", choices = cats)
  })
  
  resultado <- eventReactive(input$run, {
    df <- datos()
    dep <- input$var_dep
    grp <- input$var_grp
    prueba <- input$prueba
    
    if (prueba == "Chi-cuadrado") {
      tabla <- table(df[[dep]], df[[grp]])
      test <- chisq.test(tabla)
      list(test = test, tipo = "Chi-cuadrado")
    } else if (prueba == "McNemar") {
      tabla <- table(df[[dep]], df[[grp]])
      test <- mcnemar.test(tabla)
      list(test = test, tipo = "McNemar")
    } else if (prueba == "Kolmogorov-Smirnov") {
      test <- ks.test(df[[dep]], df[[grp]])
      list(test = test, tipo = "Kolmogorov-Smirnov")
    } else if (prueba == "Shapiro-Wilk") {
      test <- shapiro.test(df[[dep]])
      list(test = test, tipo = "Shapiro-Wilk")
    } else if (prueba == "Anderson-Darling") {
      test <- ad.test(df[[dep]])
      list(test = test, tipo = "Anderson-Darling")
    } else if (prueba == "t de Student") {
      test <- t.test(df[[dep]] ~ df[[grp]])
      list(test = test, tipo = "t de Student")
    } else if (prueba == "ANOVA") {
      modelo <- aov(as.formula(paste0("`", dep, "` ~ `", grp, "`")), data = df)
      posthoc <- TukeyHSD(modelo)
      list(test = summary(modelo), tipo = "ANOVA", posthoc = posthoc)
    } else if (prueba == "Wilcoxon") {
      test <- wilcox.test(df[[dep]] ~ df[[grp]])
      list(test = test, tipo = "Wilcoxon")
    } else if (prueba == "Kruskal-Wallis") {
      test <- kruskal.test(df[[dep]] ~ df[[grp]])
      list(test = test, tipo = "Kruskal-Wallis")
    }
  })
  
  output$resultado <- renderPrint({
    req(resultado())
    cat(sprintf("Resultado de la prueba: %s\n", resultado()$tipo))
    print(resultado()$test)
    if (!is.null(resultado()$posthoc)) {
      cat("\nPost-hoc Tukey:\n")
      print(resultado()$posthoc)
    }
  })
  
  output$interpretacion <- renderPrint({
    req(resultado())
    p <- if (!is.null(resultado()$test$p.value)) resultado()$test$p.value else NA
    if (!is.na(p)) {
      if (p < 0.05) {
        cat("El resultado es significativo (p < 0.05). Se rechaza la H0.")
      } else {
        cat("El resultado no es significativo (p >= 0.05). No se rechaza la H0.")
      }
    }
  })
  
  output$boxplot <- renderPlot({
    req(resultado())
    if (input$prueba %in% c("ANOVA", "t de Student", "Wilcoxon", "Kruskal-Wallis")) {
      df <- datos()
      ggplot(df, aes_string(x = input$var_grp, y = input$var_dep, fill = input$var_grp)) +
        geom_boxplot(alpha = 0.8) +
        theme_minimal() +
        labs(title = "Distribución de la variable por grupo")
    }
  })
  
  output$prueba_plot <- renderPlot({
    req(resultado())
    df <- datos()
    df[[input$var_grp]] <- as.factor(df[[input$var_grp]])
    
    switch(input$prueba,
           "Chi-cuadrado" = {
             tabla <- table(df[[input$var_dep]], df[[input$var_grp]])
             barplot(tabla, beside = TRUE, col = rainbow(ncol(tabla)),
                     legend = TRUE, args.legend = list(x = "topright"),
                     main = "Distribución de Frecuencias")
           },
           "McNemar" = {
             tabla <- table(df[[input$var_dep]], df[[input$var_grp]])
             barplot(tabla, beside = TRUE, col = c("orange", "purple"),
                     legend = TRUE, args.legend = list(x = "topright"),
                     main = "Gráfico para prueba de McNemar")
           },
           "Kolmogorov-Smirnov" = {
             plot.ecdf(df[[input$var_dep]], main = "Función de Distribución Empírica",
                       xlab = input$var_dep, col = "blue")
           },
           "Shapiro-Wilk" = {
             qqnorm(df[[input$var_dep]], main = "Gráfico Q-Q Shapiro-Wilk")
             qqline(df[[input$var_dep]], col = "red")
           },
           "Anderson-Darling" = {
             qqnorm(df[[input$var_dep]], main = "Q-Q Anderson-Darling")
             qqline(df[[input$var_dep]], col = "red")
           },
           "t de Student" = {
             ggplot(df, aes_string(x = input$var_grp, y = input$var_dep, fill = input$var_grp)) +
               geom_boxplot(alpha = 0.6) +
               geom_jitter(width = 0.2) +
               theme_minimal() +
               labs(title = "Comparación de Medias (t de Student)", x = "", y = "")
           },
           "Wilcoxon" = {
             ggplot(df, aes_string(x = input$var_grp, y = input$var_dep, fill = input$var_grp)) +
               geom_violin(trim = FALSE) +
               geom_boxplot(width = 0.1, fill = "white") +
               theme_minimal() +
               labs(title = "Comparación de Grupos (Wilcoxon)", x = "", y = "")
           },
           "ANOVA" = {
             ggplot(df, aes_string(x = input$var_grp, y = input$var_dep, fill = input$var_grp)) +
               geom_boxplot() +
               theme_minimal() +
               labs(title = "Boxplot ANOVA", x = "", y = "")
           },
           "Kruskal-Wallis" = {
             ggplot(df, aes_string(x = input$var_grp, y = input$var_dep, fill = input$var_grp)) +
               geom_violin(trim = FALSE, alpha = 0.6) +
               geom_jitter(width = 0.2, size = 1.5) +
               theme_minimal() +
               labs(title = "Comparación de Grupos (Kruskal-Wallis)", x = "", y = "")
           }
    )
  })
}

shinyApp(ui, server)
