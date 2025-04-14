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
      plotOutput("tukey_plot"),
      h4("Reporte de Interpretación ANOVA"),
      verbatimTextOutput("interpretacion_anova")
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
  
  output$interpretacion_anova <- renderPrint({
    req(resultado())
    
    modelo <- resultado()$modelo
    anova_res <- summary(modelo)[[1]]
    dep <- resultado()$dep
    grp <- resultado()$grp
    
    # Valores claves
    df1 <- anova_res[["Df"]][1]
    df2 <- anova_res[["Df"]][2]
    f_valor <- anova_res[["F value"]][1]
    p_valor <- anova_res[["Pr(>F)"]][1]
    ss_total <- sum(anova_res[["Sum Sq"]])
    ss_efecto <- anova_res[["Sum Sq"]][1]
    eta2 <- ss_efecto / ss_total
    
    cat("📊 **Informe de Resultados del ANOVA**\n\n")
    cat("Se realizó un análisis de varianza (ANOVA) para evaluar si existen diferencias significativas en la variable **", dep, "** según los niveles del factor **", grp, "**.\n\n", sep = "")
    
    cat("🔎 Resultado del modelo:\n")
    cat(sprintf(" - Estadístico F(%d, %d) = %.2f\n", df1, df2, f_valor))
    cat(sprintf(" - Valor p = %s\n", ifelse(p_valor < 0.001, "< .001", paste0("= ", signif(p_valor, 3)))))
    cat(sprintf(" - Tamaño del efecto (Eta²) = %.3f → ", eta2))
    
    if (eta2 >= 0.14) {
      cat("**grande**\n")
    } else if (eta2 >= 0.06) {
      cat("**moderado**\n")
    } else if (eta2 >= 0.01) {
      cat("**pequeño**\n")
    } else {
      cat("**muy pequeño**\n")
    }
    
    cat("\n📌 Interpretación del valor p:\n")
    if (p_valor < 0.05) {
      cat("El valor p es menor al nivel de significancia (0.05), lo cual indica que **existen diferencias significativas** entre al menos dos grupos.\n")
      cat("✅ **Conclusión:** Se **rechaza** la hipótesis nula.\n")
    } else {
      cat("El valor p es mayor a 0.05, lo que indica que no hay evidencia suficiente para afirmar que existan diferencias.\n")
      cat("❌ **Conclusión:** Se **acepta** la hipótesis nula (no se rechaza).\n")
    }
    
    # Mostrar resultado resumido claro
    cat("\n📢 **Resultado Final:** ")
    if (p_valor < 0.05) {
      cat("➡️ Se **rechaza** la hipótesis nula.\n")
    } else {
      cat("➡️ Se **acepta** la hipótesis nula.\n")
    }
    
    # Post-hoc Tukey si el ANOVA fue significativo
    if (p_valor < 0.05) {
      cat("\n🔍 **Análisis post-hoc (Tukey HSD):**\n")
      tukey_df <- as.data.frame(resultado()$tukey[[1]])
      signif_comps <- rownames(tukey_df)[tukey_df$`p adj` < 0.05]
      
      if (length(signif_comps) > 0) {
        cat("Se encontraron diferencias significativas entre los siguientes pares de grupos:\n")
        for (comp in signif_comps) {
          cat(" - ", comp, " (p =", signif(tukey_df[comp, "p adj"], 3), ")\n", sep = "")
        }
      } else {
        cat("No se detectaron comparaciones significativas entre pares de grupos (p ≥ 0.05).\n")
      }
    } else {
      cat("\nℹ️ **Nota:** No se realiza el análisis de comparaciones múltiples (Tukey) porque el efecto principal no fue significativo.\n")
    }
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

