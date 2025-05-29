library(shiny)
a<-22
git_data_repo_url <- "https://github.com/Edxon20/ventas_proyecto_gerencia.git"
directorio_temporal_base <- "/tmp"
nombre_directorio_data_repo <- paste0(directorio_temporal_base, "/ventas_proyecto_gerencia_data_app_V2")

cat("- INICIO APP SHINY -\n")
cat("Directorio de datos a crear:", nombre_directorio_data_repo, "\n")
flush.console()

if (dir.exists(nombre_directorio_data_repo)) {
  cat("Directorio de datos ya existe. Eliminándolo para clonación fresca.\n")
  unlink(nombre_directorio_data_repo, recursive = TRUE, force = TRUE)
  flush.console()
}

cat("Intentando clonar:", git_data_repo_url, "en", nombre_directorio_data_repo, "\n")
flush.console()
comando_clonar <- paste("git clone --depth 1", git_data_repo_url, nombre_directorio_data_repo)
# Deja que git imprima su salida para ver errores
exit_status_clonar <- system(comando_clonar, intern = FALSE) 

cat("Código de salida de Git:", exit_status_clonar, "\n")
cat("Verificando si el directorio fue creado:", nombre_directorio_data_repo, "\n")
print(paste("¿Directorio existe?:", dir.exists(nombre_directorio_data_repo)))
flush.console()

if (dir.exists(nombre_directorio_data_repo)) {
  cat("Contenido del directorio clonado (", nombre_directorio_data_repo, "):\n")
  print(list.files(nombre_directorio_data_repo, recursive = TRUE)) 
} else {
  cat("El directorio NO se creó después de la clonación o el código de salida no fue 0.\n")
}
flush.console()

if (exit_status_clonar != 0 || !dir.exists(nombre_directorio_data_repo)) {
  stop(paste("CRITICO: Fallo al clonar el repositorio de datos. Código de Git:", exit_status_clonar))
}

ruta_mcmc_matrix <- paste0(nombre_directorio_data_repo, "/mcmc_matrix_ventas_producto_FINAL.rds")
cat("Intentando cargar MCMC desde:", ruta_mcmc_matrix, "\n")
flush.console()

mcmc_mat_cargada <- NULL
tryCatch({
  mcmc_mat_cargada <- readRDS(ruta_mcmc_matrix)
  if(is.null(mcmc_mat_cargada) || !is.matrix(mcmc_mat_cargada)){
    stop("Archivo mcmc_matrix cargado no es matriz o es NULL.")
  }
  cat("Matriz MCMC cargada exitosamente.\n")
}, error = function(e) {
  stop(paste("CRITICO: Fallo al cargar la matriz MCMC desde '", ruta_mcmc_matrix, "': ", conditionMessage(e), sep=""))
})
flush.console()


predecir_ventas_semana_directo <- function(plantilla_feriados_semana, mcmc_samples_matrix) {
  if (length(plantilla_feriados_semana) != 7) {
    stop("La plantilla de feriados debe contener exactamente 7 valores (Lunes a Domingo).")
  }
  if (!all(plantilla_feriados_semana %in% c(0, 1))) {
    stop("Los valores en la plantilla de feriados deben ser 0 (No Feriado) o 1 (Sí Feriado).")
  }
  beta0_samples <- mcmc_samples_matrix[, "beta0"]
  beta_mar_samples <- mcmc_samples_matrix[, "beta_mar"]; beta_mie_samples <- mcmc_samples_matrix[, "beta_mie"]
  beta_jue_samples <- mcmc_samples_matrix[, "beta_jue"]; beta_vie_samples <- mcmc_samples_matrix[, "beta_vie"]
  beta_sab_samples <- mcmc_samples_matrix[, "beta_sab"]; beta_dom_samples <- mcmc_samples_matrix[, "beta_dom"]
  beta_feriado_samples <- mcmc_samples_matrix[, "beta_feriado"]
  beta_int_sab_fer_samples <- NULL; beta_int_dom_fer_samples <- NULL
  usa_int_sab_fer <- "beta_int_sab_fer" %in% colnames(mcmc_samples_matrix)
  usa_int_dom_fer <- "beta_int_dom_fer" %in% colnames(mcmc_samples_matrix)
  if (usa_int_sab_fer) {beta_int_sab_fer_samples <- mcmc_samples_matrix[, "beta_int_sab_fer"]}
  if (usa_int_dom_fer) {beta_int_dom_fer_samples <- mcmc_samples_matrix[, "beta_int_dom_fer"]}
  n_samples <- nrow(mcmc_samples_matrix)
  mu_semana_total_samples <- numeric(n_samples)
  dias_betas_samples <- list(NULL, beta_mar_samples, beta_mie_samples, beta_jue_samples, beta_vie_samples, beta_sab_samples, beta_dom_samples)
  for (i in 1:n_samples) {
    ventas_diarias_simuladas_esta_semana <- numeric(7)
    for (dia_idx in 1:7) {
      es_feriado_actual <- plantilla_feriados_semana[dia_idx]
      log_mu_dia <- beta0_samples[i]
      if (dia_idx > 1) {log_mu_dia <- log_mu_dia + dias_betas_samples[[dia_idx]][i]}
      log_mu_dia <- log_mu_dia + beta_feriado_samples[i] * es_feriado_actual
      if (dia_idx == 6 && usa_int_sab_fer && es_feriado_actual == 1) {log_mu_dia <- log_mu_dia + beta_int_sab_fer_samples[i]}
      if (dia_idx == 7 && usa_int_dom_fer && es_feriado_actual == 1) {log_mu_dia <- log_mu_dia + beta_int_dom_fer_samples[i]}
      ventas_diarias_simuladas_esta_semana[dia_idx] <- exp(log_mu_dia)
    }
    mu_semana_total_samples[i] <- sum(ventas_diarias_simuladas_esta_semana)
  }
  mu_semana_total_samples_finitos <- mu_semana_total_samples[is.finite(mu_semana_total_samples)]
  if (length(mu_semana_total_samples_finitos) == 0) {
    stop("Todas las ventas semanales simuladas resultaron en valores no finitos. Revisa las muestras MCMC.")
  }
  mediana_semanal <- median(mu_semana_total_samples_finitos)
  media_semanal <- mean(mu_semana_total_samples_finitos)
  intervalo_credibilidad_95 <- quantile(mu_semana_total_samples_finitos, probs = c(0.025, 0.975), na.rm = TRUE)
  return(list(
    plantilla_feriados = plantilla_feriados_semana,
    peor_caso_inf_95 = intervalo_credibilidad_95[1],
    prediccion_promedio_mediana = mediana_semanal,
    promedio_media = media_semanal,
    mejor_caso_sup_95 = intervalo_credibilidad_95[2]
  ))
}

graficar_predicciones_diarias_compacto <- function(mcmc_samples_matrix) {
  nombres_betas_principales <- c("beta0", "beta_mar", "beta_mie", "beta_jue", "beta_vie", "beta_sab", "beta_dom", "beta_feriado")
  if (!all(nombres_betas_principales %in% colnames(mcmc_samples_matrix))) {
    stop("Faltan uno o más coeficientes beta principales en la matriz MCMC.")
  }
  beta0_samples <- mcmc_samples_matrix[, "beta0"]
  beta_mar_samples <- mcmc_samples_matrix[, "beta_mar"]; beta_mie_samples <- mcmc_samples_matrix[, "beta_mie"]
  beta_jue_samples <- mcmc_samples_matrix[, "beta_jue"]; beta_vie_samples <- mcmc_samples_matrix[, "beta_vie"]
  beta_sab_samples <- mcmc_samples_matrix[, "beta_sab"]; beta_dom_samples <- mcmc_samples_matrix[, "beta_dom"]
  beta_feriado_samples <- mcmc_samples_matrix[, "beta_feriado"]
  beta_int_sab_fer_samples <- NULL; beta_int_dom_fer_samples <- NULL
  usa_int_sab_fer <- "beta_int_sab_fer" %in% colnames(mcmc_samples_matrix)
  usa_int_dom_fer <- "beta_int_dom_fer" %in% colnames(mcmc_samples_matrix)
  if (usa_int_sab_fer) {beta_int_sab_fer_samples <- mcmc_samples_matrix[, "beta_int_sab_fer"]}
  if (usa_int_dom_fer) {beta_int_dom_fer_samples <- mcmc_samples_matrix[, "beta_int_dom_fer"]}
  n_samples <- nrow(mcmc_samples_matrix)
  dias_nombres_cortos <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")
  dias_betas_samples <- list(NULL, beta_mar_samples, beta_mie_samples, beta_jue_samples, beta_vie_samples, beta_sab_samples, beta_dom_samples)
  resultados_prediccion <- data.frame(
    Escenario = character(), DiaSemana = character(), EsFeriado = character(),
    PeorCaso = numeric(), Mediana = numeric(), MejorCaso = numeric(),
    Orden = numeric(), stringsAsFactors = FALSE )
  orden_grafico <- 1
  for (es_feriado_val in c(0, 1)) {
    sufijo_feriado <- if (es_feriado_val == 1) "F" else "NF"
    for (dia_idx in 1:7) {
      log_mu_dia_samples_actual <- numeric(n_samples)
      for (i in 1:n_samples) {
        log_mu_dia <- beta0_samples[i]
        if (dia_idx > 1) {log_mu_dia <- log_mu_dia + dias_betas_samples[[dia_idx]][i]}
        log_mu_dia <- log_mu_dia + beta_feriado_samples[i] * es_feriado_val
        if (dia_idx == 6 && usa_int_sab_fer && es_feriado_val == 1) {log_mu_dia <- log_mu_dia + beta_int_sab_fer_samples[i]}
        if (dia_idx == 7 && usa_int_dom_fer && es_feriado_val == 1) {log_mu_dia <- log_mu_dia + beta_int_dom_fer_samples[i]}
        log_mu_dia_samples_actual[i] <- log_mu_dia
      }
      mu_dia_samples_actual <- exp(log_mu_dia_samples_actual)
      mu_dia_samples_finitos <- mu_dia_samples_actual[is.finite(mu_dia_samples_actual)]
      if (length(mu_dia_samples_finitos) == 0) {next}
      mediana_dia_actual <- median(mu_dia_samples_finitos)
      intervalo_actual <- quantile(mu_dia_samples_finitos, probs = c(0.025, 0.975), na.rm = TRUE)
      nombre_escenario_corto <- paste(dias_nombres_cortos[dia_idx], sufijo_feriado)
      resultados_prediccion <- rbind(resultados_prediccion, data.frame(
        Escenario = nombre_escenario_corto, DiaSemana = dias_nombres_cortos[dia_idx],
        EsFeriado = sufijo_feriado, PeorCaso = intervalo_actual[1],
        Mediana = mediana_dia_actual, MejorCaso = intervalo_actual[2],
        Orden = orden_grafico ))
      orden_grafico <- orden_grafico + 1
    }
  }
  if (nrow(resultados_prediccion) == 0) {
    plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "No hay datos para graficar."); return(invisible(NULL))
  }
  resultados_prediccion$DiaSemanaFactor <- factor(resultados_prediccion$DiaSemana, levels = dias_nombres_cortos)
  resultados_prediccion$EsFeriadoFactor <- factor(resultados_prediccion$EsFeriado, levels = c("NF", "F"))
  resultados_prediccion <- resultados_prediccion[order(resultados_prediccion$EsFeriadoFactor, resultados_prediccion$DiaSemanaFactor), ]
  resultados_prediccion$OrdenEjeY <- nrow(resultados_prediccion):1
  margenes_originales <- par("mar"); on.exit(par(mar = margenes_originales))
  par(mar = c(5.1, 7.1, 4.1, 2.1))
  rango_x <- range(c(0, resultados_prediccion$PeorCaso, resultados_prediccion$MejorCaso), na.rm = TRUE)
  if(any(!is.finite(rango_x)) || diff(rango_x) == 0) rango_x <- c(0, max(100, max(resultados_prediccion$MejorCaso, na.rm=TRUE)*1.1, na.rm=TRUE) )
  rango_x[2] <- max(rango_x[2] * 1.12, rango_x[1] + 10)
  rango_y <- c(0.5, nrow(resultados_prediccion) + 0.5)
  colores_puntos <- ifelse(resultados_prediccion$EsFeriado == "F", "orangered", "royalblue")
  colores_segmentos <- ifelse(resultados_prediccion$EsFeriado == "F", "lightcoral", "lightskyblue")
  plot(1, type="n", xlim = rango_x, ylim = rango_y, xlab = "Ventas Estimadas de Perros Calientes", ylab = "",
       main = "Predicción de Ventas Diarias (Intervalo de Credibilidad 95%)", yaxt = "n")
  abline(v = pretty(rango_x), col = "lightgray", lty = "dotted"); abline(h = resultados_prediccion$OrdenEjeY, col = "lightgray", lty = "dotted")
  segments(x0 = resultados_prediccion$PeorCaso, y0 = resultados_prediccion$OrdenEjeY, x1 = resultados_prediccion$MejorCaso, y1 = resultados_prediccion$OrdenEjeY,
           col = colores_segmentos, lwd = 3)
  points(x = resultados_prediccion$Mediana, y = resultados_prediccion$OrdenEjeY, pch = 18, col = colores_puntos, cex = 1.2)
  axis(side = 2, at = resultados_prediccion$OrdenEjeY, labels = resultados_prediccion$Escenario, las = 1, cex.axis = 0.75)
  legend("topright", inset=c(0.01, 0.01), legend = c("Mediana (No Fer.)", "Rango 95% (No Fer.)", "Mediana (Fer.)", "Rango 95% (Fer.)"),
         col = c("royalblue", "lightskyblue", "orangered", "lightcoral"), pch = c(18, NA, 18, NA),
         lty = c(NA, 1, NA, 1), lwd = c(NA, 3, NA, 3), cex = 0.7, bg="white", box.col = "gray")
}

ui <- fluidPage(
  titlePanel("Dashboard de Predicción de Ventas de Perros Calientes"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Predicción Semanal Interactiva:"),
      p("Ingrese 7 números (0 o 1) separados por comas para la semana (Lun a Dom). 1=Feriado, 0=No Feriado."),
      textInput("plantilla_input_shiny",
                label = "Plantilla Semanal (ej: 0,0,0,1,1,0,0):",
                value = "0,0,0,0,0,0,0"),
      actionButton("predict_button_shiny", "Calcular Predicción Semanal", icon = icon("calculator")),
      hr(),
      h4("Vista General Diaria:"),
      p("Presiona para generar/actualizar el gráfico que muestra la predicción para todos los escenarios diarios."),
      actionButton("show_daily_plot_button", "Mostrar Gráfico General Diario", icon = icon("chart-bar"))
    ),
    mainPanel(
      width = 8,
      h3(icon("bullseye"), "Resultados de la Predicción Semanal:"),
      verbatimTextOutput("prediccion_texto_output_shiny"),
      hr(),
      h3(icon("calendar-alt"), "Gráfico General de Predicciones Diarias:"),
      plotOutput("grafico_general_diario_output", height = "650px", width="100%")
    )
  )
)

server <- function(input, output, session) {
  
  if (is.null(mcmc_mat_cargada) || !exists("mcmc_mat_cargada") || !is.matrix(mcmc_mat_cargada)) {
    showNotification("Error: 'mcmc_mat_cargada' no está disponible o no es una matriz. La app no funcionará.", type = "error", duration = NULL)
    return()
  }
  
  prediccion_semanal_reactiva <- eventReactive(input$predict_button_shiny, {
    req(input$plantilla_input_shiny)
    plantilla_vector_shiny <- tryCatch({
      temp_vector <- as.numeric(unlist(strsplit(input$plantilla_input_shiny, ",")))
      if (length(temp_vector) != 7 || !all(temp_vector %in% c(0,1)) || any(is.na(temp_vector))) {
        stop("Plantilla inválida. Use 7 números (0/1) separados por comas.")
      }
      temp_vector
    }, error = function(e) {
      showNotification(paste("Error en la plantilla:", e$message), type = "error", duration = 7)
      return(NULL)
    })
    req(plantilla_vector_shiny)
    prediccion_actual <- predecir_ventas_semana_directo(plantilla_vector_shiny, mcmc_mat_cargada)
    return(prediccion_actual)
  })
  
  output$prediccion_texto_output_shiny <- renderPrint({
    pred_data <- prediccion_semanal_reactiva()
    if (!is.null(pred_data)) {
      texto_salida <- paste0(
        "Para la plantilla de feriados: [", paste(pred_data$plantilla_feriados, collapse=", "), "]\n",
        "(Donde 0=No Feriado, 1=Sí Feriado, para Lunes a Domingo)\n\n",
        sprintf("  - En el PEOR DE LOS CASOS (2.5%%): %.2f unidades.\n", pred_data$peor_caso_inf_95),
        sprintf("  - La PREDICCIÓN PROMEDIO (mediana) es de: %.2f unidades.\n", pred_data$prediccion_promedio_mediana),
        sprintf("  - En el MEJOR DE LOS CASOS (97.5%%): %.2f unidades.\n\n", pred_data$mejor_caso_sup_95),
        sprintf("Resumen: Espera vender alrededor de %.0f, probablemente entre %.0f y %.0f unidades esta semana.\n",
                pred_data$prediccion_promedio_mediana, pred_data$peor_caso_inf_95, pred_data$mejor_caso_sup_95)
      )
      texto_salida
    } else {
      "Por favor, ingresa una plantilla semanal válida y presiona 'Calcular Predicción Semanal'."
    }
  })
  
  output$grafico_general_diario_output <- renderPlot({
    input$show_daily_plot_button
    if (!is.null(mcmc_mat_cargada) && is.matrix(mcmc_mat_cargada)) {
      graficar_predicciones_diarias_compacto(mcmc_mat_cargada)
    } else {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1,1, "Datos del modelo (mcmc_mat_cargada) no disponibles para graficar.", col="red", cex=1.2)
    }
  })
}

shinyApp(ui = ui, server = server)