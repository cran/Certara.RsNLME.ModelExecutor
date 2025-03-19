material_collapsible_item <- function(label, ..., icon = NULL, 
                                      active = FALSE) {
  tags$li(
    tags$div(
      class = paste("collapsible-header", if (active) "active"),
      if (!is.null(icon))
        tags$i(class = "material-icons", icon),
      label
    ),
    tags$div(
      class = "collapsible-body",
      tags$span(
        ...
      )
    )
  )
}

material_collapsible <- function(..., depth = NULL, color = NULL, 
                                 type = NULL) {
  tags$ul(
    class = paste(
      "collapsible",
      if (!is.null(type)) type,
      if (!is.null(depth)) paste0("z-depth-", depth),
      if (!is.null(color)) color),
    ...
  )
}

#' @importFrom future future multisession plan value
#' @importFrom fs path_home
#' @import shiny 
#' @importFrom shinyFiles getVolumes parseSavePath shinyFileSave shinySaveButton
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
#' @importFrom  htmltools span tags HTML div h4 h5 h6
#' @import promises
#' @import Certara.RsNLME
#' @import shinymeta
#' @rawNamespace import(shinyjs, except = c(runExample))
#' @import ggplot2
#' @importFrom magrittr %>% %<>%
#' @importFrom stringr str_split str_replace str_extract_all
#' @importFrom DT renderDT DTOutput

.run_shiny_RsNLME <- function(model, hosts, allow_gaussian_fit, sub_models,
                              mmdl_file_directory, temp_dir, outputfile, 
                              metamodel_file_name, sort_cols, from_pirana) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package \"bslib\" required to use modelExecutorUI()",
         call. = FALSE
    )
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" required to use modelExecutorUI()",
         call. = FALSE
    )
  }

  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("bslib", quietly = TRUE)

  plan(multisession)
  process_start_time <- NULL
  
  if (model@isPopulation && model@columnMapping@mapping[["id"]]@columnName == "?") {
    model@isPopulation <- FALSE
  }

  ui <- generate_ui(hosts , sort_cols, model@covariateList, model@isPopulation)

  server <- function(input, output, session){

    shinyjs::disable(id = "pk_conf_level")
    shinyjs::hideElement(id = "pk_sort_col2", asis = FALSE)
    shinyjs::hideElement(id = "pk_sort_col3", asis = FALSE)
    shinyjs::hideElement(id = "pk_sort_col4", asis = FALSE)
    shinyjs::hideElement(id = "pk_sort_col5", asis = FALSE)
    
    logfiledir <- reactiveVal(NULL)
    logfilename <- reactiveVal(NULL)
    pidfilename <- ''
    
    output$execution_overview <- renderUI({
      host <- hosts[unlist(lapply(hosts, function (x) x@hostName == input$pk_execute_on))][[1]]
         host_overview <- tagList(
            div(class = "modal-div",
              style = "padding-left: 15px",
                h5(paste0("Host: ", host@hostName)),
                h5(paste0("Shared Directory: ", host@sharedDirectory))
            )
          )
        
        host_overview
      })
    
    outputOptions(output, "execution_overview", suspendWhenHidden = FALSE)
    
    execution_error <- reactiveVal(NULL)
    execution_start <- reactiveVal(NULL)
    execution_time <- reactiveVal(NULL)

    output$execution_start <- renderText({
      if (input$pk_sort_col1 != " ") {
        note <-
          paste0(
            "\n\nNote: Real time convergence data and plot output is not available for ",
            input$pk_run_mode," run mode with sort column option. Stopping execution from the Shiny GUI is additionally unavailable."
          )
      } else if (input$pk_run_mode != 'Simple') {
        note <-
          paste0(
            "\n\nNote: Real time convergence data and plot output is not available for ",
            input$pk_run_mode, " run mode. Stopping execution from the Shiny GUI is additionally unavailable."
          )
      } else {
        note <- NULL
      }
      execution_note <- paste0("Navigate to the 'Results' tab to monitor execution status...",
                               note)
      execution_note
    })
    
    outputOptions(output, "execution_start", suspendWhenHidden = FALSE)
    
    output$execution_error <- renderText({
      paste0(execution_error(), collapse = "\n")
    })
    
    outputOptions(output, "execution_error", suspendWhenHidden = FALSE)
    
    output$execution_time <- renderUI({
      HTML(execution_time())
    })
    
    outputOptions(output, "execution_time", suspendWhenHidden = FALSE)
    
    observeEvent(input$execution_status, {
      
      showModal(
        modalDialog(
          size = "m",
          title = "Job Overview",
          easyClose = TRUE,
          
          uiOutput("execution_overview"),
          div(
            style = "padding-top: 10px; padding-left: 10px;",
            div(
              style = "display: inline-block;",
              actionButton("button_execute", "Run")
            ),
            div(
              style = "display: inline-block;",
              actionButton("button_stop_execution", "Stop")
            )
          ),
          
          footer = NULL
        )
      )
    })
    
    observe({
      if(is.null(pollData())) {
        shinyjs::enable("button_execute")
        shinyjs::disable("button_stop_execution")
      } else {
        shinyjs::disable("button_execute")
        shinyjs::enable("button_stop_execution")
      }
    })
    
    
    observeEvent(input$button_stop_execution, {
      removeModal()
      showNotification(
        "Stopping execution, please wait...",
        action = NULL,
        duration = 4,
        closeButton = TRUE,
        id = NULL,
        type = "message"
      )
      
      if (!is.null(logfiledir()) && file.exists(logfiledir())) {
        jobs_dir <- file.path(logfiledir(), "jobs")
        stopJobs(jobs_dir)
      }
      
      shinyjs::removeClass(id = "execution_status", class = "ex_status_busy")
      shinyjs::addClass(id = "execution_status", class = "ex_status_idle")
    })
    
    
    
    pollData <- reactivePoll(2000, session,
                             checkFunc = function() {
                               if (!is.null(logfilename()) && file.exists(logfilename())){
                                 file.info(logfilename())$mtime[1]
                               } else
                                 ""
                             },
                             valueFunc = function() {
                               new_df <- data.frame()
                               if (!is.null(logfilename()) && file.exists(logfilename())) { 
                                 data <- readChar(logfilename(), file.info(logfilename())$size)
                                 lines <- unlist(strsplit(data, "Iteration = "))
                                 if (length(lines) > 1) {
                                 df <- data.frame(lines[-1])
                                 colnames(df) <- c("old")
                                 newline <- if (.Platform$OS.type == "windows") "\r\n\t" else "\n\t"
                                 a <- strsplit(lines[-1], newline)
                                 max_len <- max(sapply(a, length)) - 1
                                 value_fun <- function(x, i) {  
                                   splited_val <- unlist(strsplit(x[[i]], " = "))
                                   return (substring(splited_val[2], 1, nchar(splited_val[2])-1))
                                 }
                                 for(i in 1:max_len){
                                   if (i != 1) {
                                     splited_val <- unlist(strsplit(a[[1]][i], " = "))
                                     df[,substring(splited_val[1], 2)] <- as.numeric(sapply(a, value_fun, i))
                                   } else {
                                     df[,"Iter"] <- as.numeric(sapply(a, "[", i))
                                   }
                                 }
                                 df <- df[, names(df) != "old"]
                                 df["LL"] <- (-2) * df["LL"]
                                 names(df)[names(df) == "LL"] <- "-2LL"
                                 new_df <- reshape::melt(df,
                                                         id.vars = "Iter")
                                 D <- transform(new_df, Iter = Iter, 
                                                value = value)
                                 new_df <- D %>% dplyr::arrange(D)
                                 new_df["Subject"] = 1
                                 new_df["Scenario"] = "WorkFlow"
                                 names(new_df)[names(new_df) == "variable"] <- "Parameter"
                                 names(new_df)[names(new_df) == "value"] <- "Value"
                                 new_df <- new_df[, c(4, 5, 1, 2, 3)]
                                 df$Iter <- as.integer(df$Iter)
                                 }
                                 dt(new_df)
                                 dt2(df)
                               } else {
                                 NULL
                               }
                               
                             }
    )

    output$table_convergence <- renderTable({
      req(dt2())
      if(!is.data.frame(dt2())) {
        return(NULL)
      } else {
        return(dt2())
      }
    })
    
    shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
    
    table_results <- reactiveVal(NULL)
    output$table_results <- DT::renderDT({
      req(table_results())
      if(!is.data.frame(table_results())) {
        return(NULL)
      } else {
        return(table_results())
      }
    })
    outputOptions(output, "table_results", suspendWhenHidden = FALSE)
    
    stepwise_results <- reactiveVal(NULL)
    output$stepwise_results <- renderText({
      stepwise_results()
    })
      
    outputOptions(output, "stepwise_results", suspendWhenHidden = FALSE)
    
    
    exec_msg <- reactiveVal(NULL)
    exec_data <- reactiveVal(NULL)
    
    output$job_status_text <- renderText(exec_msg())

    dt <- reactiveVal(NULL)
    dt2 <- reactiveVal(NULL)
  
    plotHeight <- reactive({
      req(dt())
      if (nrow(dt()) > 0) {
        scenarios_count <- get("scenarios_counter", envir = model_executor_env)
        size <- 1
        selectedColumns <- "Parameter"
        if (input$pk_sort_col1 != " ") {
          selectedColumns <- c(selectedColumns, input$pk_sort_col1)
        } 
        if (input$pk_sort_col2 != " ") {
          selectedColumns <- c(selectedColumns, input$pk_sort_col2)
        } 
        if (input$pk_sort_col3 != " ") {
          selectedColumns <- c(selectedColumns, input$pk_sort_col3)
        } 
        if (input$pk_sort_col4 != " ") {
          selectedColumns <- c(selectedColumns, input$pk_sort_col4)
        } 
        if (input$pk_sort_col5 != " ") {
          selectedColumns <- c(selectedColumns, input$pk_sort_col5)
        } 
        un <- unique(dt()[, selectedColumns])
        if (length(un[[1]]) == 1) {
          num_plots <- length(un) * max(1, (scenarios_count - 1))
        } else {
          num_plots <- length(un[[1]]) * max(1, (scenarios_count - 1))
        }
        
        size <- ceiling(num_plots / 3)
        400 * size
      } else {
        100
      }
    })
    
    p <- reactive({
      req(dt())
      if (!is.null(dt()) && nrow(dt()) > 0) {
        sort_facet <- "Parameter ~ . + Scenario"
        if (input$pk_sort_col1 != " ") {
          sort_facet <- paste0(sort_facet, "+", input$pk_sort_col1)
        }
        if (input$pk_sort_col2 != " ") {
          sort_facet <- paste0(sort_facet, "+", input$pk_sort_col2)
        }
        if (input$pk_sort_col3 != " ") {
          sort_facet <- paste0(sort_facet, "+", input$pk_sort_col3)
        }
        if (input$pk_sort_col4 != " ") {
          sort_facet <- paste0(sort_facet, "+", input$pk_sort_col4)
        }
        if (input$pk_sort_col5 != " ") {
          sort_facet <- paste0(sort_facet, "+", input$pk_sort_col5)
        }
        
        ggplot(dt(), aes(Iter, Value, group = 1)) + geom_line() +  facet_wrap( as.formula(sort_facet), ncol = 3, scales = "free") + xlab("Iteration") + ylab("") + theme_certara()
      }
    })
    
    output$convPlot <- renderPlot({
      p()
    })
    
    output$plot.ui <- renderUI({
      div(style="overflow-y:scroll;",
      plotOutput("convPlot", height = plotHeight())
      )
    })
    
    outputOptions(output, "table_convergence", suspendWhenHidden = FALSE)
    
    numbers <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_iterations)), "Please input a number")
      )
    })
    output$pk_iterations_error <- renderPrint({ numbers() })
    
    numbers1 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_integration_points)), "Please input a number")
      )
    })
    output$pk_integration_points_error <- renderPrint({ numbers1() })
    
    numbers2 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_nonparam_iterations)), "Please input a number")
      )
    })
    output$pk_nonparam_iterations_error <- renderPrint({ numbers2() })  
    
    numbers3 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_iterations_number)), "Please input a number")
      )
    })
    output$pk_iterations_number_error <- renderPrint({ numbers3() })  
    
    numbers4 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_replicates_number)), "Please input a number")
      )
    })
    output$pk_replicates_number_error <- renderPrint({ numbers4() }) 
    
    numbers5 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_max_retries)), "Please input a number"),
        need(as.numeric(input$pk_max_retries) >= 2 && as.numeric(input$pk_max_retries) <= 99, "Must be between 2 and 99")
      )
    })
    output$pk_max_retries_error <- renderPrint({ numbers5() })
    
    
    numbers6 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_conf_level_boot)), "Please input a number"),
        need(as.numeric(input$pk_conf_level_boot) >= 0 && as.numeric(input$pk_conf_level_boot) <= 100, "Confidence level should be between 0 and 100 percent")
      )
    })
    output$pk_conf_level_boot_error <- renderPrint({ numbers6() })
    
    numbers7 <- reactive({
      validate(
        need(!is.na(as.numeric(input$pk_degrees_freedom)), "Please input a number"),
        need(as.numeric(input$pk_degrees_freedom) >= 3 && as.numeric(input$pk_degrees_freedom) <= 30, "Number of degrees of freedom should be between 3 and 30")
      )
    })
    output$pk_conf_level_boot_error <- renderPrint({ numbers7() })
    
    observeEvent(input$pk_run_mode, {
      if (input$pk_run_mode == "Simulation" || input$pk_run_mode == "Visual predictive check"){
        updateSelectInput(session, inputId = "pk_algorithm", selected = "Naive Pooled", choices = c("Naive Pooled"))
      } else if ( !model@isPopulation){
        updateSelectInput(session, inputId = "pk_algorithm", selected = "Naive Pooled", choices = c("Naive Pooled"))
      } else if (!allow_gaussian_fit) {
        updateSelectInput(session, inputId = "pk_algorithm", selected = "Laplacian", choices = c("Laplacian", "QRPEM", "IT2S-EM", "Naive Pooled"))
      } else {
        updateSelectInput(session, inputId = "pk_algorithm", selected = "FOCE ELS", choices = c("FOCE ELS", "FOCE L-B", "FO", "Laplacian", "Naive Pooled", "QRPEM", "IT2S-EM"))
      }
    })
    
    toListen <- reactive({
      list(input$pk_sort_col1, input$pk_sort_col2, input$pk_sort_col3, input$pk_sort_col4, input$pk_sort_col5)
    })
    
    observeEvent(toListen(), {
      if (model@isPopulation && (input$pk_sort_col1 != " " || input$pk_sort_col2 != " " || input$pk_sort_col3 != " " || input$pk_sort_col4 != " " || input$pk_sort_col5 != " ") ) {
        updateSelectInput(session, inputId = "pk_run_mode", selected = "Simple", choices = c("Simple", "Scenarios"))
      } else if (!model@isPopulation && (input$pk_sort_col1 != " " || input$pk_sort_col2 != " " || input$pk_sort_col3 != " " || input$pk_sort_col4 != " " || input$pk_sort_col5 != " ")) {
        updateSelectInput(session, inputId = "pk_run_mode", selected = "Simple", choices = c("Simple"))
      } else if (!model@isPopulation) {
        updateSelectInput(session, inputId = "pk_run_mode", selected = "Simple", choices = c("Simple", "Simulation"))
      } else {
        updateSelectInput(session, inputId = "pk_run_mode", selected = "Simple", choices = c("Simple", "Scenarios", "Covariate search stepwise", "Covariate search shotgun", "Bootstrap",  "Visual predictive check", "Simulation"))
      }
      
      if (input$pk_sort_col4 != " ") {
        shinyjs::showElement(id = "pk_sort_col5", asis = FALSE)
      } else if (input$pk_sort_col5 == " "){
        shinyjs::hideElement(id = "pk_sort_col5", asis = FALSE)
      }
      if (input$pk_sort_col3 != " ") {
        shinyjs::showElement(id = "pk_sort_col4", asis = FALSE)
      } else if (input$pk_sort_col4 == " "){
        shinyjs::hideElement(id = "pk_sort_col4", asis = FALSE)
      }
      if (input$pk_sort_col2 != " ") {
        shinyjs::showElement(id = "pk_sort_col3", asis = FALSE)
      } else if (input$pk_sort_col3 == " "){
        shinyjs::hideElement(id = "pk_sort_col3", asis = FALSE)
      }
      if (input$pk_sort_col1 != " ") {
        shinyjs::showElement(id = "pk_sort_col2", asis = FALSE)
      } else if (input$pk_sort_col2 == " "){
        shinyjs::hideElement(id = "pk_sort_col2", asis = FALSE)
      }
    })
    
    observeEvent(input$pk_algorithm, {
      if (model@isPopulation) {
        if (input$pk_algorithm == "QRPEM") {
          updateSelectInput(
            session,
            inputId = "pk_method",
            selected = "Fisher Score",
            choices = c("Fisher Score")
          )
        } else {
          updateSelectInput(
            session,
            inputId = "pk_method",
            selected = "Sandwich",
            choices = c("Sandwich", "Hessian", "Fisher Score", "Auto-detect")
          )
        }
      }
    })

    
    if (model@hasResetInfo) {
      allowSort <- FALSE
    } else if (length(model@userDefinedExtraDefs) > 0 && grepl("reset", model@userDefinedExtraDefs)) {
      allowSort <- FALSE
    } else {
      allowSort <- TRUE
    }
    
    observe({
      if (allowSort) { 
        shinyjs::show("pk_sort_input_col")
      } else {
        shinyjs::hide("pk_sort_input_col")
      }
    })
    
    output$isPopulation <- reactive({
      model@isPopulation
    })
    
    observe({
      if (output$isPopulation() && (input$pk_run_mode == 'Simulation') || ((input$pk_run_mode == 'Visual predictive check'))) {
        shinyjs::show("pk_num_repl_col")
      } else {
        shinyjs::hide("pk_num_repl_col")
      }
    })
    
    output$stratifyCols <- reactive({
      any(unlist(lapply(model@covariateList, function(x) x@type %in% c(Certara.RsNLME:::COVAR_CATEGORY, Certara.RsNLME:::COVAR_OCCASION))))
    })
    
    outputOptions(output, "isPopulation", suspendWhenHidden = FALSE)
    outputOptions(output, "stratifyCols", suspendWhenHidden = FALSE)
   
    output$profileTable <- renderUI({
      fixedEff <- unlist(lapply(model@structuralParams, function(x) x@fixedEffName))
      fixedEffVal <- unlist(lapply(model@structuralParams, function(x) x@initialValue))
      list(
        bslib::card(
          tags$table(style = "border: 1px solid black; padding: 0px; width: 100%;",
                     tags$tr(
                       tags$th("Fixed Effects"),
                       lapply(1:length(fixedEff), function(i) {
                           tags$th(fixedEff[i])
                       }),
                     ),
                     tags$tr(
                       tags$td("Nominal"),
                       lapply(1:length(fixedEffVal), function(i) {
                           tags$td(fixedEffVal[i])
                       }),
                     ),
                     tags$tr(
                       tags$td("Profiling"),
                       lapply(1:length(fixedEff), function(i) {
                           tags$td(checkboxInput(inputId = fixedEff[i], label = "", value = FALSE),)
                       }),
                     ),
                     tags$tr(
                       tags$td(""),
                       lapply(1:length(fixedEff), function(i) {
                         
                           tags$td(
                             column(
                               width = 1, style = "text-align:right; padding-top:0px; padding-right:10px",
                             conditionalPanel(
                               paste0("(input.", fixedEff[i] ,")"),
                               textInput(inputId = paste0(fixedEff[i], "val"), value = "", label = ""),
                             )
                           )
                         )
                       }),
                     )
          )
        )
      )
    })
    
    # counter_panels <- 1
    assign("counter_panels", value = 1, envir = model_executor_env)
    observeEvent(input$addTable, {
     count <- get("counter_panels", envir = model_executor_env)
     current_id <- paste0("panel_", count)
     cov_panel(current_id, count - 1)
     insertUI(selector = "#add_panels_here",
              ui = cov_panel_ui(current_id))
     
     assign("counter_panels", value = count + 1, envir = model_executor_env)
     # counter_panels <<- counter_panels + 1
    })
    
    # counter_sim_panels <- 1
    assign("counter_sim_panels", value = 1, envir = model_executor_env)
    observeEvent(input$addSimTable, {
      count <- get("counter_sim_panels", envir = model_executor_env)
      strParams <- paste0(unlist(lapply(model@structuralParams, function(x) x@name)), collapse=",")
      current_id <- paste0("panel_", count)
      sim_tables_panel(current_id, strParams, count - 1)
      insertUI(selector = "#sim_tables_placeholder",
               ui = sim_tables_panel_ui(current_id ))
      
      assign("counter_sim_panels", value = count + 1, envir = model_executor_env)
      # counter_sim_panels <<- counter_sim_panels + 1
    })
    
    # counter_vpc_panels <- 1
    assign("counter_vpc_panels", value = 1, envir = model_executor_env)
    observeEvent(input$addVPCTable, {
      count <- get("counter_vpc_panels", envir = model_executor_env)
      strParams <- paste0(unlist(lapply(model@structuralParams, function(x) x@name)), collapse=",")
      current_id <- paste0("panel_vpc_", count)
      sim_tables_panel(current_id, strParams, count - 1)
      insertUI(selector = "#vpc_tables_placeholder",
               ui = sim_tables_panel_ui(current_id ))
      
      assign("counter_vpc_panels", value = count + 1, envir = model_executor_env)
      # counter_vpc_panels <<- counter_vpc_panels + 1
    })
    
    # scenarios_counter <- 1
    assign("scenarios_counter", value = 1, envir = model_executor_env)
    observeEvent(input$addScenario, {
      
      if (model@isTextual) {
        covList <- sub_models
      }
      else {
        covList <- listCovariateEffectNames(model)
      }
      
      if (length(covList) > 0)
      {
        scenarios_count <- get("scenarios_counter", envir = model_executor_env)
        current_id <- paste0("panel_scenarios_", scenarios_count)
        scenarios_panel(current_id, covList, scenarios_count)
        insertUI(selector = "#scenarios_table_placeholder", ui = scenarios_panel_ui(current_id ))
      
        assign("scenarios_counter", value = scenarios_count + 1, envir = model_executor_env)
        # scenarios_counter <<- scenarios_counter + 1
      }
      else {
        execution_error("Cannot add a scenario for a model that does not contain any covariate effect parameters.")
        
        showModal(
          modalDialog(
            title = "Error",
            verbatimTextOutput("execution_error"),
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
    })
    
    ode <- reactive({
      if (input$pk_ode == "Matrix Exponent") {
        ode="MatrixExponent"
      }
      else if (input$pk_ode == "Auto-detect") {
        ode="AutoDetect"
      }
      else if (input$pk_ode == "Non-stiff DVERK") {
        ode="DVERK"
      }
      else if (input$pk_ode == "Stiff") {
        ode="Stiff"
      }
      else if (input$pk_ode == "Non-stiff DOPRI5") {
        ode="DOPRI5"
      }
      ode
    })
    
    method <- reactive({
      if (input$pk_algorithm == "QRPEM") {
        method="QRPEM"
      }
      else if (input$pk_algorithm == "IT2S-EM") {
        method="IT2S-EM"
      }
      else if (input$pk_algorithm == "FOCE L-B") {
        method="FOCE-LB"
      }
      else if (input$pk_algorithm == "FO") {
        method="FO"
      }
      else if (input$pk_algorithm == "FOCE ELS") {
        method="FOCE-ELS"
      }
      else if (input$pk_algorithm == "Laplacian") {
        method="Laplacian"
      }
      else if (input$pk_algorithm == "Naive Pooled") {
        method="Naive-Pooled"
      }
      method
    })
    
    stderr <- reactive({
      if (!input$pk_standard_errors || input$pk_algorithm == "IT2S-EM") {
        stderr = "None"
      }
      else if (input$pk_method == "Hessian") {
        stderr = "Hessian"
      }
      else if (input$pk_method == "Fisher Score") {
        stderr = "Fisher-Score"
      }
      else if (input$pk_method == "Sandwich") {
        stderr = "Sandwich"
      }
      else if (input$pk_method == "Auto-detect") {
        stderr = "Auto-Detect"
      }
      stderr
    })
    
    impsampdof <- reactive({
      if (!input$pk_algorithm == "QRPEM") {
        impsampdof = "Normal"
      }
      else if (input$pk_sampling_distribution == "Multivariate Normal") {
        impsampdof = "Normal"
      }
      else if (input$pk_sampling_distribution == "Direct Sampling") {
        impsampdof = "Direct"
      }
      else if (input$pk_sampling_distribution == "Mixture-2") {
        impsampdof = "Mixture-2"
      }
      else if (input$pk_sampling_distribution == "Multivariate Laplace") {
        impsampdof = "DoubleExponential"
      }
      else if (input$pk_sampling_distribution == "Mixture-3") {
        impsampdof = "Mixture-3"
      }
      else if (input$pk_sampling_distribution == "Multivariate t") {
        impsampdof = "T"
      }
      impsampdof
    })
    
    scramble <- reactive({
      scramble = "Owen"
      if (input$pk_scrambling_method == "None") {
        scramble = "None"
      }
      else if (input$pk_scrambling_method == "Tezuka-Faur") {
        scramble = "Tezuka-Faur"
      } 
      scramble
    })
    
    
    # Code Generation Reactives ----
    
    tableParamsReactive <- metaReactive2(varname = "tables", {
      count <- get("counter_panels", envir = model_executor_env)
      if(count == 0) return()
      
      tables <- c()
      
      for(i in 1:count){
        if(is.null(input[[paste0("panel_", i, "-pk_table_name")]]) || input[[paste0("panel_", i, "-pk_table_name")]] == "")
          next
        
        if (!as.logical(input[[paste0("panel_", i, "-pk_keep")]])) {
          tableNew <- metaExpr(
            tableParams(
              name = ..(paste0(input[[paste0("panel_", i, "-pk_table_name")]], ".csv")),
              timesList = ..(input[[paste0("panel_", i, "-pk_table_times")]]),
              covrSet = ..(input[[paste0("panel_", i, "-pk_table_cov_set")]]),
              whenDose = ..(input[[paste0("panel_", i, "-pk_table_dose")]]),
              whenObs = ..(input[[paste0("panel_", i, "-pk_table_observe")]]),
              variablesList = ..(input[[paste0("panel_", i, "-pk_table_var")]]),
              keepSource = FALSE,
              timeAfterDose = ..(as.logical(input[[paste0("panel_", i, "-pk_tad")]])),
              IRES = ..(if (input[[paste0("panel_", i, "-pk_table_observe")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_ires")]])),
              Weight = ..(if (input[[paste0("panel_", i, "-pk_table_observe")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_weight")]])),
              IWRES = ..(if (input[[paste0("panel_", i, "-pk_table_observe")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_iwres")]]))
            ))
        } else {
          tableNew <- metaExpr(
            tableParams(
              name = ..(paste0(input[[paste0("panel_", i, "-pk_table_name_keep")]], ".csv")),
              covrSet = ..(input[[paste0("panel_", i, "-pk_table_cov_keep")]]),
              whenDose = ..(input[[paste0("panel_", i, "-pk_table_dose_keep")]]),
              whenObs = ..(input[[paste0("panel_", i, "-pk_table_observe_keep")]]),
              variablesList = ..(input[[paste0("panel_", i, "-pk_table_var_keep")]]),
              keepSource = TRUE,
              timeAfterDose = ..(as.logical(input[[paste0("panel_", i, "-pk_tad")]])),
              IRES = ..(if (input[[paste0("panel_", i, "-pk_table_observe_keep")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_ires")]])),
              Weight = ..(if (input[[paste0("panel_", i, "-pk_table_observe_keep")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_weight")]])),
              IWRES = ..(if (input[[paste0("panel_", i, "-pk_table_observe_keep")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_iwres")]]))
          ))
        }
        
        tables <- metaExpr(c(..(tables), ..(tableNew)))
      }
      tables
    })
    
    tableParamsSimReactive <- metaReactive2(varname = "simTables", {
      count <- get("counter_sim_panels", envir = model_executor_env)
      if(count == 0) return()
      
      tables <- c()
      
      for(i in 1:count){
        if(is.null(input[[paste0("panel_", i, "-pk_sim_table_name")]]) || input[[paste0("panel_", i, "-pk_sim_table_name")]] == "")
          next
        
        if (!as.logical(input[[paste0("panel_", i, "-pk_sim_keep")]])) {
          tableNew <- metaExpr(
            tableParams(
              name = ..(paste0(input[[paste0("panel_", i, "-pk_sim_table_name")]], ".csv")),
              timesList = ..(input[[paste0("panel_", i, "-pk_sim_table_times")]]),
              covrSet = ..(input[[paste0("panel_", i, "-pk_sim_table_cov_set")]]),
              whenDose = ..(input[[paste0("panel_", i, "-pk_sim_table_dose")]]),
              whenObs = ..(input[[paste0("panel_", i, "-pk_sim_table_observe")]]),
              variablesList = ..(input[[paste0("panel_", i, "-pk_sim_table_var")]]),
              keepSource = FALSE,
              timeAfterDose = ..(as.logical(input[[paste0("panel_", i, "-pk_sim_tad")]])),
              forSimulation = TRUE
            ))
        } else {
          tableNew <- metaExpr(
            tableParams(
              name = ..(paste0(input[[paste0("panel_", i, "-pk_sim_table_name_keep")]], ".csv")),
              covrSet = ..(input[[paste0("panel_", i, "-pk_sim_table_cov_keep")]]),
              whenDose = ..(input[[paste0("panel_", i, "-pk_sim_table_dose_keep")]]),
              whenObs = ..(input[[paste0("panel_", i, "-pk_sim_table_observe_keep")]]),
              variablesList = ..(input[[paste0("panel_", i, "-pk_sim_table_var_keep")]]),
              keepSource = TRUE,
              timeAfterDose = ..(as.logical(input[[paste0("panel_", i, "-pk_sim_tad")]])),
              forSimulation = TRUE
            ))
        }
        
        tables <- metaExpr(c(..(tables), ..(tableNew)))
      }
      tables
    })    
    
    tableParamsVPCReactive <- metaReactive2(varname = "vpcTables", {
      count <- get("counter_vpc_panels", envir = model_executor_env)
      if(count == 0) return()
      
      tables <- c()
      
      for(i in 1:count){
        if(is.null(input[[paste0("panel_vpc_", i, "-pk_sim_table_name")]]) || input[[paste0("panel_vpc_", i, "-pk_sim_table_name")]] == "")
          next
        
        if (!as.logical(input[[paste0("panel_vpc_", i, "-pk_sim_keep")]])) {
          tableNew <- metaExpr(
            tableParams(
              name = ..(paste0(input[[paste0("panel_vpc_", i, "-pk_sim_table_name")]], ".csv")),
              timesList = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_times")]]),
              covrSet = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_cov_set")]]),
              whenDose = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_dose")]]),
              whenObs = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_observe")]]),
              variablesList = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_var")]]),
              keepSource = FALSE,
              timeAfterDose = ..(as.logical(input[[paste0("panel_vpc_", i, "-pk_sim_tad")]])),
              forSimulation = TRUE
            ))
        } else {
          tableNew <- metaExpr(
            tableParams(
              name = ..(paste0(input[[paste0("panel_vpc_", i, "-pk_sim_table_name_keep")]], ".csv")),
              covrSet = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_cov_keep")]]),
              whenDose = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_dose_keep")]]),
              whenObs = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_observe_keep")]]),
              variablesList = ..(input[[paste0("panel_vpc_", i, "-pk_sim_table_var_keep")]]),
              keepSource = TRUE,
              timeAfterDose = ..(as.logical(input[[paste0("panel_vpc_", i, "-pk_sim_tad")]])),
              forSimulation = TRUE
            ))
        }
        
        tables <- metaExpr(c(..(tables), ..(tableNew)))
      }
      tables
    })    
    
    engineParamsReactive <- metaReactive2(varname = "engineParams", {
      # could require execution before, e.g., only create code after
      # model has been executed then isolate 
      # because currently, code is always generated for the 'active' inputs
      # e.g., 
      #req(input$button_execute)
      #isolate({
          # activeEngineParams <- ...
      #   })
      activeEngineParams <- metaExpr(
        engineParams(
          model,
          sort = ..(
            if (!allowSort) FALSE else input$pk_sort_input
          ),
          ODE = ..(ode()),
          rtolODE = ..(as.numeric(
            input$pk_ode_rel_tol
          )
          ),
          atolODE = ..(as.numeric(
            input$pk_ode_abs_tol
          )),
          maxStepsODE = ..(as.numeric(
            input$pk_ode_max_step
          )),
          numIterations = ..(as.numeric(
            input$pk_iterations
          )),
          method = ..(method()),
          stdErr = ..(stderr()),
          isCentralDiffStdErr = ..(
            input$pk_diff_method == "Central Difference"
          ),
          stepSizeStdErr = ..(as.numeric(
            input$pk_step_size
          )),
          numIntegratePtsAGQ = ..(
            if (input$pk_algorithm == "FOCE ELS" ||
                input$pk_algorithm == "Laplacian")
              as.numeric(input$pk_integration_points)
            else
              1
          ),
          numIterNonParametric = ..(
            if (input$pk_nonparametric &&
                input$pk_algorithm != "Naive Pooled")
              as.numeric(input$pk_nonparam_iterations)
            else
              0
          ),
          allowSyntheticGradient = ..(if (input$pk_algorithm != "Naive Pooled")
            input$pk_synt_gradient
            else
              FALSE),
          numIterMAPNP = ..(
            if (input$pk_mapnpstart  &&
                input$pk_algorithm != "Naive Pooled")
              as.numeric(input$pk_iterations_number)
            else
              0
          ),
          numRepPCWRES = ..(
            if (input$pk_pcwres  &&
                input$pk_algorithm != "Naive Pooled")
              as.numeric(input$pk_replicates_number)
            else
              0
          ),
          stepSizeLinearize = ..(
            as.numeric(input$pk_linearization_step)
          ),
          numDigitLaplacian = ..(as.numeric(
            input$pk_lagl_ndigit
          )),
          numDigitBlup = ..(as.numeric(
            input$pk_blup_ndigit
          )),
          mapAssist =  ..(
            if (input$pk_mapassisst &&
                input$pk_algorithm == "QRPEM")
              as.numeric(input$pk_mapassisst_period)
            else
              0
          ),
          iSample = ..(
            as.numeric(input$pk_number_sample_points)
          ),
          iAcceptRatio = ..(
            as.numeric(input$pk_acceptance_ratio)
          ),
          impDist = ..(impsampdof()),
          tDOF = ..(
            if (input$pk_sampling_distribution == "Multivariate t")
              as.numeric(input$pk_degrees_freedom)
            else
              4
          ),
          numSampleSIR = ..(as.numeric(
            input$pk_sir_samples
          )),
          numBurnIn = ..(
            if (input$pk_algorithm == "QRPEM")
              as.numeric(input$pk_number_burnin_iter)
            else
              0
          ),
          freezeOmega = ..(if (input$pk_algorithm == "QRPEM")
            input$pk_frozen_omega
            else
              FALSE),
          MCPEM = ..(if (input$pk_algorithm == "QRPEM")
            input$pk_mcpem
            else
              FALSE),
          runAllIterations = ..(if (input$pk_algorithm == "QRPEM")
            input$pk_run_all_iter
            else
              FALSE),
          scramble = ..(scramble()),
          stepSizePartialDeriv = ..(as.numeric(
            input$pk_part_deriv
          )),
          numTimeStepPartialDeriv = ..(as.numeric(
            input$pk_time_steps
          )))
      )
      activeEngineParams
    })
    
    ### Sort Columns
    remaining_cols <- reactive({
      selected_cols <- c(input$pk_sort_col1, input$pk_sort_col2, input$pk_sort_col3, input$pk_sort_col4, input$pk_sort_col5)
      remaining <- lapply(1:5, function(i) setdiff(sort_cols, selected_cols[selected_cols != " " & !is.na(selected_cols) & seq_along(selected_cols) != i]))
      remaining
    })
    
    observe({
      updateSelectInput(session, "pk_sort_col1", choices = c(" ", remaining_cols()[[1]]), selected = input$pk_sort_col1)
      updateSelectInput(session, "pk_sort_col2", choices = c(" ", remaining_cols()[[2]]), selected = input$pk_sort_col2)
      updateSelectInput(session, "pk_sort_col3", choices = c(" ", remaining_cols()[[3]]), selected = input$pk_sort_col3)
      updateSelectInput(session, "pk_sort_col4", choices = c(" ", remaining_cols()[[4]]), selected = input$pk_sort_col4)
      updateSelectInput(session, "pk_sort_col5", choices = c(" ", remaining_cols()[[5]]), selected = input$pk_sort_col5)
    })
    
    observeEvent(input$button_execute, ignoreInit = T, {
      assign("process_start_time", value = Sys.time(), envir = model_executor_env)
      # process_start_time <<- Sys.time()
      
      removeModal()
      
      showNotification(
        "Starting execution, please navigate to the results tab...",
        action = NULL,
        duration = 4,
        closeButton = TRUE,
        id = NULL,
        type = "message"
      )
      toggle_status("busy")
      shinyjs::show(id = "results_window")
      
      showModal(
        modalDialog(
          title = "Execution Started",
          textOutput("execution_start"),
          easyClose = TRUE,
          footer = NULL
        )
      )
      
      dt(NULL)
      dt2(NULL)
      table_results(NULL)
      stepwise_results(NULL)
      params <- engineParamsReactive()
      host <- hosts[unlist(lapply(hosts, function (x) x@hostName == input$pk_execute_on))][[1]]
      
      if (input$pk_run_mode == "Simple" || input$pk_run_mode == "Bootstrap" || input$pk_run_mode == "Covariate search stepwise"
          || input$pk_run_mode == "Covariate search shotgun" || input$pk_run_mode == "Scenarios" || input$pk_run_mode == "Profile"
          || input$pk_run_mode == "Simulation" || input$pk_run_mode == "Visual predictive check" ) {
        
        shinyjs::html("job_status_text", "")
        
        if (input$pk_run_mode == "Simple") {
          
          shinyjs::showElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::showElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          
          execution_directory <- paste0("nlme_shiny_fitmodel")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          simpleTables <- tableParamsReactive()
          
          if (input$pk_sort_col1 != " " || input$pk_sort_col2 != " " || input$pk_sort_col3 != " " || input$pk_sort_col4 != " " || input$pk_sort_col5 != " ") {
            
            sort1 <- if (input$pk_sort_col1 != " ") paste0(input$pk_sort_col1, ",") else ""
            sort2 <- if (input$pk_sort_col2 != " ") paste0(input$pk_sort_col2, ",") else ""
            sort3 <- if (input$pk_sort_col3 != " ") paste0(input$pk_sort_col3, ",") else ""
            sort4 <- if (input$pk_sort_col4 != " ") paste0(input$pk_sort_col4, ",") else ""
            sort5 <- if (input$pk_sort_col5 != " ") paste0(input$pk_sort_col5, ",") else ""
            sort_cols <- paste0(sort1, sort2, sort3, sort4, sort5)
            sortColumns <- SortColumns(sort_cols)

            if (any(duplicated(strsplit(sort_cols, ",")[[1]]))) {
              error_txt <- "Cannot map the same column to Sort twice."
              exec_msg(error_txt)
              execution_error(paste0(error_txt, collapse="\n"))
              
              showModal(
                modalDialog(
                  title = "Error",
                  verbatimTextOutput("execution_error"),
                  easyClose = TRUE,
                  footer = NULL
                )
              )
              toggle_status("idle")
            }
            else {
              shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
              shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
              shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
              exec_msg("Execution Started. Running sortfit.")
              tstart <- Sys.time()
              tryCatch({  
                withCallingHandlers({
                  sortfit(model, host, params, sortColumns = sortColumns, simpleTables = as.list(simpleTables))
                }, error = function(e){
                  shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
                }, warning = function(w){
                  shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
                }, message = function(m){
                  shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
                })
              }, error = function(e){
                shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
              })
              
              if (from_pirana) {
                shinyjs::html(id = "job_status_text", html = paste0("<p style=\"color:#FF0000\";>Pirana UI only shows the results obtained by the last combination of values of the Sorted variables</p><br>"), add = TRUE)
              }
              
              if (file.exists(metamodel_file_name)) {
                file.copy(metamodel_file_name, model@modelInfo@workingDir)
              }
              
              if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
                source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
              }
              
              if(exists("dmp.txt")) {
                jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
                dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
                jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                     path = jsonName,
                                     digits = NA,
                                     force = TRUE)
              }
              tend <- Sys.time()
              telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
              execution_time(paste0(
                "Start Time: ", tstart, "<br/>",
                "Finish Time: ", tend, "<br/>",
                "Total Time: ", telapsed))
              exec_results <- readLines(file.path(model@modelInfo@workingDir, "StatusWindow.txt"))
              exec_msg(paste0(exec_results, collapse = "\n"))
              toggle_status("idle")
              
              showModal(
                modalDialog(
                  title = "Execution Finished",
                  htmlOutput("execution_time"),
                  easyClose = TRUE,
                  footer = NULL
                )
              )
              
              table <- read.csv(file.path(model@modelInfo@workingDir, "ConvergenceData.csv"))
              dt(table)
              shinyjs::showElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
            }
          } else {
            shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
            exec_msg("Execution Started. Check other tabs for information.")
            tstart <- Sys.time()
            f <- future({ 
              fitmodel(model, host, params, as.list(simpleTables), FALSE)
            })
            
            f %...>% (function(result) {
            
              if (file.exists(metamodel_file_name)) {
                file.copy(metamodel_file_name, model@modelInfo@workingDir)
              }
              
              if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
                source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
              }
              
              if(exists("dmp.txt")) {
                jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
                dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
                jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                     path = jsonName,
                                     digits = NA,
                                     force = TRUE)
              }
              tend <- Sys.time()
              telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
              execution_time(paste0(
                         "Start Time: ", tstart, "<br/>",
                         "Finish Time: ", tend, "<br/>",
                         "Total Time: ", telapsed))
              error_file <- file.path(model@modelInfo@workingDir, "err2.txt")
              if (file.exists(error_file)) {
                modal_title <- "Execution Error"
                err_message <- readLines(error_file)
              } else {
                modal_title <- "Execution Finished"
                err_message <- NULL
              }
              exec_msg(paste0("Execution has finished...\n\n", paste0(err_message, collapse = "\n")))
              execution_error(paste0(err_message, collapse = "<br/>"))
              logfilename(file.path(model@modelInfo@workingDir, "progress.txt"))
              logfiledir(model@modelInfo@workingDir)
              shinyjs::removeClass(id = "execution_status", class = "ex_status_busy")
              shinyjs::addClass(id = "execution_status", class = "ex_status_idle")
              
              showModal(
                modalDialog(
                  title = modal_title,
                  htmlOutput("execution_time"),
                  div(style = "height: 1rem;"),
                  htmlOutput("execution_error"),
                  easyClose = TRUE,
                  footer = NULL
                )
              )
            }) %...!% (function(err) {
              error_file <-  file.path(model@modelInfo@workingDir, "err2.txt")
              error_txt <- readLines(error_file)
              execution_error(error_txt)
              shinyjs::html(id = "job_status_text", html = paste0("<br>", error_txt, collapse = "<br>"), add = TRUE)
              toggle_status("idle")
              
              showModal(
                modalDialog(
                  title = "Error",
                  verbatimTextOutput("execution_error"),
                  easyClose = TRUE,
                  footer = NULL
                )
              )
            })
          }
          
        } else if (input$pk_run_mode == "Bootstrap") {
          
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          
          execution_directory <- paste0("nlme_shiny_boot")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          
          stratifyColumns <- c(input$pk_boot_strat1, input$pk_boot_strat2, input$pk_boot_strat3)
          stratifyColumns <- stratifyColumns[!stratifyColumns %in% "None"]
          stratifyColumns <- paste0(stratifyColumns, collapse = ", ")
          
          boot <- BootstrapParams(
               numReplicates = as.numeric(input$pk_number_bootstrap_samples),
               initialEstimates = input$pk_init_est,
               numRetries = as.numeric(input$pk_max_retries),
               randomNumSeed = as.numeric(input$pk_seed),
               confidenceLevel = as.numeric(input$pk_conf_level_boot),
               stratifyColumns = stratifyColumns
             )
          
          tstart <- Sys.time()
          
          temp_str <- ""
          
          tryCatch({  
            
            withCallingHandlers({
              Certara.RsNLME::bootstrap(model, host, params, boot)
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
              assign("temp_str", value = paste(temp_str, e$message), envir = model_executor_env)
            }, warning = function(w){
              shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
              assign("temp_str", value = paste(temp_str, w$message), envir = model_executor_env)
            }, message = function(m){
              shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
              assign("temp_str", value = paste(temp_str, m$message), envir = model_executor_env)
            })
          }, error = function(e){
            shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
          })
          
          exec_msg(get("temp_str", envir = model_executor_env))
            
          if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
            source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
          }
            
          if(exists("dmp.txt")) {
            jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
            dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
            jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                  path = jsonName,
                                  digits = NA,
                                  force = TRUE)
          }

          tend <- Sys.time()
          telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
          execution_time(paste0(
            "Start Time: ", tstart, "<br/>",
            "Finish Time: ", tend, "<br/>",
            "Total Time: ", telapsed))
          toggle_status("idle")
          
          showModal(
            modalDialog(
              title = "Execution Finished",
              htmlOutput("execution_time"),
              easyClose = TRUE,
              footer = NULL
            )
          )
        }  else if (input$pk_run_mode == "Profile") {
        
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          
          profiles <- c()
          fixedEff <- unlist(lapply(model@structuralParams, function(x) x@fixedEffName))
          fixedEffVal <- unlist(lapply(model@structuralParams, function(x) x@initialValue))
          
          for (i in 1:length(fixedEff)) {
            if (input[[fixedEff[i]]]) {
              profile <- ProfileVar(fixedEff[i], as.numeric(fixedEffVal[i]), input[[paste0(fixedEff[i], "val")]])
              profiles <- c(profiles, profile)
            }
          }
          
          perturbateMethod <- if  (input$pk_perturbation == "Perturbation %") "USE_PERCENTAGE" else "USE_DELTA"
          profileParams <- ProfileParameters(perturbateMethod, profiles)
          
          tryCatch({  
            
            withCallingHandlers({
              profilePertubate(host, params, profileParams, model, runInBackground = FALSE)
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
            }, warning = function(w){
              shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
            }, message = function(m){
              shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
            })
          }, error = function(e){
            shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
          })
          
          if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
            source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
          }
            
          if(exists("dmp.txt")) {
            jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
            dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
            jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                  path = jsonName,
                                  digits = NA,
                                  force = TRUE)
          }
          logfilename(file.path(model@modelInfo@workingDir, "progress.txt"))
          logfiledir(model@modelInfo@workingDir)
          
        } else if (input$pk_run_mode == "Scenarios") {
          
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          
          execution_directory <- paste0("nlme_shiny_scenario")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          
          sort1 <- if (input$pk_sort_col1 != " ") paste0(input$pk_sort_col1, ",") else ""
          sort2 <- if (input$pk_sort_col2 != " ") paste0(input$pk_sort_col2, ",") else ""
          sort3 <- if (input$pk_sort_col3 != " ") paste0(input$pk_sort_col3, ",") else ""
          sort4 <- if (input$pk_sort_col4 != " ") paste0(input$pk_sort_col4, ",") else ""
          sort5 <- if (input$pk_sort_col5 != " ") paste0(input$pk_sort_col5, ",") else ""
          sort_cols <- paste0(sort1, sort2, sort3, sort4, sort5)
          sortColumns <- SortColumns(sort_cols)

          Scenarios <- c()
          
          if (model@isTextual) {
            CovariateEffectNames <- sub_models
          }
          else {
            CovariateEffectNames <- listCovariateEffectNames(model)
          }
          
          scenarios_count <- get("scenarios_counter", envir = model_executor_env)
          for(i in 1:scenarios_count){
            if(is.null(input[[paste0("panel_scenarios_", i, "-pk_scenario_name")]]) || input[[paste0("panel_scenarios_", i, "-pk_scenario_name")]] == "")
              next
            
            scenarioIndex <- ""
            for (j in 1:length(CovariateEffectNames)) {
              if (input[[paste0("panel_scenarios_", i, "-", j)]]) {
                if (scenarioIndex != "")
                  scenarioIndex <- paste0(scenarioIndex, ",")
                scenarioIndex <- paste0(scenarioIndex, j)
              }
            }
            scenario <- NlmeScenario(input[[paste0("panel_scenarios_", i, "-pk_scenario_name")]],
                                     scenarioIndex,
                                     input[[paste0("panel_scenarios_", i, "-pk_annotation")]])
            Scenarios <- c(Scenarios, scenario)
          } 
          
          if (length(Scenarios) == 0) {
            exec_msg("Cannot run Scenarios mode without scenarios defined.")
            execution_error("Cannot run Scenarios mode without scenarios defined.")
            
            showModal(
              modalDialog(
                title = "Error",
                verbatimTextOutput("execution_error"),
                easyClose = TRUE,
                footer = NULL
              )
            )
            toggle_status("idle")
          } else if (any(duplicated(strsplit(sort_cols, ",")[[1]]))) {
            exec_msg("Cannot map the same column to Sort twice..")
            execution_error("Cannot map the same column to Sort twice..")
            
            showModal(
              modalDialog(
                title = "Error",
                verbatimTextOutput("execution_error"),
                easyClose = TRUE,
                footer = NULL
              )
            )
            toggle_status("idle")
          } else {
            exec_msg("Execution Started. Running scenario fit...")
            tstart <- Sys.time()
            tryCatch({  
              withCallingHandlers({
                sortfit(model, host, params, sortColumns = sortColumns, scenarios = Scenarios)
              }, error = function(e){
                shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
              }, warning = function(w){
                shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
              }, message = function(m){
                shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
              })
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
            })
            
            if (from_pirana) {
              shinyjs::html(id = "job_status_text", html = paste0("<p style=\"color:#FF0000\";>Pirana UI only shows the results obtained by the last combination of values of the Sorted variables</p><br>"), add = TRUE)
            }
              
            if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
              source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
            }
              
            if(exists("dmp.txt")) {
              jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
              dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
              jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                   path = jsonName,
                                   digits = NA,
                                   force = TRUE)
            }
            tend <- Sys.time()
            telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
            exec_results <- readLines(file.path(model@modelInfo@workingDir, "StatusWindow.txt"))
            exec_msg(paste0(exec_results, collapse = "\n"))
            execution_time(paste0(
              "Start Time: ", tstart, "<br/>",
              "Finish Time: ", tend, "<br/>",
              "Total Time: ", telapsed))
            toggle_status("idle")
            
            showModal(
              modalDialog(
                title = "Execution Finished",
                htmlOutput("execution_time"),
                easyClose = TRUE,
                footer = NULL
              )
            )
            
            table <- read.csv(file.path(model@modelInfo@workingDir, "ConvergenceData.csv"))
            dt(table)
            shinyjs::showElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          }
        } else if (input$pk_run_mode == "Covariate search stepwise") {
          
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
        
          execution_directory <- paste0("nlme_shiny_stepwise")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          
          if(length(listCovariateEffectNames(model))==0){
            execution_error("No covariate-related fixed effects to check. Covariate effects must be first added to structural parameters.")
            
            showModal(
              modalDialog(
                title = "Error",
                verbatimTextOutput("execution_error"),
                easyClose = TRUE,
                footer = NULL
              )
            )
            shinyjs::removeClass(id = "execution_status", class = "ex_status_busy")
            shinyjs::addClass(id = "execution_status", class = "ex_status_idle")
            return()
          } else {
          cp <- covariateModel(model)
          }
          
          valAddCov <- if (input$pk_criterion == "-2LL") as.numeric(input$pk_add_p) else as.numeric(input$pk_add_thresh)
          valRemCov <- if (input$pk_criterion == "-2LL") as.numeric(input$pk_remove_p) else as.numeric(input$pk_remove_thresh)
          sp <- StepwiseParams(valAddCov, valRemCov, input$pk_criterion)
          
          exec_msg("Execution started, running covariate search stepwise...")
          tstart <- Sys.time()
          tryCatch({  
            
            withCallingHandlers({
              stepwiseSearch(model, host, params, cp, sp)
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
            }, warning = function(w){
              shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
            }, message = function(m){
              shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
            })
          }, error = function(e){
            shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
          })
          
          if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
            source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
          }
            
          if(exists("dmp.txt")) {
            jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
            dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
            jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                  path = jsonName,
                                  digits = NA,
                                force = TRUE)
          }
          tend <- Sys.time()
          telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
          stepwise_output <- readLines(file.path(model@modelInfo@workingDir, "Stepwise.txt"))
          stepwise_results(paste0(stepwise_output, collapse = "\n"))
          shinyjs::showElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          exec_results <- readLines(file.path(model@modelInfo@workingDir, "StatusWindow.txt"))
          exec_msg(paste0(exec_results, collapse = "\n"))
          execution_time(paste0(
            "Start Time: ", tstart, "<br/>",
            "Finish Time: ", tend, "<br/>",
            "Total Time: ", telapsed))
          toggle_status("idle")
          
          showModal(
            modalDialog(
              title = "Execution Finished",
              htmlOutput("execution_time"),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else if (input$pk_run_mode == "Covariate search shotgun") {
          
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          
          execution_directory <- paste0("nlme_shiny_shotgun")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          
          if(length(listCovariateEffectNames(model))==0){
            execution_error("No covariate-related fixed effects to check. Covariate effects must be first added to structural parameters.")
            
            showModal(
              modalDialog(
                title = "Error",
                verbatimTextOutput("execution_error"),
                easyClose = TRUE,
                footer = NULL
              )
            )
            shinyjs::removeClass(id = "execution_status", class = "ex_status_busy")
            shinyjs::addClass(id = "execution_status", class = "ex_status_idle")
            return()
          } else {
            cp <- covariateModel(model)
          }          
          
          exec_msg("Execution started, running covariate search shotgun...")
          tstart <- Sys.time()
          
          tryCatch({  
            withCallingHandlers({
                shotgunSearch(model, host, params, cp)
              }, error = function(e){
                shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
              }, warning = function(w){
                shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
              }, message = function(m){
                shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
              })
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
            })
          
          if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
            source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
          }
            
          if(exists("dmp.txt")) {
            jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
            dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
            jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                  path = jsonName,
                                  digits = NA,
                                  force = TRUE)
          }
          tend <- Sys.time()
          telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
          table_output <- read.csv(file.path(model@modelInfo@workingDir, "Overall.csv"))
          names(table_output)[names(table_output) == 'X.2LL'] <- '-2LL'
          table_results(table_output)
          shinyjs::showElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          exec_results <- readLines(file.path(model@modelInfo@workingDir, "StatusWindow.txt"))
          exec_msg(paste0(exec_results, collapse = "\n"))
          execution_time(paste0(
            "Start Time: ", tstart, "<br/>",
            "Finish Time: ", tend, "<br/>",
            "Total Time: ", telapsed))
          toggle_status("idle")
          
          showModal(
            modalDialog(
              title = "Execution Finished",
              htmlOutput("execution_time"),
              easyClose = TRUE,
              footer = NULL
            )
          )
 
        } else if (input$pk_run_mode == "Simulation") {
          
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          
          execution_directory <- paste0("nlme_shiny_sim")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          
          simulationTables <- tableParamsSimReactive()

          tstart <- Sys.time()
          temp_str <- ""
          
          tryCatch({  
            withCallingHandlers({
              simParams <- NlmeSimulationParams(
                numReplicates = as.numeric(input$pk_number_sim_replicates),
                seed = as.numeric(input$pk_seed_sim),
                simulationTables = as.list(simulationTables),
                numPoints = as.numeric(input$pk_num_sim_points),
                maxXRange = as.numeric(input$pk_max_x_range),
                yVariables = input$pk_y_var,
                simAtObs = input$pk_sim_at_obs
              )
              simmodel(model, simParams, params, host, runInBackground = FALSE)
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
              assign("temp_str", value = paste(temp_str, e$message), envir = model_executor_env)
            }, warning = function(w){
              shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
              assign("temp_str", value = paste(temp_str, w$message), envir = model_executor_env)
            }, message = function(m){
              shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
              assign("temp_str", value = paste(temp_str, m$message), envir = model_executor_env)
            })
          }, error = function(e){
            shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
          })
          
          exec_msg(get("temp_str", envir = model_executor_env))
          if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
            source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
          }
            
          if(exists("dmp.txt")) {
            jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
            dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
            jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                  path = jsonName,
                                  digits = NA,
                                  force = TRUE)
          }
          tend <- Sys.time()
          telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
          execution_time(paste0(
            "Start Time: ", tstart, "<br/>",
            "Finish Time: ", tend, "<br/>",
            "Total Time: ", telapsed))
          toggle_status("idle")
          
          showModal(
            modalDialog(
              title = "Execution Finished",
              htmlOutput("execution_time"),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else if (input$pk_run_mode == "Visual predictive check") {
          
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_convergence\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_test\"]", asis = FALSE)
          shinyjs::hideElement(selector = "li.nav-item a[data-value=\"tab_results_output\"]", asis = FALSE)
          
          execution_directory <- paste0("nlme_shiny_vpc")
          directoryToRun <- file.path(mmdl_file_directory, execution_directory)
          
          dirList <- list.dirs(mmdl_file_directory, recursive = FALSE)
          for(index in 1:999) {
            workingDir <- sprintf("%s_%02d", directoryToRun, index)
            if(!workingDir %in% dirList){
              dir.create(workingDir)
              directoryToRun <- workingDir
              break()
            }
          }
          if(index == 999) {
            stop("cannot create working directory")
          }
          model@modelInfo@workingDir <- directoryToRun
          if (host@isLocal) {
            host@sharedDirectory <- temp_dir
          }
          directoryToRun <- basename(directoryToRun)
          if (file.exists(outputfile)) { 
            directoryToRun <- paste0(";", directoryToRun)
          }
          cat(directoryToRun, file=outputfile, fill = FALSE, append=TRUE)
          
          vpcTables <- tableParamsVPCReactive()
          tstart <- Sys.time()
          temp_str <- ""
          
          tryCatch({  
            withCallingHandlers({
              vpcParams <- NlmeVpcParams(
                numReplicates = as.numeric(input$pk_number_sim_replicates),
                seed = as.numeric(input$pk_seed_sim),
                outputPRED = as.logical(input$pk_output_pred),
                
                stratifyColumns = paste0(if (input$pk_vpc_strat1 != "None") input$pk_vpc_strat1 else "", " ", 
                                         if (input$pk_vpc_strat2 != "None") input$pk_vpc_strat2 else "", " ",
                                         if (input$pk_vpc_strat3 != "None") input$pk_vpc_strat3 else "", " "),
                simulationTables = as.list(vpcTables)
              )
              vpcmodel(model, vpcParams, params, host, runInBackground = FALSE)
            }, error = function(e){
              shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
              assign("temp_str", value = paste(temp_str, e$message), envir = model_executor_env)
            }, warning = function(w){
              shinyjs::html(id = "job_status_text", html = paste0(w$message, "<br>"), add = TRUE)
              assign("temp_str", value = paste(temp_str, w$message), envir = model_executor_env)
            }, message = function(m){
              shinyjs::html(id = "job_status_text", html = gsub("\n", "<br>", m$message), add = TRUE)
              assign("temp_str", value = paste(temp_str, m$message), envir = model_executor_env)
            })
          }, error = function(e){
            shinyjs::html(id = "job_status_text", html = paste0(e$message, "<br>"), add = TRUE)
          })
          
          exec_msg(get("temp_str", envir = model_executor_env))
          if(file.exists(file.path(model@modelInfo@workingDir, "dmp.txt"))) {
            source(file.path(model@modelInfo@workingDir, "dmp.txt"), local = TRUE)
          }
            
          if(exists("dmp.txt")) {
            jsonName <- normalizePath(file.path(model@modelInfo@workingDir,"dmp.json"), mustWork = FALSE)
            dmp.txt$coefficients$fixed <- as.data.frame(as.list(dmp.txt$coefficients$fixed))
            jsonlite::write_json(x = rapply(dmp.txt, as.data.frame, classes = "matrix", how = "replace"),
                                  path = jsonName,
                                  digits = NA,
                                  force = TRUE)
          }
          tend <- Sys.time()
          telapsed <- paste0(round(as.numeric(difftime(tend, tstart, units = "min")), 2), " minutes")
          execution_time(paste0(
            "Start Time: ", tstart, "<br/>",
            "Finish Time: ", tend, "<br/>",
            "Total Time: ", telapsed))
          toggle_status("idle")
          
          showModal(
            modalDialog(
              title = "Execution Finished",
              htmlOutput("execution_time"),
              easyClose = TRUE,
              footer = NULL
            )
          )
        }
        Sys.sleep(3)
        df <- file.info(list.dirs(temp_dir, recursive= FALSE))
        jobFileDir <- rownames(df)[which.max(df$mtime)]
        logfilename(file.path(jobFileDir, sprintf("Shared/jobs/%02d/%d/progress.txt", 1, 1)))
        logfiledir(file.path(jobFileDir, "Shared"))
      } 
    })
    
    # Code Generation ----
    # Engine Parameters
    observeEvent(input$button_engine_params_code,
                 {
                   showNotification(
                     "Generating Code",
                     action = NULL,
                     duration = 2,
                     closeButton = TRUE,
                     id = NULL,
                     type = "message"
                   )
                   code <-
                     expandChain(# Can use 'quote' function to output code as is
                       quote({
                         library(Certara.RsNLME.ModelExecutor)
                       }),
                       engineParamsReactive()
                       )
                   
                   code <- remove_default_generated_parameters(code)
                   code <- remove_unrelated_engine_parameters(code, 
                                                              input$pk_algorithm, 
                                                              input$pk_run_mode,
                                                              input$pk_standard_errors)

                   shinyAce::updateAceEditor(
                     session,
                     "engine_params_code",
                     mode = "r",
                     tabSize = 4,
                     useSoftTabs = FALSE,
                     showInvisibles = FALSE,
                     showLineNumbers = TRUE,
                     # Add line break at commas
                     value = paste0(unlist(gsub(",", ",\n",code)), collapse = "\n")
                   )
                   # material_spinner_hide(session, "me_code")
                 },
                 suspended = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$button_table_params_code,
                 {
                   req(tableParamsReactive())
                   showNotification(
                     "Generating Code",
                     action = NULL,
                     duration = 2,
                     closeButton = TRUE,
                     id = NULL,
                     type = "message"
                   )

                   code <-
                     expandChain(# Can use 'quote' function to output code as is
                       quote({
                         library(Certara.RsNLME.ModelExecutor)
                       }),
                       tableParamsReactive()
                     )
                   
                   shinyAce::updateAceEditor(
                     session,
                     "table_params_code",
                     mode = "r",
                     tabSize = 4,
                     useSoftTabs = FALSE,
                     showInvisibles = FALSE,
                     showLineNumbers = TRUE,
                     value = paste0(unlist(formatTableCode(code, type = "simple")), collapse = "\n")
                   )
                   # material_spinner_hide(session, "me_code")
                 },
                 suspended = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$button_table_sim_params_code,
                 {
                   req(tableParamsSimReactive())
                   showNotification(
                     "Generating Code",
                     action = NULL,
                     duration = 2,
                     closeButton = TRUE,
                     id = NULL,
                     type = "message"
                   )
                   code <-
                     expandChain(# Can use 'quote' function to output code as is
                       quote({
                         library(Certara.RsNLME.ModelExecutor)
                       }),
                       tableParamsSimReactive()
                     )
                   
                   shinyAce::updateAceEditor(
                     session,
                     "table_sim_params_code",
                     mode = "r",
                     tabSize = 4,
                     useSoftTabs = FALSE,
                     showInvisibles = FALSE,
                     showLineNumbers = TRUE,
                     value = paste0(unlist(formatTableCode(code, type = "simulation")), collapse = "\n")
                     
                   )
                   # material_spinner_hide(session, "me_code")
                 },
                 suspended = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$button_table_vpc_params_code,
                 {
                   req(tableParamsVPCReactive())
                   showNotification(
                     "Generating Code",
                     action = NULL,
                     duration = 2,
                     closeButton = TRUE,
                     id = NULL,
                     type = "message"
                   )
                   code <-
                     expandChain(# Can use 'quote' function to output code as is
                       quote({
                         library(Certara.RsNLME.ModelExecutor)
                       }),
                       tableParamsVPCReactive()
                     )
                   
                   shinyAce::updateAceEditor(
                     session,
                     "table_vpc_params_code",
                     mode = "r",
                     tabSize = 4,
                     useSoftTabs = FALSE,
                     showInvisibles = FALSE,
                     showLineNumbers = TRUE,
                     value = paste0(unlist(formatTableCode(code, type = "vpc")), collapse = "\n")
                   )
                   # material_spinner_hide(session, "me_code")
                 },
                 suspended = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$exitShiny, {
      removeModal()
      
      if (from_pirana){
        updateTextInput(session, inputId = "pk_new_mmdl_name", value = paste0("MMDL_", gsub(" ", "_", gsub(":", "_", Sys.time()))))
        
        showModal(
          modalDialog(
            size = "m",
            title = "Exit Model Executor",
            easyClose = TRUE,
            
            fluidRow(
              column(
                width = 12, 
                checkboxInput(inputId = "saveMMDL", label = "Create new mmdl and update estimation args/tables", value = TRUE, width = "100%")
              )
            ),
            fluidRow(
              column(
                width = 12, 
                conditionalPanel(
                  "(input.saveMMDL)",
                  div(style = "padding-top: 5px;"),
                  textInput(inputId = "pk_new_mmdl_name", value = "", label = "New mmdl file name", width = "100%")
                )
              )
            ), 
            div(
              style = "padding-top: 10px;",
              div(
                style = "display: inline-block;",
                actionButton("exitConfirm", "Exit")
              ),
              div(
                style = "display: inline-block;",
                actionButton("exitCancel", "Cancel")
              )
            ),
            
            footer = NULL
          )
        )
        
      } else {
        session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
      }
    })
    
    observeEvent(input$exitCancel, {
      removeModal()
    })
    
    observeEvent(input$exitConfirm, {
      
      filename <- input$pk_new_mmdl_name
      if (filename != "" && input$saveMMDL) {
        
        metaModelRaw <- readBin(metamodel_file_name, "raw", n = 30000)
        metamodelPart <- rawToChar(metaModelRaw)
        
        newFileName <- if (endsWith(filename, ".mmdl")) filename
                       else paste0(filename, ".mmdl")
        
        newModelName <- substr(newFileName, 1, nchar(newFileName) - 5)
        
        fullPath <- file.path(mmdl_file_directory, newFileName) 
        
        OSCR <- if (.Platform$OS.type == "windows") "\r\n" else "\n"
        
        cat(paste0("\n", newModelName), file = outputfile, fill = FALSE, append=TRUE)
            
        text <- OSCR
        if (!input$pk_sort_input) {
          text <- paste0(text, "sort = ", if ((model@hasResetInfo) || (length(model@userDefinedExtraDefs) > 0 && grepl("reset", model@userDefinedExtraDefs))) "FALSE" else (input$pk_sort_input), OSCR)
        }
        if (input$pk_ode != "Matrix Exponent") {
          text <- paste0(text, "ODE = ", ode(), OSCR)
        }
        if (input$pk_ode_rel_tol != 0.000001) {
          text <- paste0(text, "rtolODE = ", input$pk_ode_rel_tol, OSCR)
        }
        if (input$pk_ode_abs_tol != 0.000001) {
          text <- paste0(text, "atolODE = ", input$pk_ode_abs_tol, OSCR)
        }
        if (input$pk_ode_max_step != 50000) {
          text <- paste0(text, "maxStepsODE = ", input$pk_ode_max_step, OSCR)
        }
        if (input$pk_iterations != 1000) {
          text <- paste0(text, "numIterations = ", input$pk_iterations, OSCR)
        }
        if (input$pk_algorithm != "FOCE ELS") {
          text <- paste0(text, "method = ", method(), OSCR)
        }
        std_err <- stderr()
        if (std_err != "Sandwich") {
          text <- paste0(text, "stdErr = ", std_err, OSCR)
        }
        if (input$pk_diff_method != "Central Difference") {
          text <- paste0(text, "isCentralDiffStdErr = FALSE", OSCR)
        }
        if (input$pk_step_size != 0.01) {
          text <- paste0(text, "stepSizeStdErr = ", input$pk_step_size, OSCR)
        }
        if (input$pk_integration_points != 1) {
          text <- paste0(text, "numIntegratePtsAGQ = ", if (input$pk_algorithm == "FOCE ELS" || input$pk_algorithm == "Laplacian") input$pk_integration_points else "1", OSCR)
        }
        if (input$pk_nonparametric && input$pk_nonparam_iterations != 0) {
          text <- paste0(text, "numIterNonParametric = ", if (input$pk_nonparametric && input$pk_algorithm != "Naive Pooled") input$pk_nonparam_iterations else "0", OSCR)
        }
        if (input$pk_synt_gradient) {
          text <- paste0(text, "allowSyntheticGradient = ", if (input$pk_algorithm != "Naive Pooled") input$pk_synt_gradient else FALSE, OSCR)
        }
        if (input$pk_mapnpstart && input$pk_iterations_number != 0) {
          text <- paste0(text, "numIterMAPNP = ", if (input$pk_mapnpstart  && input$pk_algorithm != "Naive Pooled") input$pk_iterations_number else "0", OSCR)
        }
        if (input$pk_pcwres && input$pk_replicates_number != 0) {
          text <- paste0(text, "numRepPCWRES = ", if (input$pk_pcwres  && input$pk_algorithm != "Naive Pooled") input$pk_replicates_number else "0", OSCR)
        }
        if (input$pk_linearization_step != 0.002) {
          text <- paste0(text, "stepSizeLinearize = ", input$pk_linearization_step, OSCR)
        }
        if (input$pk_lagl_ndigit != 7) {
          text <- paste0(text, "numDigitLaplacian = ", input$pk_lagl_ndigit, OSCR)
        }
        if (input$pk_blup_ndigit != 13) {
          text <- paste0(text, "numDigitBlup = ", input$pk_blup_ndigit, OSCR)
        }
        if (input$pk_mapassisst && input$pk_mapassisst_period != 0) {
          text <- paste0(text, "mapAssist = ", if (input$pk_mapassisst && input$pk_algorithm == "QRPEM") input$pk_mapassisst_period else "0", OSCR)
        }
        if (input$pk_number_sample_points != 300) {
          text <- paste0(text, "iSample = ", input$pk_number_sample_points, OSCR)
        }
        if (input$pk_acceptance_ratio != 0.1) {
          text <- paste0(text, "iAcceptRatio = ", input$pk_acceptance_ratio, OSCR)
        }
        if (input$pk_sampling_distribution != "Multivariate Normal") {
          text <- paste0(text, "impDist = ", impsampdof(), OSCR)
        }
        if (input$pk_degrees_freedom != 4) {
          text <- paste0(text, "tDOF = ", if (input$pk_sampling_distribution == "Multivariate t") input$pk_degrees_freedom else "4", OSCR)
        }
        if (input$pk_sir_samples != 10) {
          text <- paste0(text, "numSampleSIR = ", input$pk_sir_samples, OSCR)
        }
        if (input$pk_number_burnin_iter != 0) {
          text <- paste0(text, "numBurnIn = ", if (input$pk_algorithm == "QRPEM") input$pk_number_burnin_iter else "0", OSCR)
        }
        if (input$pk_frozen_omega) {
          text <- paste0(text, "freezeOmega = ", if (input$pk_algorithm == "QRPEM") input$pk_frozen_omega else FALSE, OSCR)
        }
        if (input$pk_mcpem) {
          text <- paste0(text, "MCPEM = ",  if (input$pk_algorithm == "QRPEM") input$pk_mcpem else FALSE, OSCR)
        }
        if (input$pk_run_all_iter) {
          text <- paste0(text, "runAllIterations = ", if (input$pk_algorithm == "QRPEM") input$pk_run_all_iter else FALSE, OSCR)
        }
        if (input$pk_scrambling_method != "Owen") {
          text <- paste0(text, "scramble = ", scramble(), OSCR)
        }
        if (input$pk_part_deriv != 0.00001) {
          text <- paste0(text, "stepSizePartialDeriv = ", input$pk_part_deriv, OSCR)
        }
        if (input$pk_time_steps != 20) {
          text <- paste0(text, "numTimeStepPartialDeriv = ", input$pk_time_steps, OSCR)
        }
        
        saveUpdatedMetamodel(metamodelPart,
                             model,
                             model,
                             fullPath,
                             FALSE,
                             model@modelInfo@modelName,
                             text, "")
        
        count <- get("counter_panels", envir = model_executor_env)
        if (count > 0) {
          simTables <- c()
          for(i in 1:count){
            if(is.null(input[[paste0("panel_", i, "-pk_table_name")]]) || input[[paste0("panel_", i, "-pk_table_name")]] == "")
              next
            
            if (!as.logical(input[[paste0("panel_", i, "-pk_keep")]])) {
              SimTableObs <- NlmeTableDef(
                name = paste0(input[[paste0("panel_", i, "-pk_table_name")]], ".csv"),
                timesList = input[[paste0("panel_", i, "-pk_table_times")]],
                covrSet = input[[paste0("panel_", i, "-pk_table_cov_set")]],
                whenDose = input[[paste0("panel_", i, "-pk_table_dose")]],
                whenObs = input[[paste0("panel_", i, "-pk_table_observe")]],
                variablesList = input[[paste0("panel_", i, "-pk_table_var")]],
                keepSource = FALSE,
                timeAfterDose = as.logical(input[[paste0("panel_", i, "-pk_tad")]]),
                IRES = if (input[[paste0("panel_", i, "-pk_table_observe")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_ires")]]),
                Weight = if (input[[paste0("panel_", i, "-pk_table_observe")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_weight")]]),
                IWRES = if (input[[paste0("panel_", i, "-pk_table_observe")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_iwres")]])
              )
            } else {
              SimTableObs <- NlmeTableDef(
                name = paste0(input[[paste0("panel_", i, "-pk_table_name_keep")]], ".csv"),
                covrSet = input[[paste0("panel_", i, "-pk_table_cov_keep")]],
                whenDose = input[[paste0("panel_", i, "-pk_table_dose_keep")]],
                whenObs = input[[paste0("panel_", i, "-pk_table_observe_keep")]],
                variablesList = input[[paste0("panel_", i, "-pk_table_var_keep")]],
                keepSource = TRUE,
                timeAfterDose = as.logical(input[[paste0("panel_", i, "-pk_tad")]]),
                IRES = if (input[[paste0("panel_", i, "-pk_table_observe_keep")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_ires")]]),
                Weight = if (input[[paste0("panel_", i, "-pk_table_observe_keep")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_weight")]]),
                IWRES = if (input[[paste0("panel_", i, "-pk_table_observe_keep")]] == "") FALSE else as.logical(input[[paste0("panel_", i, "-pk_iwres")]])
              )
            }
            
            simTables <- c(simTables, SimTableObs)
          }
          
          addTablesToColumnMapping(model, as.list(simTables), newFileName, FALSE)
        }
      }
      
      removeModal()
      message("Shiny session has ended. Processing results...")
      
      session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
      stopApp()
    })

    session$onSessionEnded(function() {
      message("Ending session....")
      stopApp()
      message("Shiny session has ended")
      lfd <- isolate({logfiledir()})

     stopFile <- file.path(lfd, sprintf("jobs/%02d/%d/stop.txt", 1, 1))
      if (!is.null(lfd) && file.exists(lfd)) {
        tryCatch(
          expr = {
            cat("STOP", file = stopFile, sep = "\n", append = FALSE)
          },
          error = function(e){
            return(NULL)
          },
          warning = function(w){
            return(NULL)
          }
        )
      }
    })
  
  }
  
  runApp(
    shinyApp(ui = ui, server = server),
    launch.browser = TRUE
  )
}


#' Run Model Executor
#'
#' Used to execute a model developed in \code{Certara.RsNLME} from a Shiny GUI.
#'
#' @param model Model object generated from \code{Certara.RsNLME}.
#' @param hosts One or more hosts generated from \code{Certara.RsNLME::hostParams()}.
#' If missing, the default local host will be used.
#' @param wd Working directory where the model output folders will be created.
#' If missing, the directory specified in the model object will be used \code{model@modelInfo@workingDir}.
#' @param outputfile Text file providing a list of model output subfolders generated inside \code{wd} during the Shiny session. Only applicable for Pirana.
#' @param metamodelFileName Name of the resulting metamodel to generate. Only applicable for Pirana.
#' @param fromPirana Logical; set to \code{TRUE} when launching app from Pirana.
#' @examples 
#' if (interactive()) {
#' model <- Certara.RsNLME::pkmodel(
#'   parameterization = "Clearance",
#'   absorption = "Intravenous",
#'   numCompartments = 2,
#'   data = Certara.RsNLME::pkData,
#'   ID = "Subject",
#'   A1 = "Amount",
#'   Time = "Act_Time",
#'   CObs = "Conc",
#'   modelName = "pk_model")
#' 
#' 
#' modelExecutorUI(model)
#' 
#' }
#'  
#' @return Deploys a Shiny app to execute a \code{Certara.RsNLME} model. Returns \code{NULL} if assigned to an object.
#' @export
modelExecutorUI <-
  function(model,
           hosts,
           wd,
           outputfile = "shiny_dirs.txt",
           metamodelFileName = "temp.mmdl",
           fromPirana = FALSE) {
    
  
  Sys.setenv(RUN_TYPE = "shiny")
  if (file.exists(outputfile)) {
    file.remove(outputfile)
  }
  
  temp <- tempdir(TRUE)
  
  if (missing(hosts)) {
    hosts <- c()
  }
  
  if (!is.vector(hosts)) {
    hosts <- c(hosts)
  }
  
  if (missing(model)) {
    model <- pkmodel(data = Certara.RsNLME::pkData, ID = "Subject", Time = "Act_Time", A1 = "Amount", CObs = "Conc")
  }
  
  if (missing(wd)) {
    wd <- dirname(model@modelInfo@workingDir)
  }
  
  if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
    if (!dir.exists(wd)) {
      warning(paste0("Cannot create directory ", wd, "\nExiting model executor"))
      return("")
    }
  }
  
  if (model@isPopulation == TRUE) {
    if (is.null(model@columnMapping@mapping$id)) {
      stop("Must specify ID if 'isPopulation = TRUE'")
    }
    if (length(model@columnMapping@mapping$id) > 5) {
      stop("number of columns mapped to `ID` must be less than or equal to 5")
    }
  }
  
  if (length(hosts) == 0) {
    hosts = c(NlmeParallelHost(sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
                               installationDirectory = Sys.getenv("INSTALLDIR"),
                               hostName = "Default Local",
                               machineName = "LocalHost",
                               hostType = .Platform$OS.type,
                               numCores = 1,
                               isLocal = TRUE))
  }
  
  statements <- createModelInfo(model)
  allow_gaussian_fit <- grepl("1", statements[grepl("^\\(allowgaussianfit ", statements)])
  subModelStat <- statements[grepl("^\\(submodels", statements)]
  subModels <- unlist(strsplit(unlist(regmatches(subModelStat,gregexpr("(?<=submodels)[ a-zA-Z\\d_]+(?=\\))", subModelStat, perl = TRUE))), ' '))
  
  mappedCols <- c()
  for (i in 1:length(model@columnMapping@mapping)) {
    mappedCols <- c(mappedCols, model@columnMapping@mapping[[i]]@columnName)
  }
  sort_cols <- unique(colnames(model@inputData)[! colnames(model@inputData) %in% mappedCols])

  .run_shiny_RsNLME(model, hosts, allow_gaussian_fit, subModels[subModels != ""], wd, temp, outputfile, metamodelFileName, sort_cols, 
                    fromPirana)

}

stopFuture <- function(pid){
  tools::pskill(pid,signal = tools::SIGTERM)
  tools::pskill(pid,signal = tools::SIGKILL)
}

btnExit <- "
    .btn-exit {
      position: fixed;
      top: 10px;
      right: 10px;
      z-index: 1050;
    }
  "

headerExit <- "<style>
             p.solid {
               border-style: hidden;
               position: fixed;
               top: 0;
               right: 0;
               width: 100%;
               height: 0px;
               opacity: 1;
               text-align: left;
               z-index:99;
             }
           </style>

             <p class='solid'>
             <style>
             .btn {
             bottom: 0;
             right: 0;
             margin-right: 10px;
             margin-left: 25px;
             float:right;
             z-index:100;
             }
             </style>
             <button id='exitShiny' type='button' class='btn btn-default action-button shiny-bound-input'>Exit</button>
                       </p>"



theme_certara <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22, base_rect_size = base_size / 22,
                          grid = c("none", "horizontal", "both")) {
  grid <- match.arg(grid)
  half_line <- base_size / 2
  theme_c <- ggplot2::theme(
    line = ggplot2::element_line(
      colour = "black",
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = "white",
      colour = "black",
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(
      size = ggplot2::rel(0.9),
      colour = "grey40"
    ),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.y = ggplot2::element_text(
      angle = 0,
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.text.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.ticks = ggplot2::element_line(colour = "grey40", size = 0.3),
    axis.ticks.length = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.x = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.x.top = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.x.bottom = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.y = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.y.left = ggplot2::unit(half_line, "pt"),
    axis.ticks.length.y.right = ggplot2::unit(half_line, "pt"),
    axis.title = ggplot2::element_text(colour = "grey40"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line / 2),
      vjust = 1
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line / 2),
      vjust = 0
    ),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line / 2),
      vjust = 1
    ),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line / 2),
      vjust = 0
    ),
    legend.background = ggplot2::element_rect(colour = NA),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = ggplot2::unit(half_line, "pt"),
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.key = ggplot2::element_rect(
      fill = "white", colour = "NA"
    ),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(
      size = ggplot2::rel(0.9),
      margin = ggplot2::margin(r = 2 * half_line, unit = "pt")
    ),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(
      size = ggplot2::rel(0.9),
      hjust = 0
    ),
    legend.title.align = NULL,
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    panel.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    panel.border = ggplot2::element_rect(
      fill = NA,
      colour = "grey60",
      size = 0.3
    ),
    panel.grid = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(
      fill = "grey90", colour = "grey20"
    ),
    strip.text = ggplot2::element_text(
      colour = "grey30",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(0.3 * half_line, 0.3 * half_line, 0.5 * half_line, 0.3 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    plot.background = ggplot2::element_rect(colour = "white"),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
  
  
  
  if (grid == "horizontal") {
    theme_c <- theme_c %+replace% theme(
      panel.grid.major.y = ggplot2::element_line(colour = "grey90", size = 0.3),
      panel.grid.minor.y = ggplot2::element_line(colour = "grey90", size = 0.3)
    )
  } else if (grid == "both") {
    theme_c <- theme_c %+replace% theme(
      panel.grid.major = ggplot2::element_line(colour = "grey90", size = 0.3),
      panel.grid.minor = ggplot2::element_line(colour = "grey90", size = 0.3)
    )
  }
  
  
  
  theme_c
}

stopJobs <- function(jobs_dir){
  jobs <- list.dirs(jobs_dir, recursive = FALSE)
  
  for (job in jobs) {
    print(paste0("terminating jobs in ", job))
    dirs <- list.dirs(job, recursive = FALSE)
    for (dir in dirs) {
      stopFile <- file.path(dir, "stop.txt")
      cat("STOP", file = stopFile, sep = "\n", append = FALSE)
    }
  }
}

toggle_status <- function(status = c("idle", "busy")) {
  status <- match.arg(status)
  if(status == "idle") {
    shinyjs::removeClass(id = "execution_status", class = "ex_status_busy")
    shinyjs::addClass(id = "execution_status", class = "ex_status_idle")
  } else {
    shinyjs::removeClass(id = "execution_status", class = "ex_status_idle")
    shinyjs::addClass(id = "execution_status", class = "ex_status_busy")
  }
}

remove_unrelated_engine_parameters <- function(engine_params, algorithm, run_mode, std_err) {
  
  if (algorithm != "QRPEM") {
    engine_params %<>%
      gsub("( mapAssist = .*?,)", "", .) %>%
      gsub("( iSample = .*?,)", "", .) %>%
      gsub("( iAcceptRatio = .*?,)", "", .) %>%
      gsub("( impDist = .*?,)", "", .) %>%
      gsub("( tDOF = .*?,)", "", .) %>%
      gsub("( numSampleSIR = .*?,)", "", .) %>%
      gsub("( numBurnIn = .*?,)", "", .) %>%
      gsub("( freezeOmega = .*?,)", "", .) %>%
      gsub("( MCPEM = .*?,)", "", .) %>%
      gsub("( runAllIterations = .*?,)", "", .) %>%
      gsub("( scramble = .*?,)", "", .)
  }
  if (algorithm != "Laplacian" && algorithm != "FOCE ELS") {
    engine_params <- gsub("( numIntegratePtsAGQ = .*?,)", "", engine_params)
  }
  if (algorithm == "Naive Pooled") {
    engine_params %<>%
      gsub("( stepSizeLinearize = .*?,)", "", .) %>%
      gsub("( numDigitLaplacian = .*?,)", "", .) %>%
      gsub("( numDigitBlup = .*?,)", "", .) %>%
      gsub("( numIterNonParametric = .*?,)", "", .) %>%
      gsub("( allowSyntheticGradient = .*?,)", "", .) %>%
      gsub("( numIterMAPNP = .*?,)", "", .) %>%
      gsub("( numRepPCWRES = .*?,)", "", .)
  }
  if (algorithm == "IT2S-EM" || run_mode == "Covariate search stepwise" 
      || run_mode == "Covariate search shotgun" || run_mode == "Bootstrap") {
    engine_params %<>%
      gsub("( stdErr = .*?,)", "", .) %>%
      gsub("( isCentralDiffStdErr = .*?,)", "", .) %>%
      gsub("( stepSizeStdErr = .*?,)", "", .)
  }
  if (algorithm == "QRPEM") {
    engine_params <- gsub(" stdErr = \"Sandwich\",", "", engine_params)
  }
  if (run_mode == "Simulation" || run_mode == "Visual predictive check") {
    engine_params %<>%
      gsub("( method = .*?,)", "", .) %>%
      gsub("( numIterations = .*?,)", "", .) %>%
      gsub("( stdErr = .*?,)", "", .) %>%
      gsub("( isCentralDiffStdErr = .*?,)", "", .) %>%
      gsub("( stepSizeStdErr = .*?,)", "", .)
  }
  if (!std_err) {
    engine_params %<>%
      gsub("( isCentralDiffStdErr = .*?,)", "", .) %>%
      gsub("( stepSizeStdErr = .*?,)", "", .)
  }
  engine_params <- gsub(",)", ")", engine_params)
  
  engine_params
}

remove_default_generated_parameters <- function(engine_params) {
  engine_params <- gsub(" sort = TRUE,", "", formatCode(engine_params))
  engine_params %<>%
    gsub(" ODE = \"MatrixExponent\",", "", .) %>%
    gsub(" rtolODE = 1e-06,", "", .) %>%
    gsub(" atolODE = 1e-06,", "", .) %>%
    gsub(" maxStepsODE = 50000,", "", .) %>%
    gsub(" numIterations = 1000,", "", .) %>%
    gsub(" method = \"FOCE-ELS\",", "", .) %>%
    gsub(" stdErr = \"Sandwich\",", "", .) %>%
    gsub(" isCentralDiffStdErr = TRUE,", "", .) %>%
    gsub(" stepSizeStdErr = 0.01,", "", .) %>%
    gsub(" numIntegratePtsAGQ = 1,", "", .) %>%
    gsub(" numIterNonParametric = 0,", "", .) %>%
    gsub(" allowSyntheticGradient = FALSE,", "", .) %>%
    gsub(" numIterMAPNP = 0,", "", .) %>%
    gsub(" numRepPCWRES = 0,", "", .) %>%
    gsub(" stepSizeLinearize = 0.002,", "", .) %>%
    gsub(" numDigitLaplacian = 7,", "", .) %>%
    gsub(" numDigitBlup = 13,", "", .) %>%
    gsub(" mapAssist = 0,", "", .) %>%
    gsub(" iSample = 300,", "", .) %>%
    gsub(" iAcceptRatio = 0.1,", "", .) %>%
    gsub(" impDist = \"Normal\",", "", .) %>%
    gsub(" tDOF = 4,", "", .) %>%
    gsub(" numSampleSIR = 10,", "", .) %>%
    gsub(" numBurnIn = 0,", "", .) %>%
    gsub(" freezeOmega = FALSE,", "", .) %>%
    gsub(" MCPEM = FALSE,", "", .) %>%
    gsub(" runAllIterations = FALSE,", "", .) %>%
    gsub(" scramble = \"Owen\",", "", .) %>%
    gsub(" stepSizePartialDeriv = 1e-05,", "", .) %>%
    gsub(" numTimeStepPartialDeriv = 20", "", .)
  
  engine_params
}