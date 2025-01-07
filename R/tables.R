cov_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cov"))
  )
}

cov_panel <- function(id, num) {
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 output$cov <- renderUI({
                     tags$div(id = id, # style = "width: 95%; margin:auto; padding:10px;",
                              bslib::card(
                                fluidRow(
                                  style = "padding-left: 1rem;",
                                  column(
                                    width =2, style='padding:0.6rem;',
                                    checkboxInput(inputId = ns("pk_keep"), label = "Keep source structure", value = FALSE),
                                  )
                                ),
                                conditionalPanel(
                                  "!input.pk_keep",ns = ns,
                                           fluidRow(
                                             tags$table(
                                               style = "border: 1px solid black; padding: 0px; width: 90%; margin:auto; padding:10px; table-layout: fixed;",
                                               tags$tr(
                                                 tags$th("Table Name"),
                                                 tags$th("Times"),
                                                 tags$th("When Covariate Set"),
                                                 tags$th("When Dose"),
                                                 tags$th("When Observe"),
                                                 tags$th("Variables")
                                               ),
                                               tags$tr(
                                                 tags$td(textInput(inputId = ns("pk_table_name"), value = paste0("Table", num), label = "")),
                                                 tags$td(textInput(inputId = ns("pk_table_times"), value = "", label = "")),
                                                 tags$td(textInput(inputId = ns("pk_table_cov_set"), value = "", label = "")),
                                                 tags$td(textInput(inputId = ns("pk_table_dose"), value = "", label = "")),
                                                 tags$td(textInput(inputId = ns("pk_table_observe"), value = "", label = "")),
                                                 tags$td(textInput(inputId = ns("pk_table_var"), value = "", label = "")),
                                               )
                                             )
                                           )
                                ),
                                conditionalPanel(
                                  "input.pk_keep",ns = ns,
                                           fluidRow(
                                             tags$table(style = "border: 1px solid black; padding: 0px; width: 90%; margin:auto; padding:10px; table-layout: fixed;",
                                           tags$tr(
                                             tags$th("Table Name"),
                                             tags$th("Covariates"),
                                             tags$th("Doses"),
                                             tags$th("Observations"),
                                             tags$th("Variables")
                                           ),
                                           tags$tr(
                                             tags$td(textInput(inputId = ns("pk_table_name_keep"), value = paste0("Table", num), label = "")),
                                             tags$td(textInput(inputId = ns("pk_table_cov_keep"), value = "", label = "")),
                                             tags$td(textInput(inputId = ns("pk_table_dose_keep"), value = "", label = "")),
                                             tags$td(textInput(inputId = ns("pk_table_observe_keep"), value = "", label = "")),
                                             tags$td(textInput(inputId = ns("pk_table_var_keep"), value = "", label = "")),
                                           )
                                          )
                                        )
                                ),
                                            fluidRow(
                                              class = "multi-input-with-checkbox",
                                              style = "padding-left: 1rem",
                                              column(width = 2, style = "padding: 0.6rem;",
                                                              checkboxInput(inputId = ns("pk_tad"), label = "TAD", value = FALSE),
                                              ),
                                              column(
                                                width = 6,
                                                conditionalPanel(
                                                  "(input.pk_table_observe != '' && !input.pk_keep) || (input.pk_table_observe_keep != '' && input.pk_keep)", ns = ns,
                                                  fluidRow(
                                                    column(width = 4, style = "padding: 0.6rem;",
                                                           checkboxInput(inputId = ns("pk_ires"), label = "IRES", value = FALSE),
                                                    ),
                                                    column(width = 4, style = "padding: 0.6rem;",
                                                           checkboxInput(inputId = ns("pk_weight"), label = "Weight", value = FALSE),
                                                    ),
                                                    column(width = 4, style = "padding: 0.6rem;",
                                                           checkboxInput(inputId = ns("pk_iwres"), label = "IWRES", value = FALSE),
                                                    )
                                                  )
                                                )
                                              ),
                                              column(
                                                width = 1, 
                                                offset = 3,
                                                style='padding-left:0.75rem; padding-right:0px; padding-bottom:1rem; position: relative;',
                                                conditionalPanel(
                                                  "(input.pk_table_observe != '' && !input.pk_keep) || (input.pk_table_observe_keep != '' && input.pk_keep)", ns = ns,
                                                  shinyWidgets::actionBttn(ns("removeTblBtn1"), label = NULL, icon = shiny::icon("minus"), style = "material-circle" )
                                                ),
                                                conditionalPanel(
                                                  "(input.pk_table_observe == '' || input.pk_keep) && (input.pk_table_observe_keep == '' || !input.pk_keep)", ns = ns,
                                                  shinyWidgets::actionBttn(ns("removeTblBtn2"), label = NULL, icon = shiny::icon("minus"), style = "material-circle" )
                                                )
                                              )
                                            )
                              )
                     )
                   
                 })
                 
                 outputOptions(output, "cov", suspendWhenHidden = FALSE)  
                 
                 # remove the element
                 observeEvent(input$removeTblBtn1, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-pk_table_name', null)"))
                 })
                 
                 observeEvent(input$removeTblBtn2, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-pk_table_name', null)"))
                 })
                 
               })
}

sim_tables_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tables"))
  )
}

sim_tables_panel <- function(id, strParams, num) {
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 output$tables <- renderUI({
                     tags$div(
                       id = id, 
                       bslib::card(
                         fluidRow(
                           style = "padding-left: 1rem;",
                           column(
                             width =2, style="padding: 0.6rem;",
                             checkboxInput(inputId = ns("pk_sim_keep"), label = "Keep source structure", value = FALSE),
                           )
                         ),
                         conditionalPanel(
                           "!input.pk_sim_keep",ns = ns,
                           fluidRow(
                             tags$table(
                               style = "border: 1px solid black; padding: 0px; width: 90%; margin:auto; padding:10px; table-layout: fixed;",
                               tags$tr(
                                 tags$th("Table Name"),
                                 tags$th("Times"),
                                 tags$th("When Covariate Set"),
                                 tags$th("When Dose"),
                                 tags$th("When Observe"),
                                 tags$th("Variables")
                               ),
                               tags$tr(
                                 tags$td(textInput(inputId = ns("pk_sim_table_name"), value = paste0("SimTable", num), label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_times"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_cov_set"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_dose"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_observe"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_var"), value = "", label = "")),
                               )
                             )
                           )
                         ),
                         conditionalPanel(
                           "input.pk_sim_keep",ns = ns,
                           fluidRow(
                             tags$table(
                               style = "border: 1px solid black; padding: 0px; width: 90%; margin:auto; padding:10px; table-layout: fixed;",
                               tags$tr(
                                 tags$th("Table Name"),
                                 tags$th("Covariates"),
                                 tags$th("Doses"),
                                 tags$th("Observations"),
                                 tags$th("Variables")
                               ),
                               tags$tr(
                                 tags$td(textInput(inputId = ns("pk_sim_table_name_keep"), value = paste0("SimTable", num), label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_cov_keep"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_dose_keep"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_observe_keep"), value = "", label = "")),
                                 tags$td(textInput(inputId = ns("pk_sim_table_var_keep"), value = "", label = "")),
                               )
                             )
                           )
                         ),
                         fluidRow(
                           class = "multi-input-with-checkbox",
                           style = "padding-left: 1rem",
                           column(
                             width = 1, 
                             checkboxInput(inputId = ns("pk_sim_tad"), label = "TAD", value = FALSE),
                           ),
                           column(
                             width = 2, offset = 8, style='padding-left:20px; padding-right:0px; padding-top:10px; padding-bottom:1rem;',
                             shiny::actionButton(ns("strParamBtn"), label = "Structural Parameters")
                           ),
                           column(
                             width = 1, style = "padding-left:0.6rem; padding-right:0px; padding-top:0px; padding-bottom:1rem; position: relative;",
                             shinyWidgets::actionBttn(ns("removeTblBtn"), label = NULL, icon = shiny::icon("minus"), style = "material-circle" )
                           )
                         )
                       )
                     )
                 })
                 
                 outputOptions(output, "tables", suspendWhenHidden = FALSE)  
                 
                 observeEvent(input$removeTblBtn, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-pk_sim_table_name', null)"))
                 })
                 
                 observeEvent(input$strParamBtn,  {
                   print("Success")
                   print(paste0(id, "-pk_sim_table_var"))
                   runjs(paste0('$("#', id, '-pk_sim_table_var").val("', strParams, '").trigger("change");'))
                   runjs(paste0('$("#', id, '-pk_sim_table_var_keep").val("', strParams, '").trigger("change");'))
                   # updateTextInput(session, inputId = paste0(id, "-pk_sim_table_var"), value = "strParams")
                   # updateTextInput(session, inputId = paste0(id, "-pk_sim_table_var_keep"), value = "strParams")
                 })
               })
}

scenarios_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("scenarios"))
  )
}

scenarios_panel <- function(id, covarList, num) {
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 output$scenarios <- renderUI({
                     tags$div(
                       id = id, 
                       bslib::card(
                         tags$table(
                           style = "border: 1px solid black; padding: 0px; width: 100%; table-layout: fixed; font-size: small;",
                           tags$tr(
                             tags$th("Scenario"),
                             lapply(1:length(covarList), function(i) {
                               tags$th(covarList[i])
                             }),
                             tags$th("Annotation")
                           ),
                           tags$tr(
                             tags$td(textInput(inputId = ns("pk_scenario_name"), value = paste0("sc000", num), label = "")),
                             lapply(1:length(covarList), function(i) {
                               tags$td(checkboxInput(inputId = ns(i), label = "", value = FALSE),)
                             }),
                             tags$td(textInput(inputId = ns("pk_annotation"), value = "", label = ""))
                           )
                         ),
                         fluidRow(
                           class = "multi-input-with-checkbox",
                           style = "padding-left: 1rem",
                           column(
                             width = 1, offset = 11,
                             style = "padding-left:0.6rem; padding-right:0px; padding-top:0px; padding-bottom:1rem; position: relative;",
                             shinyWidgets::actionBttn(ns("removeRowBtn"), label = NULL, icon = shiny::icon("minus"), style = "material-circle" )
                           )
                         )
                       )
                     )
                   
                 })
                 
                 outputOptions(output, "scenarios", suspendWhenHidden = FALSE)  
                 
                 observeEvent(input$removeRowBtn, {
                   removeUI(selector = paste0("div#", id))
                   runjs(paste0("Shiny.onInputChange('", id, "-pk_scenario_name', null)"))
                 })
               })
}

vpc_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("vpc"))
  )
}

vpc_panel <- function(id, obsName, isCat, isBql) {
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 output$vpc <- renderUI({
                     tags$div(id = id,
                              bslib::card(
                                bslib::card_header(obsName),
                                # title = obsName,
                                            fluidRow(
                                              column(
                                                width = 1, style = "padding:10px;",
                                                selectInput(inputId = ns("pk_xaxis"), label = "X axis", choices = c("t", "TAD", "PRED"), selected = "t")
                                              ),
                                              column(
                                                width = 1, style = "padding:10px;",
                                                selectInput(inputId = ns("pk_bin_method"), label = "Binning option", choices = c("None", "K-means", "Explicit centers","Explicit boundaries"), selected = "None")
                                              ),
                                              column(
                                                width = 2, style = "padding:10px;",
                                                textInput(inputId = ns("pk_bin_option"), value = "", label = "Centers/Boundaries"),
                                              ),
                                              column(
                                                width = 2, style = "padding:10px;",
                                                textInput(inputId = ns("pk_quant"), value = "", label = "Quantiles %"),
                                              ),
                                              column(
                                                width = 2, style = "padding:10px;",
                                                textInput(inputId = ns("pk_quant_conf_int"), value = "", label = "Conf. intervals for predicted quantiels %"),
                                              ),
                                              #conditionalPanel(
                                              #  condition = "output.isBql",
                                              #  column(
                                              #    width = 3, style = "padding:35px;",
                                              #    checkboxInput(inputId = "pk_as_lloq", label = "Treat as LLOQ", value = FALSE),
                                              #  )
                                              #),
                                              #conditionalPanel(
                                              #  condition = "output.isCat",
                                              #  column(
                                              #    width = 2, style = "padding:10px;",
                                              #    textInput(inputId = ns("pk_group"), value = "", label = "Boundaries for y value grouping"),
                                              #  )
                                              #)
                                            )
                              )
                     )
                 })
               })
}

formatTableCode <- function(code, type = c("simple", "simulation", "vpc")){
  type <- match.arg(type)
  code <- shinymeta::formatCode(code) %>%
    str_extract_all("tableParams*(.*?)*E\\)")
  
  if(type == "simulation") {
    varname <- "simTables <- "
  } else if (type == "vpc") {
    varname <- "vpcTables <- "
  } else {
    varname <- "tables <- "
  }
  
  code <- paste0(varname, "c(\n\t", 
                 paste0(unlist(code), collapse = ",\n\t"),
                 "\n)")

  code
}