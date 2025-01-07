
generate_ui <- function(hosts, sort_cols, covariate_list, isPopulation){
  if (any(unlist(lapply(covariate_list, function(x) x@type %in% c(Certara.RsNLME:::COVAR_CATEGORY, Certara.RsNLME:::COVAR_OCCASION))))){
    stratas <- unlist(lapply(covariate_list, function(x){ if (x@type %in% c(Certara.RsNLME:::COVAR_CATEGORY, Certara.RsNLME:::COVAR_OCCASION)) x@name}))
  } else {
    stratas <- NULL
  }
  
  if (!isPopulation){
    choices_run_mode <-  c("Simple", "Simulation")
    choices_algorithm <- c("Naive Pooled")
    selected_algorithm <- c("Naive Pooled")
    choices_method <- c("Hessian")
    selected_method <- c("Hessian")
  } else {
    choices_run_mode <-  c("Simple", "Scenarios", "Covariate search stepwise", "Covariate search shotgun", "Bootstrap",  "Visual predictive check", "Simulation")
    choices_algorithm <- c("FOCE ELS", "FOCE L-B", "FO", "Laplacian", "Naive Pooled", "QRPEM", "IT2S-EM")
    selected_algorithm <- c("FOCE ELS")
    choices_method <- c("Sandwich", "Hessian", "Fisher Score", "Auto-detect")
    selected_method <- c("Sandwich")
  }
 
  jsFunctions <- "shinyjs.closewindow = function() { window.close(); }"

  ui <- tagList(
    
    # ShinyJS ----
    shinyjs::useShinyjs(),
    
    shinyjs::extendShinyjs(
      text = jsFunctions,
      functions = c("closewindow")
    ),
    
    tags$head(tags$style(styleCSS)),
    tags$head(
      tags$script(
        '
        Shiny.addCustomMessageHandler("scrollCallback",
          function(color) {
            var objDiv = document.getElementById("job_status_modal");
            objDiv.scrollTop = objDiv.scrollHeight;
          }
        );'
      )
    ),
    
    # Page ----
    bslib::page_fluid(
      class = "certara-page",
      title = "Model Executor",
      
      # Header ----
      certara_header("Model Executor"),
      
      div(
        class = "main-content",
        
        # Execution Inputs Card ----
        bslib::card(
          class = "execution_inputs",
          fluidRow(
            column(style = "display: flex; justify-content: right; align-self: center; padding-top: 0.75rem;",
              width = 2, 
              h5("Run Mode:"),
            ),
            column(
                   width = 3,
                   selectInput(inputId = "pk_run_mode", 
                               label = "", 
                               choices = choices_run_mode, 
                               selected = "Simple",
                               width = "100%")
            ),
            column(style = "display: flex; justify-content: right; align-self: center; padding-top: 0.75rem;",
              width = 1,
              h5("Host:"),
            ),
            column(
                   width = 3,
                   selectInput(inputId = "pk_execute_on", 
                               label = "", 
                               choices = lapply(hosts, function(x) x@hostName), 
                               selected = "Local",
                               width = "100%")
            ),
            column(width = 1,
                   # bslib::input_task_button(id = "execution_status", 
                   #                          label = "Execute Model",
                   #                          label_busy = "Executing...",
                   #                          icon = icon("play-circle", verify_fa = FALSE), 
                   #                          class = "ex_status_idle")
                   actionLink("execution_status", 
                              label = "", 
                              icon = icon("play-circle", verify_fa = FALSE), 
                              class = "ex_status_idle")
            )
          )
        ),
        
        # Main Card ----
        bslib::navset_card_underline(
        
          # Options Panel ----
          bslib::nav_panel(
            title = "Options",
            
            ## Title ----
            fluidRow(
              style = "padding-left: 1rem;",
              column(
                width = 3, 
                conditionalPanel(
                  "(input.pk_run_mode != 'Simulation') && (input.pk_run_mode != 'Visual predictive check')",
                  h4("Estimation")
                ),
                conditionalPanel(
                  "(input.pk_run_mode == 'Simulation')",
                  h4("Simulation Setup")
                ),
                conditionalPanel(
                  "(input.pk_run_mode == 'Visual predictive check')",
                  h4("VPC Setup")
                )
              )
            ),
            
            ## Options ----
            fluidRow(
              class = "multi-input-with-checkbox",
              style = "padding-left: 1rem;",
              column(
                width = 2, 
                style = "align-self: flex-start;",
                selectInput(inputId = "pk_algorithm", label = "Algorithm", choices = choices_algorithm, selected = selected_algorithm)
              ),
              column(
                width = 2, 
                style = "align-self: flex-start;",
                selectInput(inputId = "pk_ode", label = "ODE Solver", choices = c("Matrix Exponent", "Auto-detect", "Non-stiff DVERK","Non-stiff DOPRI5" , "Stiff"), selected = "Matrix Exponent")
              ),
              column(
                width = 2,
                style = "align-self: flex-start;",
                conditionalPanel(
                  "(input.pk_run_mode != 'Simulation') && (input.pk_run_mode != 'Visual predictive check')",
                  fluidRow(
                    column(
                      width = 12, 
                      textInput(inputId = "pk_iterations", value = 1000, label = "Maximum Number of Iterations"),
                      shiny::verbatimTextOutput("pk_iterations_error")
                    )
                  )
                ),
                conditionalPanel(
                  "(output.isPopulation && (input.pk_run_mode == 'Simulation')) || (input.pk_run_mode == 'Visual predictive check')",
                  fluidRow(
                    column(
                      width = 12, 
                      textInput(inputId = "pk_seed_sim", value = 1234, label = "Seed Number"),
                    )
                  )
                )
              ),
              column(
                width = 2,
                style = "align-self: flex-start;",
                id = "pk_num_repl_col",
                textInput(inputId = "pk_number_sim_replicates", value = 100, label = "Number of Replicates"),
              ),
              column(
                width = 2,
                style = "align-self: flex-start; padding-top: 2rem;",
                id = "pk_sort_input_col",
                checkboxInput(inputId = "pk_sort_input", label = "Sort Input", value = TRUE)
              ) %>% shinyjs::hidden()
            ),
            
            ## Sort Columns ----
            fluidRow(
              style = "padding-left: 1rem;",
              column(
                width = 2,
                selectInput(inputId = "pk_sort_col1", label = "Sort Column", choices = c(" ", sort_cols), selected = " ")
              ),
              column(
                width = 2,
                selectInput(inputId = "pk_sort_col2", label = "Sort Column", choices = c(" ", sort_cols), selected = " ")
              ),
              column(
                width = 2,
                selectInput(inputId = "pk_sort_col3", label = "Sort Column", choices = c(" ", sort_cols), selected = " ")
              ),
              column(
                width = 2,
                selectInput(inputId = "pk_sort_col4", label = "Sort Column", choices = c(" ", sort_cols), selected = " ")
              ),
              column(
                width = 2,
                selectInput(inputId = "pk_sort_col5", label = "Sort Column", choices = c(" ", sort_cols), selected = " ")
              )
            ),
            
            ## VPC Options ----
            conditionalPanel(
              "(input.pk_run_mode == 'Visual predictive check')",
              fluidRow(
                class = "multi-input-with-checkbox",
                style = "padding-left: 1rem;",
                column(
                  class = "col-checkbox",
                  width = 2, 
                  checkboxInput(inputId = "pk_output_pred", label = "Output PRED", value = FALSE),
                ),
                
                conditionalPanel(
                  condition = "output.stratifyCols",
                  column(
                    width = 1,
                    style = "display: flex; margin-bottom: 1rem; justify-content: right;",
                    h6("Stratify:"),
                  ),
                  column(
                    width = 2, 
                    selectInput(inputId = "pk_vpc_strat1", label = "", choices = c("None", stratas), selected = "None")
                  ),
                  column(
                    width = 2, 
                    selectInput(inputId = "pk_vpc_strat2", label = "", choices = c("None", stratas), selected = "None")
                  ),
                  column(
                    width = 2, 
                    selectInput(inputId = "pk_vpc_strat3", label = "", choices = c("None", stratas), selected = "None")
                  )
                )
              )
            ),
            
            ## Advanced Options ----
            bslib::accordion(
              open = FALSE,
              
              bslib::accordion_panel(
                title = h6("Advanced Options"), 
                value = "Advanced Options",
                
                fluidRow(
                  class = "large-labels",
                  
                  ### Left Column (Non-QRPEM) ---- 
                  column(
                    width = 6, 
                    
                    #### Row 1 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'FOCE ELS') || (input.pk_algorithm == 'Laplacian')",
                      fluidRow(
                        column(
                          width = 4, 
                          textInput(inputId = "pk_integration_points", value = 1, label = "Number of Integration Points for AGQ"),
                          shiny::verbatimTextOutput("pk_integration_points_error")
                        )
                      )
                    ),
                    
                    #### Row 2 ----
                    conditionalPanel(
                      "(input.pk_algorithm != 'Naive Pooled')",
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_nonparametric", label = "Non Parametric", value = FALSE),
                        ),
                        column(
                          width = 4, 
                          conditionalPanel(
                            "(input.pk_nonparametric)",
                            textInput(inputId = "pk_nonparam_iterations", value = 1, label = "Number of Non Parametric Iterations"),
                            shiny::verbatimTextOutput("pk_nonparam_iterations_error")
                          )
                        )
                      )
                    ),
                    
                    #### Row 3 ----
                    conditionalPanel(
                      "(input.pk_algorithm != 'Naive Pooled')",
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_mapnpstart", label = "MAP-NP Start", value = FALSE),
                        ),
                        column(
                          width = 4, 
                          conditionalPanel(
                            "(input.pk_mapnpstart)",
                            textInput(inputId = "pk_iterations_number", value = 3, label = "Number of Iterations"), 
                            shiny::verbatimTextOutput("pk_iterations_number_error")
                          ),
                        ),
                      ),
                    ),
                    
                    #### Row 4 ----
                    conditionalPanel(
                      "(input.pk_algorithm != 'Naive Pooled')",
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_pcwres", label = "PCWRES", value = FALSE),
                        ),
                        column(
                          width = 4, 
                          conditionalPanel(
                            "(input.pk_pcwres)",
                            textInput(inputId = "pk_replicates_number", value = 100, label = "Number of Replicates"),
                            shiny::verbatimTextOutput("pk_replicates_number_error")
                          ),
                        ),
                      ),
                    ),
                    
                    #### Row 5 ----
                    conditionalPanel(
                      condition = 'output.isPopulation && input.pk_algorithm != "Naive Pooled"',
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_synt_gradient", label = "Synthetic Gradients", value = FALSE),
                        ),
                      ),
                    ),
                    
                    #### Row 6 ----
                    conditionalPanel(
                      condition = 'output.isPopulation && input.pk_algorithm != "Naive Pooled"',
                      fluidRow(class = "options-row",
                        column(
                          width = 4, 
                          textInput(inputId = "pk_lagl_ndigit", value = 7, label = "LAGL nDigit"),
                        ),
                        column(
                          width = 4, 
                          textInput(inputId = "pk_blup_ndigit", value = 13, label = "BLUP nDigit"),
                        ),
                        column(
                          width = 4, 
                          textInput(inputId = "pk_linearization_step", value = 0.002, label = "Model Linearization Step Size"),
                        ),
                      ),
                    ),
                    
                    #### Row 7 ----
                    conditionalPanel(
                      condition = '!output.isPopulation',
                      fluidRow(class = "options-row large-labels",
                        column(
                          width = 4, 
                          textInput(inputId = "pk_part_deriv", value = 0.00001, label = "Step Size to Calculate Partial Derivative"),
                        ),
                        column(
                          width = 4, 
                          textInput(inputId = "pk_time_steps", value = 20, label = "Number of Time Steps to Output Partial Derivative"),
                        ),
                      ),
                    ),
                    
                    #### Row 8 ----
                    fluidRow(class = "options-row",
                      column(
                        width = 4, 
                        textInput(inputId = "pk_ode_rel_tol", value = 0.000001, label = "ODE Rel. Tol"),
                      ),
                      column(
                        width = 4, 
                        textInput(inputId = "pk_ode_abs_tol", value = 0.000001, label = "ODE Abs. Tol"),
                      ),
                      column(
                        width = 4, 
                        textInput(inputId = "pk_ode_max_step", value = 50000, label = "ODE Max step"),
                      ),
                    ),
                  ),
                  
                  ### Right Column (QRPEM) ----
                  column(
                    width = 6,
                    
                    #### Row 1 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'QRPEM')",
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_mapassisst", label = "MAP Assist", value = FALSE),
                        ),
                        column(
                          width = 4, 
                          conditionalPanel(
                            "(input.pk_mapassisst)",
                            textInput(inputId = "pk_mapassisst_period", value = 1, label = "Periodicity for MAP Assist"), ),
                        ),
                      ),
                    ),
                    
                    #### Row 2 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'QRPEM')",
                      fluidRow(
                        class = "options-row",
                        column(
                          width = 4, 
                          textInput(inputId = "pk_number_sample_points", value = 300, label = "Number of Sample Points"),
                        ),
                        column(
                          width = 4, 
                          textInput(inputId = "pk_acceptance_ratio", value = 0.1 , label = "Acceptance Ratio"), 
                        )
                      )
                    ),
                    
                    #### Row 3 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'QRPEM')",
                      fluidRow(
                        class = "options-row",
                        column(
                          width = 4, 
                          selectInput(inputId = "pk_sampling_distribution", label = "Importance Sampling Distribution", choices = c("Multivariate Normal", "Direct Sampling", "Multivariate Laplace", "Multivariate t", "Mixture-2", "Mixture-3"), selected = "Multivariate Normal")
                        ),
                        column(
                          width = 4,
                          conditionalPanel(
                            "(input.pk_sampling_distribution == 'Multivariate t')",
                            # style = "padding: 0 0.7rem 1rem 0.7rem;",
                            # column(
                            #   style = "padding: inherit;",
                            #   width = 4, 
                              textInput(inputId = "pk_degrees_freedom", value = 4, label = "Degrees of Freedom (between 3 and 30)"),
                              shiny::verbatimTextOutput("pk_degrees_freedom_error")
                            # )
                          )
                        )
                      )
                    ),
                    
                    #### Row 4 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'QRPEM')",
                      fluidRow(
                        class = "options-row",
                        column(
                          width = 4, 
                          selectInput(inputId = "pk_scrambling_method", label = "Quasi-Random Scrambling Method", choices = c("None", "Owen", "Tezuka-Faur"), selected = "Owen")
                        ),
                        column(
                          width = 4, 
                          textInput(inputId = "pk_sir_samples", value = 10, label = "Number of SIR Samples"),
                        )
                      )
                    ),
                    
                    #### Row 5 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'QRPEM')",
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          width = 4, 
                          textInput(inputId = "pk_number_burnin_iter", value = 0, label = "Number of Burn-In Iterations"), 
                        ),
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_frozen_omega", label = "Frozen Omega during Burn-In", value = FALSE),
                        )
                      )
                    ),
                    
                    #### Row 6 ----
                    conditionalPanel(
                      "(input.pk_algorithm == 'QRPEM')",
                      fluidRow(
                        class = "multi-input-with-checkbox options-row",
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_mcpem", label = "MCPEM", value = FALSE),
                        ),
                        column(
                          class = "col-checkbox",
                          width = 4, 
                          checkboxInput(inputId = "pk_run_all_iter", label = "Run All Requested Iterations", value = FALSE),
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            ## More Options ----              
                
              ### Simple/Scenarios (!IT2S-EM) ----
                conditionalPanel(
                  "(input.pk_algorithm != 'IT2S-EM') && ((input.pk_run_mode == 'Simple') || (input.pk_run_mode == 'Scenarios') )",
                  fluidRow(
                    style = "padding-left: 1rem;",
                    column(
                      width = 2,
                      checkboxInput(inputId = "pk_standard_errors", label = "Standard Errors", value = TRUE)
                    )
                  ),
                  conditionalPanel(
                    "(input.pk_standard_errors)",
                    fluidRow(
                      style = "padding-left: 1rem;",
                      column(
                        width = 3, 
                        selectInput(inputId = "pk_method", label = "Method", choices = choices_method, selected = selected_method)
                      ),
                      column(
                        width = 3, 
                        textInput(inputId = "pk_conf_level", value = 95, label = "Confidence Level (%)"),
                        shiny::verbatimTextOutput("pk_conf_level_error")
                      ),
                      column(
                        width = 3, 
                        selectInput(inputId = "pk_diff_method", label = "Finite Difference Method", choices = c("Central Difference", "Forward Difference"), selected = "Central Difference")
                      ),
                      column(
                        width = 2,
                        conditionalPanel(
                          condition = 'output.isPopulation',
                          textInput(inputId = "pk_step_size", value = 0.01,  label = "Step Size")
                        )
                      )
                    )
                  )
                ),
                
              ### Bootstrap ----
                conditionalPanel(
                  "(input.pk_run_mode == 'Bootstrap')",
                  fluidRow(
                    style = "padding-left: 1rem;",
                    column(
                      width = 2,
                      h6("Bootstrap Setup")
                    ) 
                  ),
                  fluidRow(
                    class = "multi-input-with-checkbox",
                    style = "padding-left: 1rem;",
                    column(
                      width = 2, 
                      textInput(inputId = "pk_number_bootstrap_samples", value = 100, label = "Number of Bootstrap Samples"),
                    ),
                    column(
                      width = 2, 
                      textInput(inputId = "pk_max_retries", value = 2, label = "Maximum Number of Retries"),
                      shiny::verbatimTextOutput("pk_max_retries_error")
                    ),
                    column(
                      width = 2, 
                      textInput(inputId = "pk_seed", value = 1234, label = "Seed Number"),
                    ),
                    column(
                      class = "col-checkbox",
                      width = 2, 
                      checkboxInput(inputId = "pk_init_est", label = "Estimate Initial Values", value = FALSE),
                    ),
                    column(
                      width = 2, 
                      textInput(inputId = "pk_conf_level_boot", value = 95, label = "Confidence Level"),
                      shiny::verbatimTextOutput("pk_conf_level_boot_error")
                    )
                  ),
                  conditionalPanel(
                    condition = 'output.stratifyCols',
                    fluidRow(
                      class = "multi-input-with-checkbox",
                      style = "padding-left: 1rem;",
                      column(
                        width = 1, offset = 1, 
                        style = "display: flex; margin-bottom: 1rem; justify-content: right;",
                        h6("Stratify:"),
                      ),
                      column(
                        width = 2, 
                        selectInput(inputId = "pk_boot_strat1", label = "", choices = c("None", stratas), selected = "None")
                      ),
                      column(
                        width = 2, 
                        selectInput(inputId = "pk_boot_strat2", label = "", choices = c("None", stratas), selected = "None")
                      ),
                      column(
                        width = 2, 
                        selectInput(inputId = "pk_boot_strat3", label = "", choices = c("None", stratas), selected = "None")
                      )
                    )
                  )
                ),
                
              ### Stepwise ----
              conditionalPanel(
                "(input.pk_run_mode == 'Covariate search stepwise')",
                fluidRow(
                  style = "padding-left: 1rem;",
                  column(
                    width = 2,
                    h6("Stepwise Setup")
                  )
                ), 
                fluidRow(
                  style = "padding-left: 1rem;",
                  column(
                    width = 2, 
                    selectInput(inputId = "pk_criterion", label = "Criterion", choices = c("-2LL", "AIC", "BIC"), selected = "-2LL")
                  ),
                  column(
                    width = 4,
                    conditionalPanel(
                      "(input.pk_criterion == '-2LL')",
                      fluidRow(
                        column(
                          width = 6, 
                          textInput(inputId = "pk_add_p", value = 0.01, label = "P-value used to add a covariate"),
                        ),
                        column(
                          width = 6, 
                          textInput(inputId = "pk_remove_p", value = 0.001, label = "P-value used to remove a covariate"),
                        )
                      )
                    ),
                    conditionalPanel(
                      "(input.pk_criterion != '-2LL')",
                      fluidRow(
                        column(
                          width = 6, 
                          textInput(inputId = "pk_add_thresh", value = 6.635, label = "Threshold to add a covariate"),
                        ),
                        column(
                          width = 6, 
                          textInput(inputId = "pk_remove_thresh", value = 10.828, label = "Threshold to remove a covariate")
                        )
                      )
                    )
                  )
                )
              ),
            
            ## Code Generator ----
            fluidRow(
              column(width = 12,
                bslib::card(
                  style = "height: 150px; max-height: 700px; overflow-y: scroll; overflow-x: hidden;",
                  shinyAce::aceEditor(
                    outputId = "engine_params_code",
                    autoScrollEditorIntoView = TRUE,
                    minLines = 5,
                    maxLines = 50,
                    value = NULL,
                    readOnly = TRUE,
                    placeholder = "Click the generate button to output R code..."
                  )
                ),
                # bslib::input_task_button(
                #   id = "button_engine_params_code",
                #   label = "Generate",
                #   label_busy = "Generating...",
                #   icon = icon("code", lib = "font-awesome")
                # )
                actionButton(inputId = "button_engine_params_code", label = "Generate", icon = icon("code", lib = "font-awesome"))
              )
            )
          ),
          
        # Table Panel ----
          bslib::nav_panel(
            title = "Tables",
            value = "tab_tables",

            div(
              
              ## Simple ----
              conditionalPanel(
                "(input.pk_run_mode == 'Simple')",
                div(style = "padding:5px;"),
                div(id = "add_panels_here"),
                fluidRow(
                  class = "multi-input-with-checkbox",
                  column(
                    width = 1, offset = 10,
                    style = "display: flex; justify-content: right; padding-right: 1rem;",
                    h6("Add Table:"),
                  ),
                  column(
                    width = 1, 
                    shinyWidgets::actionBttn("addTable", label = NULL, icon = shiny::icon("plus"), style = "material-circle" ),
                  )
                )
              ),
              
              ## Simulation / VPC ----
              conditionalPanel(
                "(input.pk_run_mode == 'Simulation' || input.pk_run_mode == 'Visual predictive check')",
                div(style = "padding:5px;"),
                conditionalPanel(
                  "!output.isPopulation",
                  fluidRow(
                    class = "multi-input-with-checkbox",
                    style = "padding-left: 1rem;",
                    column(
                      width = 2, 
                      textInput(inputId = "pk_num_sim_points", value = 100, label = "Number of Simulation Points"),
                    ),
                    column(
                      width = 2, 
                      textInput(inputId = "pk_max_x_range", value = 50, label = "Maximum X Range"),
                    ),
                    column(
                      width = 2, 
                      textInput(inputId = "pk_y_var", value = "", label = "Y Variables"),
                    ),
                    column(
                      class = "col-checkbox",
                      width = 2, 
                      checkboxInput(inputId = "pk_sim_at_obs", label = "Simulation at Observations", value = TRUE),
                    ),
                  ),
                ),
                conditionalPanel("input.pk_run_mode == 'Simulation'",
                  div(id = "sim_tables_placeholder"),
                  fluidRow(
                    class = "multi-input-with-checkbox",
                    column(
                      width = 1, offset = 10,
                      style = "display: flex; justify-content: right; padding-right: 1rem;",
                      h6("Add Table:"),
                    ),
                    column(
                      width = 1,
                      shinyWidgets::actionBttn("addSimTable", label = NULL, icon = shiny::icon("plus"), style = "material-circle" ),
                    )
                  )
                ),
                conditionalPanel("input.pk_run_mode == 'Visual predictive check'",
                  div(id = "vpc_tables_placeholder"),
                  fluidRow(
                    class = "multi-input-with-checkbox",
                    column(
                      width = 1, offset = 10, 
                      style = "display: flex; justify-content: right; padding-right: 1rem;",
                      h6("Add Table:"),
                    ),
                    column(
                      width = 1, 
                      shinyWidgets::actionBttn("addVPCTable", label = NULL, icon = shiny::icon("plus"), style = "material-circle" )
                    )
                  )
                )
              ),
              
              ## Scenarios ----
              conditionalPanel(
                "(input.pk_run_mode == 'Scenarios')",
                fluidRow(
                  style = "padding-left: 2rem;",
                  h6("Scenario:")
                ), 
                div(style = "padding:5px;"),
                div(id = "scenarios_table_placeholder"),
                fluidRow(
                  class = "multi-input-with-checkbox",
                  column(
                    width = 1, offset = 10, 
                    style = "display: flex; justify-content: right; padding-right: 1rem;",
                    h6("Add Scenario:"),
                  ),
                  column(
                    width = 1, 
                    shinyWidgets::actionBttn("addScenario", label = NULL, icon = shiny::icon("plus"), style = "material-circle" ),
                  )
                ),
              ),
              
              ## Profile ----
              conditionalPanel(
                "(input.pk_run_mode == 'Profile')",
                fluidRow(
                  style = "padding-left: 2rem;",
                  h6("Profile Setup")
                ), 
                div(style = "padding:5px;"),
                
                uiOutput("profileTable"),
                
                
                fluidRow(
                  style = "padding-left: 2rem;",
                  column(
                    width = 2, style = "padding:10px;",
                    selectInput(inputId = "pk_perturbation", label = "", choices = c("Perturbation %", "Perturbation Amt"), selected = "Perturbation %")
                  )
                )
              )
            ),
            
            ## Code Generators ----
            fluidRow(
              style = "padding:10px;",
              
              ### Simple ---- 
              conditionalPanel(
                "input.pk_run_mode == 'Simple'",
                column(
                  width = 12,
                  bslib::card(
                    style = "height: 150px; max-height: 700px; overflow-y: scroll; overflow-x: hidden;",
                    shinyAce::aceEditor(
                      outputId = "table_params_code",
                      autoScrollEditorIntoView = TRUE,
                      minLines = 5,
                      maxLines = 50,
                      value = NULL,
                      readOnly = TRUE,
                      placeholder = "Click the generate button to output R code..."
                    )
                  ),
                  actionButton(inputId = "button_table_params_code", label = "Generate", icon = icon("code", lib = "font-awesome"))
                )
              ),
              
              ### Simulation ----
              conditionalPanel(
                "input.pk_run_mode == 'Simulation'",
                column(
                  width = 12,
                  bslib::card(
                    style = "height: 150px; max-height: 700px; overflow-y: scroll; overflow-x: hidden;",
                    shinyAce::aceEditor(
                      outputId = "table_sim_params_code",
                      autoScrollEditorIntoView = TRUE,
                      minLines = 5,
                      maxLines = 50,
                      value = NULL,
                      readOnly = TRUE,
                      placeholder = "Click the generate button to output R code..."
                    )
                  ),
                  actionButton(inputId = "button_table_sim_params_code", label = "Generate", icon = icon("code", lib = "font-awesome"))
                )
              ),
              
              ### VPC ----
              conditionalPanel(
                "input.pk_run_mode == 'Visual predictive check'",
                column(width = 12,
                       bslib::card(
                         style = "height: 150px; max-height: 700px; overflow-y: scroll; overflow-x: hidden;",
                         shinyAce::aceEditor(
                           outputId = "table_vpc_params_code",
                           autoScrollEditorIntoView = TRUE,
                           minLines = 5,
                           maxLines = 50,
                           value = NULL,
                           readOnly = TRUE,
                           placeholder = "Click the generate button to output R code..."
                         )
                       ),
                       actionButton(inputId = "button_table_vpc_params_code", label = "Generate", icon = icon("code", lib = "font-awesome"))
                )
              )
            )
          ),
          
        # Results Panel ----
          bslib::nav_panel(
            # material_tab_content(
            title = "Results",
            value = "tab_results",
            
            fluidRow(style = "padding:15px;",
                     div(
                       id = "resultstabs", class = "ptab",
                       
                       bslib::navset_pill_list(
                         id = "result_subtabs",
                         fluid = FALSE,
                         widths = c(2, 10),
                         
                       # material_tabs(
                       #   tabs = c(
                       #     "Progress Text" = "tab_progress",
                       #     "Convergence Data"= "tab_test", #real time convergence data
                       #     "Output" = "tab_results_output",
                       #     "Convergence Plot" = "tab_convergence" #real time convergence plot data
                       #   ),
                       #   icon = c("announcement", "storage", "storage", "multiline_chart")
                       # ),
                         bslib::nav_panel(
                           title = "Progress Text",
                           value = "tab_progress",
                           shiny::verbatimTextOutput("job_status_text")
                         ),
                         bslib::nav_panel(
                           title = "Convergence Data",
                           value = "tab_test",
                           div(id = "table_convergence_data",
                               shiny::tableOutput("table_convergence")
                           )
                         ),
                         bslib::nav_panel(
                           title = "Convergence Plot",
                           value = "tab_convergence",
                           bslib::card(
                             full_screen = TRUE,
                             uiOutput("plot.ui")
                           )
                         ),
                         bslib::nav_panel(
                           title = "Output",
                           value = "tab_results_output",
                           div(id = "table_finished_results",
                               DT::DTOutput("table_results"),
                               shiny::verbatimTextOutput("stepwise_results")
                           )
                         )
                       )
                     ),
                     shiny::textOutput("pk_errors"),
                     tags$head(tags$style("#pk_errors{color: red;
                                      font-size: 20px;
                                      font-style: italic;
                                          }"
                               )
                     )
                )
            )
          )
        ),
      
      # Footer
      certara_footer(url = 'https://certara.github.io/R-RsNLME-model-executor/')
      
    )
    
  )
  return(ui)
}

create_id <- function () 
{
  paste(format(as.hexmode(sample(256, 8, replace = TRUE) - 
                            1), width = 2), collapse = "")
}

material_tabs <- function(tabs, color = NULL, icon = NULL) {
  material_tabs <- shiny::tagList()
  
  this_id <- paste0("tabs-id-", create_id())
  
  for (i in 1:length(tabs)) {
    material_tabs[[i]] <-
      shiny::tags$li(
        class = "tab",
        # shiny::tags$div(
        #   if (!is.null(icon[[i]]))
        #     tags$i(class = "material-icons", icon[[i]])
        # ),
        shiny::tags$a(
          class =
            paste0(
              ifelse(
                is.null(color),
                "",
                paste0(" ", color, "-text")
              )
            ),
          href = paste0("#", tabs[[i]]),
          tags$i(class = "material-icons", icon[[i]]),
          names(tabs)[[i]]
        )
      )
  }
  
  
  if (!is.null(color)) {
    tabs_style <-
      shiny::tagList(
        shiny::tags$head(
          shiny::tags$style(
            paste0(
              "
            #", this_id, " .indicator {
            position: absolute;
            bottom: 0;
            height: 2px;
            background-color: ", color, " !important;
            will-change: left, right;
            }
            #", this_id, " .tab a:focus, #", this_id, " .tab a:focus.active {
            background-color: ", paste0("rgba(", paste0(as.character(grDevices::col2rgb(color)[, 1]), collapse = ", "), ", 0.2)"), ";
            outline: none;
            }
            "
            )
          )
        )
      )
  } else {
    tabs_style <- shiny::tags$div()
  }
  
  shiny::tagList(
    shiny::tags$ul(
      id = this_id,
      class = "tabs tabs-fixed-width",
      material_tabs
    ),
    tabs_style
  )
}

