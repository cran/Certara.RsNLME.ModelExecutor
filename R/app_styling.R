styleCSS <-
  ".certara-page {
    display: flex;
    flex-direction: column;
    min-height: 100vh;
  }
  
  body, .container-fluid {
    background-color: #616161;
    margin: 0;
    padding: 0;
  }
  
  .main-content {
    padding: 1rem;
    flex: 1;
  }

  .certara-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0px 20px;
    background-color: #d3d3d3;
    border-bottom: 2px solid #CC0000;
  }

  .logo-title {
    display: flex;
    align-items: center;
  }

  .logo-title img {
    margin-right: 10px;
  }

  .certara-footer {
    display: flex;
    justify-content: space-between;
    align-items: center;
    left: 0;
    bottom: 0;
    width: 100%;
    height: 35px;
    background-color: #d3d3d3;
    border-top: 1px solid #CC0000;
    color: #000;
    text-align: left;
    z-index: 12;
  }

  .nav-item {
    flex: 1;
    text-align: center;
  }
  
  .card .well {
    height: fit-content;
  }
  
  #table_convergence_data {
    width: 100%;
    overflow: auto;
  }
  
  .shiny-panel-conditional .col-sm-1,
  .shiny-panel-conditional .col-sm-2,
  .shiny-panel-conditional .col-sm-3 {
    padding: inherit;
  }
  
  .multi-input-with-checkbox {
    display: flex;
    align-items: baseline;
  }

  .multi-input-with-checkbox .col-sm-1,
  .multi-input-with-checkbox .col-sm-2,
  .multi-input-with-checkbox .col-sm-3 {
    align-self: flex-end;
  }

  .multi-input-with-checkbox .col-checkbox {
    align-self: flex-end;
    margin-bottom: 5px;
  }
  
  .options-row {
    padding-left: 0.5rem; 
    min-height: 4.75rem;
    align-items: baseline;
  }
  
  .options-row .form-group {
    margin-bottom: 0 !important;
  }
  
  .options-row .col-checkbox {
    padding-bottom: 0.5rem;
  }
  
  .shiny-input-container label {
    color: #9e9e9e;
    font-size: 0.7rem;
    line-height: 9%;
    margin: 0.366667rem 0 -0.54rem 0;
  }

  .shiny-input-container .checkbox label {
    color: initial;
    font-size: initial;
    line-height: initial;
    margin: initial;
  }
  
  .large-labels .control-label {
    line-height: initial;
  }
  
  .execution_inputs,
  .execution_inputs .card-body {
    overflow: visible;
  }
    
  .far.fa-circle-play {
    font-size: 2rem;
    padding-top: 1.7rem;
  }
  
  .ex_status_idle {
    color: green;
  }

  .ex_status_busy {
    color: red;
  }
  
  .sort-columns {
    overflow: visible;
  }
  
  .save_btn {
    color: black;
    text-decoration: none;
    cursor: pointer;
  }

  .save_btn:hover {
    color: black;
    text-decoration: none;
  }

  .btn {
    color: #fff;
    background-color: #1d7eba;
    text-decoration: none;
    text-align: center;
    letter-spacing: .5px;
    -webkit-transition: background-color .2s ease-out;
    transition: background-color .2s ease-out;
    cursor: pointer;
  }

  .btn:hover {
    background-color: #008CBA;
    color: #fff;
  }
  
  #button_stop_execution,
  #exitCancel {
    background-color: grey;
  }
  
  #button_stop_execution:hover,
  #exitCancel:hover {
    background-color: darkgrey; /* Change this color to the desired hover color */
  }
  
  th, td {
    padding-left: 0.2rem;
    padding-right: 0.2rem;
  }
  
  .modal-div {
    overflow-wrap: break-word;
  }
  
  .selectize-control {
    margin-bottom: 0;
  }
  
"


certara_header <- function(header_title) {
  div(class = "certara-header",
      div(class = "logo-title",
          tags$a(
            href = "https://www.certara.com",
            target = "_blank",
            class = "brand-logo",
            tags$img(src = "https://cdn.shortpixel.ai/spai/w_133+q_lossless+ret_img+to_webp/https://www.certara.com/app/uploads/2023/05/certara-logo-2023.png")
          ),
          h4(class = "header_title", header_title, style = "margin-top: 20px; font-family: Segoe UI !important")
      ),
      shiny::actionLink(inputId = "exitShiny",
                        label = "Save & Exit",
                        icon = icon("save"),
                        class = "save_btn"
      )
  )
}

certara_footer <- function(url) {
  div(class = "certara-footer",
      tags$p(style = 'margin: 0; padding-right: 5px; font-size:small',
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://www.certara.com/', target = '_blank', 'Home'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = url, target = '_blank', 'Help'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://certara.service-now.com/csm', target = '_blank', 'Support'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://www.certara.com/legal/privacy-center/', target = '_blank', 'Privacy Policy')
      ),
      tags$p(style = 'margin: 0; padding-right: 5px; font-size:small; text-align: right;',
             HTML("&#169; 2011-2024 Certara USA, Inc., All rights reserved. Version: 3.0.1")
      )
  )
}
