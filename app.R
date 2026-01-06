library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(stringr)

# -----------------------------
# API KEYS (from environment)
# -----------------------------
openai_api_key      <- Sys.getenv("OPENAI_API_KEY")
companies_house_key <- Sys.getenv("COMPANIES_HOUSE_API_KEY")

if (openai_api_key == "" || companies_house_key == "") {
  warning("Remember to set OPENAI_API_KEY and COMPANIES_HOUSE_API_KEY in .Renviron for user 'shiny'.")
}

# -----------------------------
# Local CRM CSV
# -----------------------------
calls_log_file <- "/tmp/epicontactdb.csv"

# -----------------------------
# Helper: null / empty coalesce
# -----------------------------
`%||%` <- function(a, b) {
  if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) a else b
}

# -----------------------------
# OpenAI helper
# -----------------------------
hey_chatGPT <- function(question) {
  if (openai_api_key == "") {
    return("OpenAI API key not configured. Please set OPENAI_API_KEY.")
  }

  body_content <- list(
    model = "gpt-4.1-mini",
    messages = list(
      list(
        role    = "system",
        content = "You are a concise business analyst. Summarise UK companies in clear, plain English."
      ),
      list(
        role    = "user",
        content = question
      )
    )
  )

  res <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", openai_api_key)),
    httr::content_type_json(),
    body   = jsonlite::toJSON(body_content, auto_unbox = TRUE),
    encode = "json"
  )

  if (httr::status_code(res) == 200) {
    parsed <- httr::content(res, as = "parsed")
    out    <- parsed$choices[[1]]$message$content
    return(str_trim(out))
  } else {
    msg <- httr::content(res, as = "text")
    warning("OpenAI error: ", msg)
    return("I couldn't generate a summary at the moment.")
  }
}

# -----------------------------
# Companies House helpers
# -----------------------------
search_companies <- function(search_input) {
  base_url       <- "https://api.company-information.service.gov.uk"
  endpoint       <- "/advanced-search/companies"
  all_results    <- list()
  items_per_page <- 100
  start_index    <- 0
  total_hits     <- Inf

  while (start_index < total_hits) {
    Sys.sleep(0.5)

    query_params <- list(
      start_index    = start_index,
      size           = items_per_page,
      company_status = "active"
    )

    if (grepl("^[0-9]+$", search_input)) {
      query_params$sic_codes <- search_input
    } else {
      query_params$company_name_includes <- search_input
    }

    res <- RETRY(
      "GET",
      url   = paste0(base_url, endpoint),
      query = query_params,
      httr::authenticate(companies_house_key, ""),
      accept_json(),
      times = 3
    )

    if (status_code(res) != 200) break

    json_content <- rawToChar(res$content)
    result       <- fromJSON(json_content, simplifyVector = FALSE)

    if (!is.null(result$items) && length(result$items) > 0) {
      Company_Data <- lapply(result$items, function(item) {
        data.frame(
          Company_Name       = ifelse(!is.null(item$company_name),     item$company_name,     NA),
          Company_Number     = ifelse(!is.null(item$company_number),   item$company_number,   NA),
          Incorporation_Date = ifelse(!is.null(item$date_of_creation), item$date_of_creation, NA),
          Status             = ifelse(!is.null(item$company_status),   item$company_status,   NA),
          stringsAsFactors   = FALSE
        )
      })
      all_results <- c(all_results, Company_Data)
      start_index <- start_index + items_per_page

      if (is.infinite(total_hits) && !is.null(result$total_results)) {
        total_hits <- result$total_results
      }
    } else {
      break
    }
  }

  if (length(all_results) == 0) {
    return(data.frame(
      Company_Name       = character(),
      Company_Number     = character(),
      Incorporation_Date = character(),
      stringsAsFactors   = FALSE
    ))
  }

  combined <- do.call(rbind, all_results)

  # keep only active, then DROP the Status column
  active <- subset(combined, Status == "active")
  active$Status <- NULL

  active
}

get_company_profile <- function(company_number) {
  url <- paste0("https://api.company-information.service.gov.uk/company/", company_number)
  res <- GET(
    url,
    authenticate(companies_house_key, ""),
    accept_json()
  )

  if (status_code(res) != 200) {
    warning("Profile API error status: ", status_code(res))
    return(NULL)
  }

  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (identical(txt, "") || is.null(txt)) return(NULL)

  profile <- jsonlite::fromJSON(txt)

  addr <- profile$registered_office_address
  address <- if (!is.null(addr)) {
    parts <- unlist(addr[c(
      "premises", "address_line_1", "address_line_2",
      "locality", "region", "postal_code", "country"
    )])
    paste(na.omit(parts), collapse = ", ")
  } else NA

  sic_codes <- if (!is.null(profile$sic_codes)) paste(profile$sic_codes, collapse = ", ") else NA
  postcode  <- if (!is.null(addr$postal_code)) addr$postal_code else NA

  list(
    raw          = profile,
    name         = profile$company_name,
    number       = profile$company_number,
    status       = profile$company_status,
    type         = profile$type,
    created      = profile$date_of_creation,
    jurisdiction = profile$jurisdiction,
    sic_codes    = sic_codes,
    address      = address,
    postcode     = postcode
  )
}

get_company_officers <- function(company_number, limit = 5) {
  url <- paste0("https://api.company-information.service.gov.uk/company/", company_number, "/officers")
  res <- GET(
    url,
    authenticate(companies_house_key, ""),
    accept_json()
  )

  if (status_code(res) != 200) {
    warning("Officers API error status: ", status_code(res), " for company ", company_number)
    return(NULL)
  }

  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (identical(txt, "") || is.null(txt)) return(NULL)

  officers <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  items    <- officers$items
  if (is.null(items) || length(items) == 0) return(NULL)

  n <- length(items)
  if (!is.null(limit)) n <- min(n, limit)

  rows <- lapply(seq_len(n), function(i) {
    it <- items[[i]]
    data.frame(
      Name      = if (!is.null(it$name))         it$name         else NA_character_,
      Role      = if (!is.null(it$officer_role)) it$officer_role else NA_character_,
      Appointed = if (!is.null(it$appointed_on)) it$appointed_on else NA_character_,
      Resigned  = if (!is.null(it$resigned_on))  it$resigned_on  else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');

      html, body {
        --dt-row-selected: 224, 255, 128;
        --dt-row-selected-text: 15, 27, 42;
        --dt-row-stripe: 0, 0, 0;
        --dt-row-hover: 224, 255, 128;
        height: 100%;
        overflow-x:hidden; /* kill any tiny horizontal overflow */
      }

      body {
        font-family:'Inter',sans-serif;
        background:#0f1b2a;
        color:#f5ffbd;
        margin:0;
        padding:0;
        min-height:100vh;
        display:flex;
        flex-direction:column;
        justify-content:center;  /* center hero on first load */
        align-items:center;
        position:relative;
        overflow-y:auto;
      }

      /* After a search, we add has-results to body via JS */
      body.has-results {
        justify-content:flex-start;  /* now behaves like normal top-down page */
      }

      .container {
        width:100%;
        max-width:1400px;
        padding:0 2rem;
        text-align:center;
        position:relative;
      }

      h1 {
        font-size:5.5rem;
        font-weight:800;
        color:#e0ff80;
        margin:2rem 0 3rem 0;
        text-shadow:0 0 40px rgba(224,255,128,0.6);
      }

      .input-card {
        background:rgba(15,27,42,0.9);
        backdrop-filter:blur(20px);
        border-radius:32px;
        padding:3.5rem 4rem;
        box-shadow:0 40px 100px rgba(0,0,0,0.8);
        width:100%;
        position:relative;
        margin-bottom:2rem;
      }

      .form-control {
        background:rgba(20,35,55,0.95)!important;
        border:none!important;
        border-radius:20px!important;
        color:#f5ffbd!important;
        padding:1.1rem 1.5rem!important;
        font-size:1.1rem;
        margin-bottom:1.1rem;
        box-shadow:inset 0 0 20px rgba(0,0,0,0.4);
        transition:all 0.4s ease;
      }

      .form-control::placeholder {color:rgba(224,255,128,0.6);}
      .form-control:focus {
        background:rgba(25,40,65,0.98)!important;
        box-shadow:
          inset 0 0 20px rgba(0,0,0,0.4),
          0 0 30px rgba(224,255,128,0.35)!important;
        outline:none;
      }

      .btn-primary {
        background:#e0ff80!important;
        color:#0f1b2a!important;
        border:none!important;
        border-radius:20px!important;
        padding:1.0rem 5rem!important;
        font-weight:700;
        font-size:1.1rem;
        box-shadow:0 0 80px rgba(224,255,128,0.7);
      }

      .results-row {
        display:flex;
        gap:1.8rem;
        margin-top:2rem;
        flex-wrap:wrap;
        text-align:left;
      }

      .epi-col-left {
        flex:1 1 55%;
        min-width:260px;
      }

      .epi-col-right {
        flex:1 1 40%;
        min-width:260px;
      }

      .epi-section-title {
        font-size:1.05rem;
        font-weight:600;
        color:#e0ff80;
        margin-bottom:0.1rem;
      }

      .epi-section-subtitle {
        font-size:0.85rem;
        color:rgba(224,255,128,0.7);
        margin-bottom:0.4rem;
      }

      .epi-panel {
        border-radius:20px;
        padding:0.8rem 1rem;
        background:rgba(10,18,30,0.95);
        border:1px solid rgba(224,255,128,0.15);
        box-shadow:inset 0 0 20px rgba(0,0,0,0.6);
      }

      .epi-panel table.dataTable {
        font-size:0.85rem;
      }

      .dataTables_wrapper {
        width:100%;
        overflow-x:auto;
      }

      table.dataTable {
        width:100% !important;
      }

      table.dataTable thead th {
        background-color:#0b1725;
        border-bottom:1px solid #111827;
        color:#f5ffbd;
        font-weight:600;
      }

      table.dataTable tbody td {
        color:#e5f6b0;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color:#e0ff80 !important;
        border:none !important;
        background:transparent !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        color:#0f1b2a !important;
        background:#e0ff80 !important;
        border-radius:999px;
        box-shadow:0 0 14px rgba(224,255,128,0.7);
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        color:#0f1b2a !important;
        background:rgba(224,255,128,0.8) !important;
        border-radius:999px;
      }

      table.dataTable tbody tr {
        background-color:#020617;
      }

      table.dataTable tbody tr:hover td {
        background:rgba(224,255,128,0.08) !important;
      }

      table.dataTable tr.selected td,
      table.dataTable td.selected {
        background-color:rgba(224,255,128,0.25) !important;
        color:#0f1b2a !important;
      }

      table.dataTable tbody tr.selected>*,
      table.dataTable tbody tr.dt-rowSelected>*,
      table.dataTable.display tbody tr.selected>*,
      table.dataTable.display tbody tr.selected>.sorting_1,
      table.dataTable.stripe>tbody>tr.odd.selected>* {
        box-shadow:inset 0 0 0 9999px rgba(224,255,128,0.25) !important;
        color:#0f1b2a !important;
      }

      table.dataTable.hover>tbody>tr:hover>*,
      table.dataTable.display>tbody>tr:hover>* {
        box-shadow: inset 0 0 0 9999px rgba(224, 255, 128, 0.10) !important;
        color:#f5ffbd !important;
      }

      table.dataTable.hover>tbody>tr.selected:hover>*,
      table.dataTable.display>tbody>tr.selected:hover>* {
        box-shadow: inset 0 0 0 9999px rgba(224, 255, 128, 0.35) !important;
        color:#0f1b2a !important;
      }

      .ch-link {
        color:#e0ff80 !important;
        font-size:0.8rem;
        text-decoration:none;
      }

      .ch-link:hover {
        color:#f9ffb0 !important;
        text-decoration:underline;
      }

      .company-header {
        display:flex;
        justify-content:space-between;
        align-items:flex-start;
        margin-bottom:10px;
      }

      .company-name {
        font-size:1.25rem;
        font-weight:650;
        color:#f5ffbd;
      }

      .company-number {
        font-size:0.8rem;
        color:rgba(224,255,128,0.7);
      }

      .chip {
        display:inline-block;
        padding:2px 10px;
        border-radius:999px;
        font-size:0.7rem;
        font-weight:500;
        margin-top:4px;
      }

      .chip-status {
        background:rgba(34,197,94,0.12);
        color:#bbf7d0;
        border:1px solid rgba(22,163,74,0.6);
      }

      .chip-sic {
        background:rgba(250,204,21,0.07);
        color:#facc15;
        border:1px solid rgba(234,179,8,0.5);
        margin-top:4px;
      }

      .meta-row {
        font-size:0.9rem;
        margin-bottom:3px;
        color:#f5ffbd;
      }

      .meta-label {
        font-weight:600;
        margin-right:4px;
        color:#e0ff80;
      }

      .officer-table {
        font-size:0.82rem;
        width:100%;
      }

      .officer-table th {
        font-weight:600;
        color:#f5ffbd;
        border-bottom:1px solid #111827;
      }

      .officer-table td {
        border-top:1px solid #020617;
        color:#e5f6b0;
      }

      .ai-summary {
        margin-top:8px;
        background:rgba(20,35,55,0.98);
        border-radius:16px;
        padding:12px 14px;
        border:1px solid rgba(224,255,128,0.5);
        box-shadow:0 0 35px rgba(224,255,128,0.35);
        font-size:0.98rem;
        color:#f5ffbd;
        text-align:left;
      }

      .ai-summary-title {
        font-weight:700;
        margin-bottom:4px;
        font-size:1rem;
        color:#e0ff80;
      }

      .link-pill {
        display:inline-block;
        padding:0.3rem 1.1rem;
        border-radius:999px;
        background:#e0ff80;
        color:#0f1b2a !important;
        font-size:0.8rem;
        font-weight:650;
        text-decoration:none !important;
        margin-right:0.5rem;
        box-shadow:0 0 18px rgba(224,255,128,0.7);
        transition:all 0.2s ease;
      }

      .link-pill:hover {
        background:#f9ffb0;
        box-shadow:0 0 26px rgba(224,255,128,0.9);
        transform:translateY(-1px);
      }

      .call-log-area {
        margin-top:8px;
      }

      .credit-container {
        margin-top:2rem;
        text-align:center;
        position:relative;
      }

      .credit {
        display:inline-block;
        font-size:1rem;
        color:rgba(224,255,128,0.6);
      }

      .shiny-input-container {width:100%;}

      .shiny-notification {
        position: fixed !important;
        bottom: 30px !important;
        top: auto !important;
        right: auto !important;
        left: 50% !important;
        transform: translateX(-50%) !important;
        min-width: 260px;
        max-width: 340px;
        background: rgba(8, 15, 26, 0.95);
        border-radius: 999px;
        padding: 8px 16px;
        box-shadow: 0 18px 50px rgba(0, 0, 0, 0.85);
        border: none;
        color: #f5ffbd;
        z-index: 9999;
      }

      .shiny-notification .progress {
        height: 4px;
        background: rgba(15, 27, 42, 0.9);
        border-radius: 999px;
        overflow: hidden;
        margin-bottom: 4px;
        box-shadow: none;
      }

      .shiny-notification .progress-bar {
        background: linear-gradient(90deg,#e0ff80,#f9ffb0,#e0ff80);
      }

      .shiny-notification .close {
        color: #e0ff80;
        opacity: 0.7;
      }

      .datepicker.dropdown-menu {
        background: #0f1b2a;
        border-radius: 16px;
        border: 1px solid rgba(224,255,128,0.4);
        box-shadow: 0 20px 50px rgba(0,0,0,0.8);
      }

      .datepicker table tr th,
      .datepicker table tr td {
        color: #e0ff80;
        background: transparent;
      }

      .datepicker table tr td.day:hover,
      .datepicker table tr td.focused {
        background: rgba(224,255,128,0.15);
        color: #f9ffb0;
      }

      .datepicker table tr td.active,
      .datepicker table tr td.active:hover {
        background: #e0ff80 !important;
        color: #0f1b2a !important;
        text-shadow: none !important;
      }

      .datepicker .prev span,
      .datepicker .next span {
        color: #e0ff80;
      }

      .datepicker .prev:hover span,
      .datepicker .next:hover span {
        color: #0f1b2a;
      }

      /* --------- RESPONSIVE TWEAKS --------- */
      @media (max-width: 992px) {
        h1 {
          font-size: 4rem;
          margin-bottom: 2rem;
        }
        .container {
          padding: 0 1.2rem;
        }
        .input-card {
          padding: 2.5rem 2rem;
        }
        .results-row {
          flex-direction: column;
        }
        .epi-col-left,
        .epi-col-right {
          flex: 1 1 100%;
          min-width: 100%;
        }
      }

      @media (max-width: 600px) {
        h1 {
          font-size: 3rem;
          margin:1.5rem 0 1.5rem 0;
        }
        .container {
          padding: 0 1rem;
        }
        .input-card {
          padding: 2rem 1.2rem;
          margin-bottom:1.5rem;
        }
        .form-control {
          font-size: 1rem;
          padding: 0.9rem 1.1rem !important;
        }
        .btn-primary {
          font-size: 1rem;
          padding: 0.9rem 1.2rem !important;
        }
        .company-header {
          flex-direction: column;
          gap: 0.4rem;
        }
        table.dataTable thead th,
        table.dataTable tbody td {
          white-space: normal !important;
          font-size: 0.78rem;
        }
      }
    ")),
    # Typewriter placeholder for search_input
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var examples = [
          'Enter SIC code or keyword',
          'Try 74909 for environmental consulting',
          'Search \"dairy\" to find dairy companies'
        ];
        var currentExample = 0;
        var currentChar = 0;
        var typingForward = true;
        var pauseCounter = 0;

        function typeWriter() {
          var $field = $('#search_input');

          if ($field.is(':focus') || $field.val().length > 0) {
            setTimeout(typeWriter, 150);
            return;
          }

          if (currentExample >= examples.length) currentExample = 0;
          var text = examples[currentExample];

          if (typingForward) {
            currentChar++;
            if (currentChar > text.length) {
              currentChar = text.length;
              pauseCounter++;
              if (pauseCounter > 10) {
                typingForward = false;
                pauseCounter = 0;
              }
            }
          } else {
            currentChar--;
            if (currentChar <= 0) {
              currentChar = 0;
              typingForward = true;
              currentExample = (currentExample + 1) % examples.length;
            }
          }

          var defaultPlaceholder = \"Enter SIC code or keyword (e.g. 74909 or 'dairy')\";
          var placeholderText = text.substring(0, currentChar);
          $field.attr('placeholder', placeholderText || defaultPlaceholder);

          setTimeout(typeWriter, 80);
        }

        typeWriter();
      });
    ")),
    # JS handler to toggle body layout after search
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleBodyLayout', function(message) {
        if (message.hasResults) {
          document.body.classList.add('has-results');
        } else {
          document.body.classList.remove('has-results');
        }
      });
    "))
  ),

  div(
    class = "container",
    h1("EPICONNECT"),
    div(
      class = "input-card",

      textInput(
        "search_input",
        label = NULL,
        placeholder = "",
        width = "100%"
      ),
      actionButton(
        "search_btn",
        "Search Companies",
        class = "btn-primary",
        width = "100%"
      ),

      conditionalPanel(
        condition = "output.show_panels == '1'",
        div(
          class = "results-row",
          div(
            class = "epi-col-left",
            div(
              class = "epi-section-title", "Search Results",
              tags$div(class = "epi-section-subtitle", textOutput("results_hint", inline = TRUE))
            ),
            div(class = "epi-panel", DTOutput("results_table"))
          ),
          div(
            class = "epi-col-right",
            div(
              class = "epi-section-title", "Company Details"
            ),
            div(class = "epi-panel", uiOutput("company_details_ui"))
          )
        )
      ),

      div(
        class = "credit-container",
        span(class = "credit", "Developed by igarner@antlerbio.com")
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {

  companies_data   <- reactiveVal(NULL)
  selected_profile <- reactiveVal(NULL)
  selected_officers<- reactiveVal(NULL)
  selected_summary <- reactiveVal(NULL)

  calls_data <- reactiveVal({
    if (file.exists(calls_log_file)) {
      df <- suppressWarnings(
        tryCatch(
          read.csv(calls_log_file, stringsAsFactors = FALSE),
          error = function(e) data.frame()
        )
      )

      if (!"Name"            %in% names(df)) df$Name            <- character(nrow(df))
      if (!"DateLastContact" %in% names(df)) df$DateLastContact <- character(nrow(df))
      if (!"Comments"        %in% names(df)) df$Comments        <- character(nrow(df))

      n <- nrow(df)
      if (!"company_number" %in% names(df)) df$company_number <- rep(NA_character_, n)
      if (!"company_name"   %in% names(df)) df$company_name   <- rep(NA_character_, n)
      if (!"SalesmoonName"  %in% names(df)) {
        df$SalesmoonName <- if ("Name" %in% names(df)) df$Name else rep(NA_character_, n)
      }

      df
    } else {
      data.frame(
        Name            = character(),
        DateLastContact = character(),
        Comments        = character(),
        company_number  = character(),
        company_name    = character(),
        SalesmoonName   = character(),
        stringsAsFactors = FALSE
      )
    }
  })

  observeEvent(input$search_btn, {
    req(input$search_input)

    withProgress(message = "Searching companies...", value = 0, {
      results <- search_companies(input$search_input)
      companies_data(results)
      selected_profile(NULL)
      selected_officers(NULL)
      selected_summary(NULL)

      # Once a search has been run, switch layout to top-down
      session$sendCustomMessage("toggleBodyLayout", list(hasResults = TRUE))
    })
  })

  output$results_hint <- renderText({
    dat <- companies_data()
    if (is.null(dat)) return("No search run yet.")
    if (nrow(dat) == 0) return("No active companies found.")
    paste(nrow(dat), "active companies found.")
  })

  output$show_panels <- renderText({
    dat <- companies_data()
    if (!is.null(dat) && nrow(dat) > 0) "1" else "0"
  })
  outputOptions(output, "show_panels", suspendWhenHidden = FALSE)

  output$results_table <- renderDT({
    dat <- companies_data()
    req(dat)

    datatable(
      dat,
      selection = "single",
      escape    = TRUE,  # no HTML column any more
      rownames  = FALSE,
      options   = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "tip"
      )
    )
  })

  observeEvent(input$results_table_rows_selected, {
    idx <- input$results_table_rows_selected
    dat <- companies_data()
    req(length(idx) == 1, !is.null(dat))

    row            <- dat[idx, ]
    company_number <- row$Company_Number
    company_name   <- row$Company_Name

    withProgress(message = paste("Fetching details for", company_name, "..."), value = 0, {
      incProgress(0.3, detail = "Company profile")
      profile  <- get_company_profile(company_number)

      if (is.null(profile)) {
        selected_profile(NULL)
        selected_officers(NULL)
        selected_summary("No profile data returned by Companies House.")
        return()
      }

      selected_profile(profile)

      incProgress(0.3, detail = "Company officers")
      officers <- get_company_officers(company_number)
      selected_officers(officers)

      incProgress(0.3, detail = "Summary")
      sic_text <- profile$sic_codes %||% "no SIC codes available"

      officers_text <- if (!is.null(officers) && nrow(officers) > 0) {
        paste(
          head(paste0(officers$Name, " (", officers$Role, ")"), 5),
          collapse = ", "
        )
      } else {
        "no officer information available"
      }

      prompt <- paste0(
        "Provide a concise, non-technical summary of the UK company '", company_name,
        "' (company number ", company_number, "). ",
        "Based on its SIC codes (", sic_text, ") ",
        "and the following key officers: ", officers_text, ". ",
        "Focus on what the company likely does and any risk-relevant points. ",
        "Limit to about 120 words."
      )
      summary <- hey_chatGPT(prompt)
      selected_summary(summary)
    })
  })

  observeEvent(input$save_contact, {
    profile <- selected_profile()
    req(profile)

    salesmoon_name <- input$salesmoon_name %||% ""
    contact_date   <- input$contact_date
    comments       <- input$contact_comments %||% ""

    req(nzchar(salesmoon_name), !is.null(contact_date))

    df <- calls_data()

    new_row <- data.frame(
      Name            = salesmoon_name,
      DateLastContact = as.character(contact_date),
      Comments        = comments,
      company_number  = profile$number,
      company_name    = profile$name,
      SalesmoonName   = salesmoon_name,
      stringsAsFactors = FALSE
    )

    missing_cols <- setdiff(names(df), names(new_row))
    for (col in missing_cols) {
      new_row[[col]] <- NA
    }
    new_row <- new_row[names(df)]

    updated <- rbind(df, new_row)
    write.csv(updated, calls_log_file, row.names = FALSE)
    calls_data(updated)

    showNotification("Contact saved for this company.", type = "message", duration = 3)
  })

  output$company_details_ui <- renderUI({
    profile  <- selected_profile()
    officers <- selected_officers()
    summary  <- selected_summary()
    calls    <- calls_data()

    if (is.null(profile)) {
      return(
        div(
          style = "color:rgba(224,255,128,0.7); font-size:0.9rem;",
          "Run a search and click on a company row to see details here."
        )
      )
    }

    last_call_text <- "No contacts logged yet."
    if (!is.null(calls) && nrow(calls) > 0 && "company_number" %in% names(calls)) {
      sub <- calls[calls$company_number == profile$number, , drop = FALSE]
      if (nrow(sub) > 0) {
        if ("DateLastContact" %in% names(sub)) {
          sub$._dlc <- suppressWarnings(as.Date(sub$DateLastContact))
          sub <- sub[order(sub$._dlc, decreasing = TRUE, na.last = TRUE), ]
        }
        lc <- sub[1, ]
        nm <- lc$SalesmoonName %||% lc$Name %||% "Unknown"
        dt <- lc$DateLastContact %||% "unknown date"
        cm <- lc$Comments %||% ""
        if (nzchar(cm)) {
          last_call_text <- paste0("Salesmoon ", nm, " on ", dt, " — Commoonts: ", cm)
        } else {
          last_call_text <- paste0("Salesmoon ", nm, " on ", dt)
        }
      }
    }

    ch_url <- paste0(
      "https://find-and-update.company-information.service.gov.uk/company/",
      profile$number
    )

    pc        <- profile$postcode %||% ""
    query_str <- paste(profile$name, pc, "phone")

    google_url <- paste0(
      "https://www.google.com/search?q=",
      URLencode(query_str, reserved = TRUE)
    )

    yell_url <- paste0(
      "https://www.yell.com/ucs/UcsSearchAction.do?keywords=",
      URLencode(profile$name, reserved = TRUE),
      "&location=",
      URLencode(pc, reserved = TRUE)
    )

    tagList(
      div(
        class = "company-header",
        div(
          div(class = "company-name", profile$name),
          div(class = "company-number", paste("Company no.", profile$number)),
          br(),
          span(class = "chip chip-status", toupper(profile$status %||% ""))
        ),
        div(
          a(
            href   = ch_url,
            target = "_blank",
            class  = "ch-link",
            icon("external-link"), " Companies House"
          )
        )
      ),

      if (!is.na(profile$sic_codes) && nzchar(profile$sic_codes))
        div(
          span(class = "chip chip-sic", paste("SIC:", profile$sic_codes))
        ),

      br(),

      div(
        class = "meta-row",
        span(class = "meta-label", "Incorporated:"),
        span(profile$created %||% "Unknown")
      ),
      div(
        class = "meta-row",
        span(class = "meta-label", "Type:"),
        span(profile$type %||% "Unknown")
      ),
      div(
        class = "meta-row",
        span(class = "meta-label", "Jurisdiction:"),
        span(profile$jurisdiction %||% "Unknown")
      ),
      div(
        class = "meta-row",
        span(class = "meta-label", "SIC:"),
        span(profile$sic_codes %||% "Unknown")
      ),

      if (!is.na(profile$address) && nzchar(profile$address))
        div(
          class = "meta-row",
          span(class = "meta-label", "Registered office:"),
          span(profile$address)
        ),

      div(
        class = "meta-row",
        span(class = "meta-label", "Find phone:"),
        a(href = google_url, target = "_blank", class = "link-pill", "Google"),
        a(href = yell_url,   target = "_blank", class = "link-pill", "Yell")
      ),

      div(
        class = "meta-row",
        span(class = "meta-label", "Calls:"),
        span(last_call_text)
      ),

      div(
        class = "call-log-area",
        textInput("salesmoon_name", NULL, placeholder = "Salesmoon name", width = "100%"),
        dateInput("contact_date", NULL, value = Sys.Date(), width = "100%"),
        textAreaInput("contact_comments", NULL, placeholder = "Commoonts", rows = 2, width = "100%"),
        actionButton("save_contact", "Save contact", class = "btn-primary", width = "100%")
      ),

      tags$hr(style = "border-color:#111827; margin:8px 0;"),
      h5(style = "font-size:0.9rem; margin-top:0; color:#e0ff80;", "Key officers"),
      if (is.null(officers)) {
        div(style = "font-size:0.82rem; color:rgba(224,255,128,0.8);", "No officer data available.")
      } else {
        tags$table(
          class = "officer-table table table-sm",
          tags$thead(
            tags$tr(
              tags$th("Name"),
              tags$th("Role"),
              tags$th("Appointed"),
              tags$th("Resigned")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(officers)), function(i) {
              tags$tr(
                tags$td(officers$Name[i]),
                tags$td(officers$Role[i]),
                tags$td(officers$Appointed[i]),
                tags$td(officers$Resigned[i])
              )
            })
          )
        )
      },
      tags$hr(style = "border-color:#111827; margin:8px 0;"),
      div(
        class = "ai-summary",
        div(class = "ai-summary-title", "Summary"),
        div(style = "font-size:0.98rem;", summary %||% "No summary available.")
      )
    )
  })
}

shinyApp(ui = ui, server = server)
