library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(DT)

suppressMessages(source("cleaning.r"))

# --- Theme ---
hcc_theme <- bs_theme(
    version = 5,
    primary = "#2A5239",
    success = "#799B3E",
    secondary = "#63666A",
    bg = "#FFFFFF",
    fg = "#1a1a1a",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro", wght = c(600, 700))
) |>
    bs_add_rules("
        .sidebar { border-right: 3px solid #2A5239; }
        .bslib-value-box { border-radius: 0.5rem; }
    ")

all_majors   <- sort(unique(na.omit(df$major_area)))
all_programs <- sort(unique(na.omit(df$program_code)))
all_years    <- sort(unique(na.omit(df$academic_year)), decreasing = TRUE)
all_terms    <- sort(unique(na.omit(df$term_id)), decreasing = TRUE)

# --- UI ---
ui <- page_sidebar(
    title = tags$span(
        tags$img(src = "HCC Logo (Green).png", alt = "HCC", height = "40px"),
        " Stop-Out Dashboard"
    ),
    theme = hcc_theme,
    class = "bslib-page-dashboard",
    sidebar = sidebar(
        title = "Filters",
        width = 290,
        bg = "#f4f7f4",
        actionButton(
            "clear_btn", "Clear All Filters",
            icon  = icon("xmark"),
            class = "btn-outline-secondary w-100 mb-3"
        ),
        accordion(
            open = TRUE,
            accordion_panel(
                "Program",
                icon = bs_icon("book"),
                selectizeInput(
                    "major_filter", "Major Area",
                    choices  = all_majors,
                    multiple = TRUE,
                    options  = list(placeholder = "All Major Areas")
                ),
                selectizeInput(
                    "program_filter", "Program Code",
                    choices  = all_programs,
                    multiple = TRUE,
                    options  = list(placeholder = "All Programs")
                )
            ),
            accordion_panel(
                "Term / Academic Year",
                icon = bs_icon("calendar3"),
                selectizeInput(
                    "year_filter", "Academic Year",
                    choices  = all_years,
                    multiple = TRUE,
                    options  = list(placeholder = "All Academic Years")
                ),
                selectizeInput(
                    "term_filter", "Term ID",
                    choices  = all_terms,
                    multiple = TRUE,
                    options  = list(placeholder = "All Terms")
                )
            ),
            accordion_panel(
                "Stop Out Types",
                icon = bs_icon("signpost-split"),
                selectInput(
                    "graduated_filter",
                    "Graduated (Program Level)",
                    choices = c("All" = "", "Yes" = "TRUE", "No" = "FALSE")
                ),
                selectInput(
                    "grad_any_program_filter",
                    "Graduated from Any Program",
                    choices = c("All" = "", "Yes" = "TRUE", "No" = "FALSE")
                ),
                selectInput(
                    "grad_any_major_filter",
                    "Graduated from Any Major",
                    choices = c("All" = "", "Yes" = "TRUE", "No" = "FALSE")
                ),
                selectInput(
                    "grad_all_major_filter",
                    "Graduated from All Majors",
                    choices = c("All" = "", "Yes" = "TRUE", "No" = "FALSE")
                )
            ),
            accordion_panel(
                "Student Type",
                icon = bs_icon("person-badge"),
                input_switch("ccp_filter",     "Remove CCP Students",   value = FALSE),
                input_switch("hs_filter",      "Remove HS Students",    value = FALSE),
                input_switch("primary_filter", "Primary Programs Only", value = FALSE)
            )
        ),
        tags$hr(),
        downloadButton("download_btn", "Download CSV", class = "btn-success w-100")
    ),
    layout_column_wrap(
        width = 1 / 4,
        fill = FALSE,
        value_box(
            title    = "Stop-Out Records",
            value    = textOutput("vb_total", inline = TRUE),
            showcase = bs_icon("person-x-fill"),
            theme    = "primary"
        ),
        value_box(
            title    = "Unique Students",
            value    = textOutput("vb_unique", inline = TRUE),
            showcase = bs_icon("people-fill"),
            theme    = "secondary"
        ),
        value_box(
            title    = "CCP Students",
            value    = textOutput("vb_ccp", inline = TRUE),
            showcase = bs_icon("mortarboard-fill"),
            theme    = "success"
        ),
        value_box(
            title    = "High School Students",
            value    = textOutput("vb_hs", inline = TRUE),
            showcase = bs_icon("building"),
            theme    = value_box_theme(bg = "#4472a0", fg = "#ffffff")
        )
    ),
    card(
        full_screen = TRUE,
        card_header("Stop-Out Student Records"),
        DTOutput("student_table")
    )
)

# --- Server ---
server <- function(input, output, session) {

    # Cascade: academic year → term IDs
    observeEvent(input$year_filter, {
        if (length(input$year_filter) > 0) {
            terms_in_year <- df |>
                filter(academic_year %in% input$year_filter) |>
                pull(term_id) |>
                na.omit() |>
                unique() |>
                sort(decreasing = TRUE)
        } else {
            terms_in_year <- all_terms
        }
        updateSelectizeInput(session, "term_filter",
            choices  = terms_in_year,
            selected = intersect(input$term_filter, terms_in_year)
        )
    }, ignoreNULL = FALSE)

    # Cascade: major area → program codes
    observeEvent(input$major_filter, {
        if (length(input$major_filter) > 0) {
            codes <- df |>
                filter(major_area %in% input$major_filter) |>
                pull(program_code) |>
                na.omit() |>
                unique() |>
                sort()
        } else {
            codes <- all_programs
        }
        updateSelectizeInput(session, "program_filter",
            choices  = codes,
            selected = intersect(input$program_filter, codes)
        )
    }, ignoreNULL = FALSE)

    # Clear all filters
    observeEvent(input$clear_btn, {
        updateSelectizeInput(session, "major_filter",   choices = all_majors,   selected = character(0))
        updateSelectizeInput(session, "program_filter", choices = all_programs, selected = character(0))
        updateSelectizeInput(session, "year_filter",    choices = all_years,    selected = character(0))
        updateSelectizeInput(session, "term_filter",    choices = all_terms,    selected = character(0))
        updateSelectInput(session, "graduated_filter",         selected = "")
        updateSelectInput(session, "grad_any_program_filter", selected = "")
        updateSelectInput(session, "grad_any_major_filter",   selected = "")
        updateSelectInput(session, "grad_all_major_filter",   selected = "")
        updateSwitchInput(session, "ccp_filter",     value = FALSE)
        updateSwitchInput(session, "hs_filter",      value = FALSE)
        updateSwitchInput(session, "primary_filter", value = FALSE)
    })

    filtered <- reactive({
        d <- df

        if (length(input$major_filter) > 0) {
            d <- filter(d, major_area %in% input$major_filter)
        }
        if (length(input$program_filter) > 0) {
            d <- filter(d, program_code %in% input$program_filter)
        }
        if (length(input$year_filter) > 0) {
            d <- filter(d, academic_year %in% input$year_filter)
        }
        if (length(input$term_filter) > 0) {
            d <- filter(d, term_id %in% input$term_filter)
        }
        if (nzchar(input$graduated_filter)) {
            val <- as.logical(input$graduated_filter)
            d <- filter(d, graduated == val)
        }
        if (nzchar(input$grad_any_program_filter)) {
            val <- as.logical(input$grad_any_program_filter)
            d <- filter(d, graduated_from_any_program == val)
        }
        if (nzchar(input$grad_any_major_filter)) {
            val <- as.logical(input$grad_any_major_filter)
            d <- filter(d, graduated_from_any_major == val)
        }
        if (nzchar(input$grad_all_major_filter)) {
            val <- as.logical(input$grad_all_major_filter)
            d <- filter(d, graduated_from_all_major == val)
        }
        if (isTRUE(input$ccp_filter)) {
            d <- filter(d, ever_ccp == FALSE)
        }
        if (isTRUE(input$hs_filter)) {
            d <- filter(d, ever_high_school == FALSE)
        }
        if (isTRUE(input$primary_filter)) {
            d <- filter(d, ever_primary == TRUE)
        }

        d
    })

    # Value boxes
    output$vb_total  <- renderText(format(nrow(filtered()), big.mark = ","))

    output$vb_unique <- renderText(
        format(n_distinct(filtered()$stc_person_id), big.mark = ",")
    )

    output$vb_ccp <- renderText({
        n <- filtered() |>
            filter(ever_ccp == TRUE) |>
            distinct(stc_person_id) |>
            nrow()
        format(n, big.mark = ",")
    })

    output$vb_hs <- renderText({
        n <- filtered() |>
            filter(ever_high_school == TRUE) |>
            distinct(stc_person_id) |>
            nrow()
        format(n, big.mark = ",")
    })

    # Table
    table_data <- reactive({
        filtered() |>
            arrange(desc(academic_year), term_id) |>
            select(
                `Student ID`           = stc_person_id,
                `Program Code`         = program_code,
                `Major Area`           = major_area,
                `Term ID`              = term_id,
                `Academic Year`        = academic_year,
                `Graduated`            = graduated,
                `Primary Program`      = ever_primary,
                `CCP Student`          = ever_ccp,
                `High School`          = ever_high_school,
                `Grad Any Program`     = graduated_from_any_program,
                `Grad Any Major`       = graduated_from_any_major,
                `Grad All Majors`      = graduated_from_all_major
            )
    })

    output$student_table <- renderDT({
        datatable(
            table_data(),
            rownames  = FALSE,
            selection = "none",
            options   = list(
                pageLength = 25,
                scrollX    = TRUE,
                dom        = "lftip",
                columnDefs = list(
                    list(targets = c(5:11), className = "dt-center")
                )
            )
        ) |>
            formatStyle(
                "CCP Student",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#e8f4e8", "transparent"))
            ) |>
            formatStyle(
                "High School",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#fff9e6", "transparent"))
            ) |>
            formatStyle(
                "Primary Program",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#e8eef8", "transparent"))
            ) |>
            formatStyle(
                "Grad Any Program",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#fde8e8", "transparent"))
            ) |>
            formatStyle(
                "Grad Any Major",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#fde8e8", "transparent"))
            ) |>
            formatStyle(
                "Grad All Majors",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#fde8e8", "transparent"))
            )
    })

    # Download
    output$download_btn <- downloadHandler(
        filename = function() paste0("stop_out_report_", Sys.Date(), ".csv"),
        content  = function(file) {
            readr::write_csv(
                filtered() |>
                    left_join(student_pii, by = c("stc_person_id" = "campus_id")),
                file
            )
        }
    )
}

shinyApp(ui, server)
