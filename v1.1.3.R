# Welsh vs English Pay Comparator
# England 2016, Wales 2002, Wales 2026 (proposed)
# uses pay after 2025 DDRB uplift, and the 2025b figures for the proposed 2026 contract

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(shinyjs)

# Data	  
pay_scales <- data.frame(
  grade = c("FY1", "FY2", "ST1", "ST2", "ST3", "ST4", "ST5", "ST6", "ST7", "ST8"),
  base_salary = c(38831, 44439, 52656, 52656, 65048, 65048, 65048, 73992, 73992, 73992)
)

Wales_pay_scales <- data.frame(
  grade_wales = c("FY1","FY1","FY1","FY2","FY2","FY2","SpR","SpR","SpR","SpR","SpR","SpR","SpR","SpR","SpR","SpR"),
  wales_pay_increment = c("0","1","2","0","1","2","0","1","2","3","4","5","6","7","8","9"),
  wales_2002_2025 = c(35390,37487,39587,43466,46192,48915,46324,49046,52853,55157,57929,60706,63482,66259,69034,71814),
  wales_2026 = c(40000,40000,40000,50000,50000,50000,55000,55000,62000,62000,68000,68000,74000,74000,78000,78000)
)

Banding <- data.frame(
  pay_band_mult_name = c("unbanded","1C","1B","1A","2B","2A","3","GP"),
  pay_band_mult = c(1,1.2,1.4,1.5,1.5,1.8,2,1.45)
)

pay_premia <- data.frame(
  type = c("Academia", "GP", "Psych Core", "Psych HST (3 years)", "Psych HST (4 years)", "Histopathology", 
           "OMFS (3 years)", "OMFS (4 years)", "OMFS (5 years)", 
           "OMFS (6 years)", "OMFS (7 years)", "OMFS (8 years)",
           "EM (3 years)", "EM (4 years)", "EM (5 years)", 
           "EM(6 years)", "EM (7 years)", "EM (8 years)"),
  f_premia = c(5424, 11118, 4520, 4520, 3390, 5424, 9040, 6780, 5424, 4520, 3875, 3390, 9040, 6780, 5424, 4520, 3875, 3390)
)

# Constants
# Income Tax
PERSONAL_ALLOWANCE <- 12570
TAPER_START <- 100000
TAPER_END <- 125140

# Eng/Wales/NI Rates
BASIC_RATE <- 0.2
HIGHER_RATE <- 0.4
ADDITIONAL_RATE <- 0.45

# Eng/Wales/NI Thresh
INCOME_THRESH <- 12570
INCOME_BASIC_THRESH <- 50270
INCOME_HIGHER_THRESH <- 125140

# Eng/Wales/NI Bands
BASIC_BAND <- INCOME_BASIC_THRESH - INCOME_THRESH #37700
HIGHER_BAND <- INCOME_HIGHER_THRESH - INCOME_BASIC_THRESH #74870

# NI
NI_THRESHOLD <- 12570
NI_BASIC_LIMIT <- 50270
NI_RATE1 <- 0.08
NI_RATE2 <- 0.02

# Student Loans
P1_THRESH <- 26065
P2_THRESH <- 28470
P4_THRESH <- 32745  # Scotland
P5_THRESH <- 25000
PG_THRESH <- 21000

# UI	
ui <- bslib::page_fluid(
  useShinyjs(),
  
  titlePanel("Welsh 2002 vs Welsh 2026 vs English 2016 Pay comparator v1.1.3"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Grade Select
      h6(strong("What grade are you?")),
      radioButtons("grade", label = NULL, choices = pay_scales$grade),
      
      # GPST check
      conditionalPanel(
        condition = "input.grade != 'FY1' && input.grade != 'FY2'",
        checkboxInput("gp_band", "If you are a GPST, are you currently on a GP placement?", value = FALSE)
      ),
      
      # Welsh incremental pay selector
      h6(strong("Welsh Incremental Credit")),
      radioButtons("pay_wales", "How much incremental credit do you have?",
                   choices = c("0/min", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                   selected = "0/min",
                   inline = TRUE
      ),
      
      # Banding Options (Wales 2002 only)
      h6(strong("Banding Options (Wales 2002)")),
      p("Banding only applies to the Welsh 2002 contract.", 
        style = "font-size: 0.9em; color: #6c757d; margin-bottom: 8px;"),

      conditionalPanel(
        condition = "input.gp_band == false",
        checkboxInput("know_band", "Do you know your pay band? If not we'll assume you are unbanded", value = TRUE),
        conditionalPanel(
          condition = "input.know_band == true",
          radioButtons("pay_band", "What's your band?",
                       choices = c("unbanded", "1C", "1B", "1A", "2B", "2A", "3"),
                       selected = "unbanded",
                       inline = TRUE
          )				
        )
      ),
      
      # hours	  
      h6(strong("What would your rota look like on the proposed 2026 Welsh contract or 2016 English contract?")),
      tagList(
        numericInput("non_enhanced_hours", "Average total weekly hours", value = 48, min = 0, max = 72, step = 0.25),
        numericInput("enhanced_hours_wales", 
                     label = HTML("<strong>(Wales 2026)</strong> How many of those hours are enhanced/OOH? - plain time is 7am-7pm in Wales"), 
                     value = 8, min = 0, max = 56, step = 0.25),
        numericInput("enhanced_hours_eng", 
                     label = HTML("<strong>(England 2016)</strong> How many of those hours are enhanced/OOH? - plain time is 7am-9pm in England"), 
                     value = 8, min = 0, max = 56, step = 0.25),
        checkboxInput("weekend_work", 
                      label = HTML("<strong>(England 2016)</strong> Do you work weekends?"), 
                      value = TRUE),
        conditionalPanel(
          condition = "input.weekend_work == true",
          numericInput("weekend_freq", 
                       label = HTML("<strong>(England 2016)</strong> If you work weekends, what is your average frequency? (e.g. 1 in 6 = enter 6). This can be a decimal number."), 
                       value = 6, min = 1)
        ),
        checkboxInput("is_on_call_allowance", 
                      label = HTML("<strong>(England 2016)</strong> Do you get an on-call allowance?"), 
                      value = FALSE)
      ),
      
      uiOutput("premia_selector"),
      
      h6(strong("London Weighting (England 2016)")),
      checkboxInput("is_London_weight", "Do you get London Weighting?", value = FALSE),
      conditionalPanel(
        condition = "input.is_London_weight == true",
        selectInput("london_weighting", "London Weighting Band:",
                    choices = c("Fringe", "Outer", "Inner"),
                    selected = "Inner")
      ),
      
      
      
      h6(strong("Pension Options")),
      checkboxInput("is_under_StatePensionAge", "Are you under state pension age?", value = TRUE),
      conditionalPanel(
        condition = "input.is_under_StatePensionAge == true",
        checkboxInput("is_opt_in_pension", "Are you opted in to the pension?", value = TRUE)
      ),
      
      h6(strong("Select Student Loan Plans You Repay")),
      checkboxGroupInput("plans", label = NULL,
                         choices = list(
                           "Plan 1 (pre-2012)" = "plan1",
                           "Plan 2 (post-2012)" = "plan2",
                           "Plan 4 (Scotland)" = "plan4",
                           "Plan 5 (England, 2023+)" = "plan5",
                           "Postgraduate Loan" = "pgloan"
                         )
      ),
      
      
      actionButton("calculate", "Calculate Pay Differences"),
      
      
      h6(strong("Comparison Options")),
      radioButtons("comparison_type", "Compare:",
                   choices = c("Net Pay" = "net", "Gross Pay" = "gross"),
                   selected = "gross",
                   inline = TRUE)
    ),
    
    mainPanel(
      div(id = "results_section",
          uiOutput("salary_output"),
          uiOutput("pension_warning"),
          uiOutput("comparison_output"),
          plotOutput("barplot_output"),
          uiOutput("feedback_message")
      )
    )
  )
)

server <- function(input, output, session) {
  
  vals <- reactiveValues(
    salary_html = NULL,
    calculated = FALSE
  )
  
  observe({
    total_hours <- input$non_enhanced_hours
    if (!is.null(total_hours) && !is.na(total_hours) && total_hours >= 0) {
      # Update both enhanced hour inputs
      for(input_name in c("enhanced_hours_wales", "enhanced_hours_eng")) {
        current_val <- input[[input_name]]
        if (!is.null(current_val) && !is.na(current_val)) {
          updateNumericInput(session, input_name, 
                             max = total_hours, 
                             value = min(current_val, total_hours))
        }
      }
    }
  })
  
  # Flexible premia selector
  output$premia_selector <- renderUI({
    grade <- input$grade
    choices <- if (grade %in% c("FY1", "FY2")) {
      c("Academia")  # only academia allowed
    } else {
      pay_premia$type
    }
    
    tagList(
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        h6(strong("Flexible Pay Premia Type")),
        tags$span(
          bs_icon("info-circle", size = "1.25em", class = "text-primary"),
          `data-bs-toggle` = "popover",
          `data-bs-trigger` = "focus",
          `data-bs-placement` = "right",
          `data-bs-content` = "For the Welsh 2026 OMFS premia, select any OMFS option. Generally you should only select up to two options. You can select more, but there don't seem to be any realistic combinations other than Academia + one other.",
          tabindex = "0",
          style = "cursor: pointer;"
        )
      ),
      checkboxGroupInput("flexible_pay_premia", label = NULL, choices = choices, selected = NULL),
      tags$script(HTML("
				var popoverTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"popover\"]'))
				var popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
					return new bootstrap.Popover(popoverTriggerEl)
				});
			"))
    )
  })
  
  # Auto-update Welsh incremental choices based on English grade
  observe({
    # Map English grade to Welsh grade
    welsh_grade <- if (input$grade == "FY1") {
      "FY1"
    } else if (input$grade == "FY2") {
      "FY2"
    } else {
      "SpR"  # All ST grades map to SpR
    }
    
    max_increment <- switch(welsh_grade,
                            "FY1" = 2,
                            "FY2" = 2,
                            "SpR" = 9,
                            2)
    
    choices <- as.character(0:max_increment)
    labels <- ifelse(choices == "0", "0/min", choices)
    updateRadioButtons(session, "pay_wales",
                       choices = setNames(choices, labels),
                       selected = choices[1])
  })
  
  # GP band reset if FY1/FY2
  observe({
    if (input$grade == "FY1" || input$grade == "FY2") {
      updateCheckboxInput(session, "gp_band", value = FALSE)
    }
  })
  
  # Ensure GP pay premia is selected when gp_band is TRUE
  observe({
    if (!is.null(input$gp_band)) {
      current <- input$flexible_pay_premia
      if (input$gp_band) {
        # Add "GP" if not present
        if (is.null(current) || !("GP" %in% current)) {
          updateCheckboxGroupInput(session, "flexible_pay_premia",
                                   selected = unique(c(current, "GP")))
        }
      } else {
        # Remove "GP" if present
        if (!is.null(current) && "GP" %in% current) {
          updateCheckboxGroupInput(session, "flexible_pay_premia",
                                   selected = setdiff(current, "GP"))
        }
      }
    }
  })
  
  observeEvent(input$calculate, {   
    req(input$pay_wales, input$grade)
    
    # Set flag that calculation has been done
    vals$calculated <- TRUE
    
    # Common inputs
    selected_weight <- 0
    adj_weight <- 0
    band_mult <- 1
    
    # ===== ENGLAND 2016 CALCULATIONS =====
    get_london_weighting <- function(band) {
      switch(band, "Fringe" = 149, "Outer"  = 527, "Inner"  = 2162, 0)
    }
    
    base_salary <- pay_scales %>% filter(grade == input$grade) %>% pull(base_salary)
    
    # Handle null for pay premia
    selected_premia <- if (is.null(input$flexible_pay_premia)) {
      0
    } else {
      sum(pay_premia$f_premia[pay_premia$type %in% input$flexible_pay_premia], na.rm = TRUE)
    }
    
    # London weight
    selected_weight <- if (input$is_London_weight) get_london_weighting(input$london_weighting) else 0
    adj_premia <- selected_premia
    adj_weight <- selected_weight
    
    # Input validation for hours
    if (is.null(input$non_enhanced_hours) || is.na(input$non_enhanced_hours)) {
      showNotification("Please enter a valid number for average total weekly hours.", type = "error")
      return()
    }
    
    if (is.null(input$enhanced_hours_eng) || is.na(input$enhanced_hours_eng)) {
      showNotification("Please enter a valid number for english enhanced/OOH hours.", type = "error")
      return()
    }
    if (is.null(input$enhanced_hours_wales) || is.na(input$enhanced_hours_wales)) {
      showNotification("Please enter a valid number for welsh enhanced/OOH hours.", type = "error")
      return()
    }
    
    total_time <- input$non_enhanced_hours	
    enhanced_time_eng <- input$enhanced_hours_eng
    enhanced_time_wales <- input$enhanced_hours_wales
    
    # Check if enhanced hours exceed total hours
    if (enhanced_time_eng > total_time) {
      showNotification("Enhanced hours cannot exceed total weekly hours.", type = "error")
      return()
    }
    if (enhanced_time_wales > total_time) {
      showNotification("Enhanced hours cannot exceed total weekly hours.", type = "error")
      return()
    }
    
    basic_hours_eng <- total_time - enhanced_time_eng
    basic_hours_wales <- total_time - enhanced_time_wales
    
    # Estimate based on rota
    base_hourly <- base_salary / 40 / 52
    enhanced_multiplier <- 1.37
    non_enhanced_multiplier <- 1.00
    
    # Weekend frequency validation
    if (is.null(input$weekend_freq) || is.na(input$weekend_freq) || input$weekend_freq < 1) {
      showNotification("Please enter a valid Weekend Frequency (e.g. 5 for 1 in 5).", type = "error")
      return()
    }
    
    weekend_freq <- input$weekend_freq
    
    if(!input$weekend_work){
      weekend_freq <- 9
    }
    
    weekend_supplement <- function(freq, salary) {
      if (freq <= 2) return(salary * 0.15)
      if (freq <= 3) return(salary * 0.10)
      if (freq <= 4) return(salary * 0.075)
      if (freq <= 5) return(salary * 0.06)
      if (freq <= 6) return(salary * 0.05)
      if (freq <= 7) return(salary * 0.04)
      if (freq <= 8) return(salary * 0.03)
      return(0)
    }
    
    weekend_pay <- weekend_supplement(weekend_freq, base_salary)
    adj_weekend_pay <- weekend_pay
    
    # On-call
    on_call_allowance <- if (input$is_on_call_allowance) base_salary * 0.08 else 0
    adj_on_call <- on_call_allowance
    
    # OOH Pay breakdown
    enhanced_pa <- input$enhanced_hours_eng * base_hourly * enhanced_multiplier * 52
    non_enhanced_pa <- basic_hours_eng * base_hourly * non_enhanced_multiplier * 52
    enhanced_pay <- enhanced_pa * (0.37/1.37)
    non_enhanced_pay <- non_enhanced_pa + enhanced_pa * (1/1.37)
    
    # England Gross
    gross_pay_eng <- non_enhanced_pay + enhanced_pay + adj_weekend_pay + adj_on_call + adj_premia + adj_weight
    OOH_pay <- non_enhanced_pay + enhanced_pay + adj_weekend_pay + adj_on_call - base_salary
    
    # Warnings
    if (!is.null(input$non_enhanced_hours) && !is.na(input$non_enhanced_hours) &&
        input$non_enhanced_hours > 48) {
      showNotification("Warning: The total of the enhanced + non enhanced hours you entered are greater than 48. If you have not opted out of the EWTD your hours should be 48 or less. If you have opted out, your hours should be 56 or less. Are your hours correct?", type = "warning")
    }
    
    # ===== WELSH 2002 and 2026 BASE PAY =====
    # Map English grade to Welsh grade
    selected_grade <- if (input$grade == "FY1") {
      "FY1"
    } else if (input$grade == "FY2") {
      "FY2"
    } else {
      "SpR"  # All ST grades map to SpR
    }
    
    selected_increment <- if (!is.null(input$pay_wales)) input$pay_wales else "0"
    
    matched_row <- subset(Wales_pay_scales,
                          grade_wales == selected_grade & wales_pay_increment == selected_increment)
    
    wales_2002_2025 <- if (nrow(matched_row) == 1) matched_row$wales_2002_2025 else 0
    wales_2026 <- if (nrow(matched_row) == 1) matched_row$wales_2026 else 0
    
    # Get band multiplier for Wales 2002
    selected_band <- if (!is.null(input$pay_band)) input$pay_band else "unbanded"
    matched_band_row <- subset(Banding, pay_band_mult_name == selected_band)
    
    if(input$gp_band) {
      band_mult <- 1.45
    } else if (selected_grade == "FY1" && selected_band == "unbanded") {
      band_mult <- 1.05
    } else if (nrow(matched_band_row) == 1) {
      band_mult <- matched_band_row$pay_band_mult
    } else {
      band_mult <- 1
    }
    
    # Wales 2002 calculations (with banding)
    welsh_unband <- ceiling(wales_2002_2025)
    welsh_gross_old <- ceiling(welsh_unband * band_mult)
    welsh_band_enhance <- welsh_gross_old - welsh_unband
    band_percent <- (band_mult - 1) * 100
    
    # Wales 2026 calculations (no banding, uses enhanced hours)
    w_base_hourly <- wales_2026 / 40 / 52
    welsh_enhanced <- 1.5
    w_enhanced_pa <- input$enhanced_hours_wales * w_base_hourly * welsh_enhanced * 52
    w_non_enhanced_pa <- basic_hours_wales * w_base_hourly * non_enhanced_multiplier * 52
    w_enhanced_pay <- w_enhanced_pa * (0.5/1.5)
    w_non_enhanced_pay <- w_non_enhanced_pa + w_enhanced_pa * (1/1.5)
    
    # Get SpR increment 0 base pay for Wales 2026 premia calculations
    spr_increment_0 <- Wales_pay_scales %>% 
      filter(grade_wales == "SpR" & wales_pay_increment == "0") %>% 
      pull(wales_2026)
    
    # Calculate Wales 2026 premia additions
    w_2026_premia <- 0
    if (!is.null(input$flexible_pay_premia)) {
      # GP premia: 0.3 * SpR increment 0
      if ("GP" %in% input$flexible_pay_premia) {
        w_2026_premia <- w_2026_premia + (0.3 * spr_increment_0)
      }
      # OMFS premia: 0.086 * SpR increment 0 (for any OMFS selection)
      omfs_options <- c("OMFS (3 years)", "OMFS (4 years)", "OMFS (5 years)", 
                        "OMFS (6 years)", "OMFS (7 years)", "OMFS (8 years)")
      if (any(omfs_options %in% input$flexible_pay_premia)) {
        w_2026_premia <- w_2026_premia + (0.086 * spr_increment_0)
      }
    }
    
    welsh_gross_2026 <- w_enhanced_pay + w_non_enhanced_pay + w_2026_premia
    
    # ===== PENSION CALCULATIONS =====
    pension_opt <- ifelse(!input$is_under_StatePensionAge || !input$is_opt_in_pension, 0, 1)
    
    pensionable_eng <- base_salary + adj_weight
    pensionable_w_02 <- wales_2002_2025
    pensionable_w_26 <- wales_2026
    
    pension_base_eng <- ifelse(pension_opt < 1, 0, pensionable_eng)	
    pension_base_w_02 <- ifelse(pension_opt < 1, 0, pensionable_w_02)
    pension_base_w_26 <- ifelse(pension_opt < 1, 0, pensionable_w_26)
    
    # Pension function (pensionable pay is made of base salary and london weighting)
    pension_calc <- function(x) {
      if (x <= 13259) return(x * 0.052)
      if (x <= 27288) return(x * 0.065)
      if (x <= 33247) return(x * 0.083)
      if (x <= 49913) return(x * 0.098)
      if (x <= 63994) return(x * 0.107)
      return(x * 0.125)
    }
    
    pension_ded_eng <- pension_calc(pension_base_eng)
    pension_ded_w_02 <- pension_calc(pension_base_w_02)
    pension_ded_w_26 <- pension_calc(pension_base_w_26)
    
    adjGross_eng <- gross_pay_eng - pension_ded_eng
    adjGross_w_02 <- welsh_gross_old - pension_ded_w_02
    adjGross_w_26 <- welsh_gross_2026 - pension_ded_w_26
    
    # ===== STUDENT LOANS =====
    plan_data <- list(
      plan1 = list(threshold = P1_THRESH, rate = 0.09),
      plan2 = list(threshold = P2_THRESH, rate = 0.09),
      plan4 = list(threshold = P4_THRESH, rate = 0.09),
      plan5 = list(threshold = P5_THRESH, rate = 0.09),
      pgloan = list(threshold = PG_THRESH, rate = 0.06)
    )
    
    selected_plans <- input$plans
    has_pg <- "pgloan" %in% selected_plans
    undergrad_plans <- setdiff(selected_plans, "pgloan")
    
    loan_repay <- function(net) {
      total <- 0
      if (length(undergrad_plans) > 0) {
        min_plan <- undergrad_plans[which.min(sapply(undergrad_plans, function(p) plan_data[[p]]$threshold))]
        thresh <- plan_data[[min_plan]]$threshold
        total <- total + max(0, net - thresh) * plan_data[[min_plan]]$rate
      }
      if (has_pg) {
        pg_thresh <- max(plan_data$pgloan$threshold, ifelse(length(undergrad_plans) > 0, plan_data[[min_plan]]$threshold, 0))
        total <- total + max(0, net - pg_thresh) * plan_data$pgloan$rate
      }
      return(total)
    }
    
    student_loan_eng <- loan_repay(gross_pay_eng)
    student_loan_w_02 <- loan_repay(welsh_gross_old)
    student_loan_w_26 <- loan_repay(welsh_gross_2026)
    
    # ===== INCOME TAX =====
    income_tax <- function(gross) {
      pa <- PERSONAL_ALLOWANCE
      if (gross > TAPER_START && gross <= TAPER_END) {
        pa <- max(0, PERSONAL_ALLOWANCE - (gross - TAPER_START) / 2)
      } else if (gross > TAPER_END) {
        pa <- 0
      }
      
      # Account for marginal rate
      bandwidth_increase <- PERSONAL_ALLOWANCE - pa
      higher_band_adj <- HIGHER_BAND + bandwidth_increase
      
      taxable_income <- max(0, gross - pa)
      
      # England/Wales/NI tax bands
      if (taxable_income <= BASIC_BAND) {
        return(taxable_income * BASIC_RATE)
      } else if (taxable_income <= BASIC_BAND + higher_band_adj) {
        return(BASIC_BAND * BASIC_RATE + (taxable_income - BASIC_BAND) * HIGHER_RATE)
      } else {
        return(BASIC_BAND * BASIC_RATE + higher_band_adj * HIGHER_RATE + 
                 (taxable_income - BASIC_BAND - higher_band_adj) * ADDITIONAL_RATE)
      }
    }
    
    # ===== NI =====
    ni_calc <- function(gross) {
      if (gross <= NI_THRESHOLD) {
        return(0)
      } else if (gross <= NI_BASIC_LIMIT) {
        return((gross - NI_THRESHOLD) * NI_RATE1)
      } else {
        basic_ni_band <- NI_BASIC_LIMIT - NI_THRESHOLD
        upper_ni_band <- gross - NI_BASIC_LIMIT
        return(basic_ni_band * NI_RATE1 + upper_ni_band * NI_RATE2)
      }
    }
    
    income_tax_eng <- income_tax(adjGross_eng)
    income_tax_w_02 <- income_tax(adjGross_w_02)
    income_tax_w_26 <- income_tax(adjGross_w_26)
    
    ni_eng <- ifelse(input$is_under_StatePensionAge, ni_calc(adjGross_eng), 0)
    ni_w_02 <- ifelse(input$is_under_StatePensionAge, ni_calc(adjGross_w_02), 0)
    ni_w_26 <- ifelse(input$is_under_StatePensionAge, ni_calc(adjGross_w_26), 0)
    
    # ===== NET PAY =====
    net_eng <- adjGross_eng - student_loan_eng - income_tax_eng - ni_eng
    net_w_02 <- adjGross_w_02 - student_loan_w_02 - income_tax_w_02 - ni_w_02
    net_w_26 <- adjGross_w_26 - student_loan_w_26 - income_tax_w_26 - ni_w_26
    
    pension_accrued_eng <- pension_base_eng / 54
    pension_accrued_w_02 <- pension_base_w_02 / 54
    pension_accrued_w_26 <- pension_base_w_26 / 54
    
    # ===== OUTPUT =====
    fmt <- function(x) sprintf("\u00A3%s", formatC(x, format = "f", digits = 2, big.mark = ","))
    
    out <- c()
    
    # England 2016
    out <- c(out, "<h4>England 2016 Contract</h4>")
    out <- c(out, sprintf("<p><strong>Gross Pay:</strong> %s<br><em>Pensionable:</em> %s</p>", 
                          fmt(gross_pay_eng), fmt(pensionable_eng)))
    
    out <- c(out, "<p><strong>Breakdown:</strong></p><ul>")
    out <- c(out, sprintf("<li>%s hrs at Basic rate: %s</li>", total_time, fmt(non_enhanced_pay)))
    if (enhanced_pay > 0) out <- c(out, sprintf("<li>Enhanced Pay (%s hrs @ 37%%): %s</li>", enhanced_time_eng, fmt(enhanced_pay)))
    if (adj_weekend_pay > 0) out <- c(out, sprintf("<li>Weekend Allowance: %s</li>", fmt(adj_weekend_pay)))
    if (adj_on_call > 0) out <- c(out, sprintf("<li>On-call Availability: %s</li>", fmt(adj_on_call)))
    if (adj_premia > 0) out <- c(out, sprintf("<li>Pay Premia: %s</li>", fmt(adj_premia)))
    if (adj_weight > 0) out <- c(out, sprintf("<li>London Weighting: %s</li>", fmt(adj_weight)))
    out <- c(out, "</ul>")
    
    out <- c(out, "<p><strong>Deductions:</strong></p><ul>")
    if (pension_ded_eng > 0) out <- c(out, sprintf("<li>Pension: %s</li>", fmt(pension_ded_eng)))
    if (student_loan_eng > 0) out <- c(out, sprintf("<li>Student Loan: %s</li>", fmt(student_loan_eng)))
    out <- c(out, sprintf("<li>Income Tax: %s</li>", fmt(income_tax_eng)))
    out <- c(out, sprintf("<li>National Insurance: %s</li>", fmt(ni_eng)))
    out <- c(out, "</ul>")
    
    out <- c(out, sprintf("<p><strong>Net Pay:</strong> %s</p>", fmt(net_eng)))
    if (pension_accrued_eng > 0) out <- c(out, sprintf("<p><em>Pension Accrued:</em> %s</p>", fmt(pension_accrued_eng)))
    
    # Wales 2002
    out <- c(out, "<hr><h4>Wales 2002 Contract</h4>")
    out <- c(out, sprintf("<p><strong>Gross Pay:</strong> %s<br><em>Pensionable:</em> %s</p>", 
                          fmt(welsh_gross_old), fmt(pensionable_w_02)))
    
    if (band_mult > 1) {
      out <- c(out, "<p><strong>Breakdown:</strong></p><ul>")
      if (welsh_unband > 0) out <- c(out, sprintf("<li>Base Pay: %s</li>", fmt(welsh_unband)))
      if (welsh_band_enhance > 0) out <- c(out, sprintf("<li>Banding Uplift (%s%%): %s</li>", band_percent, fmt(welsh_band_enhance)))
      out <- c(out, "</ul>")
    }
    
    out <- c(out, "<p><strong>Deductions:</strong></p><ul>")
    if (pension_ded_w_02 > 0) out <- c(out, sprintf("<li>Pension: %s</li>", fmt(pension_ded_w_02)))
    if (student_loan_w_02 > 0) out <- c(out, sprintf("<li>Student Loan: %s</li>", fmt(student_loan_w_02)))
    out <- c(out, sprintf("<li>Income Tax: %s</li>", fmt(income_tax_w_02)))
    out <- c(out, sprintf("<li>National Insurance: %s</li>", fmt(ni_w_02)))
    out <- c(out, "</ul>")
    
    out <- c(out, sprintf("<p><strong>Net Pay:</strong> %s</p>", fmt(net_w_02)))
    if (pension_accrued_w_02 > 0) out <- c(out, sprintf("<p><em>Pension Accrued:</em> %s</p>", fmt(pension_accrued_w_02)))
    
    # Wales 2026
    out <- c(out, "<hr><h4>Wales 2026 Contract (Proposed)</h4>")
    out <- c(out, sprintf("<p><strong>Gross Pay:</strong> %s<br><em>Pensionable:</em> %s</p>", 
                          fmt(welsh_gross_2026), fmt(pensionable_w_26)))
    
    out <- c(out, "<p><strong>Breakdown:</strong></p><ul>")
    out <- c(out, sprintf("<li>%s hrs at Basic rate: %s</li>", total_time, fmt(w_non_enhanced_pay)))
    if (w_enhanced_pay > 0) out <- c(out, sprintf("<li>Enhanced Pay (%s hrs @ 50%%): %s</li>", enhanced_time_wales, fmt(w_enhanced_pay)))
    if (w_2026_premia > 0) out <- c(out, sprintf("<li>Pay Premia (GP/OMFS): %s</li>", fmt(w_2026_premia)))
    out <- c(out, "</ul>")
    
    out <- c(out, "<p><strong>Deductions:</strong></p><ul>")
    if (pension_ded_w_26 > 0) out <- c(out, sprintf("<li>Pension: %s</li>", fmt(pension_ded_w_26)))
    if (student_loan_w_26 > 0) out <- c(out, sprintf("<li>Student Loan: %s</li>", fmt(student_loan_w_26)))
    out <- c(out, sprintf("<li>Income Tax: %s</li>", fmt(income_tax_w_26)))
    out <- c(out, sprintf("<li>National Insurance: %s</li>", fmt(ni_w_26)))
    out <- c(out, "</ul>")
    
    out <- c(out, sprintf("<p><strong>Net Pay:</strong> %s</p>", fmt(net_w_26)))
    if (pension_accrued_w_26 > 0) out <- c(out, sprintf("<p><em>Pension Accrued:</em> %s</p>", fmt(pension_accrued_w_26)))
    
    # Store values for barplots and comparison
    vals$net_eng <- net_eng
    vals$net_w_02 <- net_w_02
    vals$net_w_26 <- net_w_26
    vals$pensionable_eng <- pensionable_eng
    vals$pensionable_w_02 <- pensionable_w_02
    vals$pensionable_w_26 <- pensionable_w_26
    vals$gross_pay_eng <- gross_pay_eng
    vals$welsh_gross_old <- welsh_gross_old
    vals$welsh_gross_2026 <- welsh_gross_2026
    vals$pension_ded_eng <- pension_ded_eng
    vals$pension_ded_w_02 <- pension_ded_w_02
    vals$pension_ded_w_26 <- pension_ded_w_26
    vals$student_loan_eng <- student_loan_eng
    vals$student_loan_w_02 <- student_loan_w_02
    vals$student_loan_w_26 <- student_loan_w_26
    vals$income_tax_eng <- income_tax_eng
    vals$income_tax_w_02 <- income_tax_w_02
    vals$income_tax_w_26 <- income_tax_w_26
    vals$ni_eng <- ni_eng
    vals$ni_w_02 <- ni_w_02
    vals$ni_w_26 <- ni_w_26
    
    vals$salary_html <- HTML(paste(out, collapse = "\n"))
  })
  
  # Reactive comparison summary that updates when comparison_type changes
  comparison_summary <- reactive({
    req(vals$calculated)
    req(vals$gross_pay_eng, vals$welsh_gross_old, vals$welsh_gross_2026)
    req(vals$net_eng, vals$net_w_02, vals$net_w_26)
    
    is_gross <- input$comparison_type == "gross"
    pay_label <- if(is_gross) "Gross Pay" else "Net Pay"
    
    eng_value <- if(is_gross) vals$gross_pay_eng else vals$net_eng
    w02_value <- if(is_gross) vals$welsh_gross_old else vals$net_w_02
    w26_value <- if(is_gross) vals$welsh_gross_2026 else vals$net_w_26
    
    fmt <- function(x) sprintf("\u00A3%s", formatC(x, format = "f", digits = 2, big.mark = ","))
    
    out <- c()
    out <- c(out, "<hr><h4>Comparison Summary</h4>")
    
    out <- c(out, "<table style='width:100%; border-collapse: collapse;'>")
    out <- c(out, "<tr style='border-bottom: 1px solid #ddd;'>")
    out <- c(out, "<th style='text-align:left; padding:8px;'>Contract</th>")
    out <- c(out, sprintf("<th style='text-align:right; padding:8px;'>%s</th>", pay_label))
    out <- c(out, "<th style='text-align:right; padding:8px;'>Difference vs Wales 2002</th>")
    out <- c(out, "</tr>")
    
    diff_eng <- eng_value - w02_value
    pct_eng <- (diff_eng / w02_value) * 100
    color_eng <- if(diff_eng >= 0) "green" else "red"
    out <- c(out, "<tr style='background-color:#f9f9f9;'>")
    out <- c(out, sprintf("<td style='padding:8px;'>England 2016</td>"))
    out <- c(out, sprintf("<td style='text-align:right; padding:8px;'>%s</td>", fmt(eng_value)))
    out <- c(out, sprintf("<td style='text-align:right; padding:8px; color:%s;'>%s (%s%.2f%%)</td>", 
                          color_eng, fmt(diff_eng), if(diff_eng >= 0) "+" else "", pct_eng))
    out <- c(out, "</tr>")
    
    out <- c(out, "<tr>")
    out <- c(out, sprintf("<td style='padding:8px;'>Wales 2002</td>"))
    out <- c(out, sprintf("<td style='text-align:right; padding:8px;'>%s</td>", fmt(w02_value)))
    out <- c(out, "<td style='text-align:right; padding:8px;'>—</td>")
    out <- c(out, "</tr>")
    
    diff_w26 <- w26_value - w02_value
    pct_w26 <- (diff_w26 / w02_value) * 100
    color_w26 <- if(diff_w26 >= 0) "green" else "red"
    out <- c(out, "<tr style='background-color:#f9f9f9;'>")
    out <- c(out, sprintf("<td style='padding:8px;'>Wales 2026</td>"))
    out <- c(out, sprintf("<td style='text-align:right; padding:8px;'>%s</td>", fmt(w26_value)))
    out <- c(out, sprintf("<td style='text-align:right; padding:8px; color:%s;'>%s (%s%.2f%%)</td>", 
                          color_w26, fmt(diff_w26), if(diff_w26 >= 0) "+" else "", pct_w26))
    out <- c(out, "</tr>")
    
    out <- c(out, "</table>")
    
    HTML(paste(out, collapse = "\n"))
  })
  
  output$comparison_output <- renderUI({
    comparison_summary()
  })
  
  output$salary_output <- renderUI({
    vals$salary_html
  })
  
  # If pensionable pay is greater than gross pay
  output$pension_warning <- renderUI({
    req(vals$pensionable_eng, vals$gross_pay_eng, vals$pensionable_w_02, vals$welsh_gross_old, 
        vals$pensionable_w_26, vals$welsh_gross_2026)
    
    if (vals$pensionable_eng > (vals$gross_pay_eng + 0.00001) || 
        vals$pensionable_w_02 > (vals$welsh_gross_old + 0.00001) ||
        vals$pensionable_w_26 > (vals$welsh_gross_2026 + 0.00001)) {
      HTML(
        '<div class="alert alert-warning" role="alert">
					⚠️ <strong>Note:</strong> Your pensionable pay is higher than your gross pay.
					Please read this <a href="https://www.westmidlandsdeanery.nhs.uk/support/less-than-full-time-training/less-than-full-time-training-guide/pensions" 
					target="_blank" class="alert-link">LTFT Pension info</a>.
				</div>'
      )
    } else {
      NULL
    }
  })
  
  # Barplot render
  output$barplot_output <- renderPlot({
    req(input$calculate)
    
    net_values <- c(vals$net_eng, vals$net_w_02, vals$net_w_26)
    deduction_values <- c(
      vals$pension_ded_eng + vals$student_loan_eng + vals$income_tax_eng + vals$ni_eng,
      vals$pension_ded_w_02 + vals$student_loan_w_02 + vals$income_tax_w_02 + vals$ni_w_02,
      vals$pension_ded_w_26 + vals$student_loan_w_26 + vals$income_tax_w_26 + vals$ni_w_26
    )
    
    values_matrix <- rbind(Net = net_values, Deductions = deduction_values)
    
    par(mgp = c(4, 0.5, 0))
    bp <- barplot(values_matrix,
                  col = c("steelblue", "tomato"),
                  names.arg = c("England 2016", "Wales 2002", "Wales 2026"),
                  ylim = c(0, max(colSums(values_matrix)) * 1.1),
                  main = "Net Pay vs Deductions Comparison",
                  ylab = "(£)",
                  legend.text = TRUE,
                  args.legend = list(x = "topright"),
                  yaxt = "n")
    
    # Add custom y-axis with comma formatting
    y_ticks <- pretty(c(0, max(colSums(values_matrix)) * 1.1))
    axis(2, at = y_ticks, labels = format(y_ticks, big.mark = ","), las = 1)
    
    # Add text labels on top of each bar segment
    cumul_values <- apply(values_matrix, 2, cumsum)
    for (i in 1:ncol(values_matrix)) {
      for (j in 1:nrow(values_matrix)) {
        height <- cumul_values[j, i]
        label <- format(values_matrix[j, i], big.mark = ",")
        text(x = bp[i], y = height - values_matrix[j, i]/2, labels = label, col = "white", cex = 0.9)
      }
    }
  })
  
  # Scroll experiment
  observeEvent(input$calculate, {
    runjs("
			const results = document.getElementById('results_section');
			if (results) {
				const isMobile = /iPhone|iPad|Android/i.test(navigator.userAgent);
				const yOffset = isMobile ? -50 : -150;
				const y = results.getBoundingClientRect().top + window.pageYOffset + yOffset;
				window.scrollTo({top: y, behavior: 'smooth'});

				results.classList.remove('flash');
				void results.offsetWidth;
				results.classList.add('flash');
			}
		")  
  })
  
  output$feedback_message <- renderUI({
    tags$p(
      style = "font-size: 0.9em; color: #6c757d; margin-top: 20px;",
      "This is a work in progress. If you have found an error, please let me know at ",
      tags$a(href = "https://github.com/viren-bajaj/Wales_v_Eng_compare/issues", target = "_blank", "my GitHub page.")
    )
  })
}

shinyApp(ui = ui, server = server)