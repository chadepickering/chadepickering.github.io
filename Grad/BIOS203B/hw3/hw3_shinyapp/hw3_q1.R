#########################################################
### PLEASE GO TO THE FOLLOWING URL TO VIEW SHINY APP: ###
### https://cepickering.shinyapps.io/LA_City_Employee_Payroll/ ###
#########################################################

## Question 1.

### Preprocessing
library(tidyverse)
library(dplyr)
library(shiny)
library(rsconnect)
# payroll <- read_csv("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv", 
#                     col_types = cols(MOU = col_character(), 
#                                      "Record Number" = col_character()))
# 
# # 1. Tidy data.
# process <- payroll %>% 
#   mutate( # strip dollar signs, rename cols
#       year = as.numeric(Year),
#       rec_id = `Record Number`,
#       base_pay = as.numeric(str_sub(`Base Pay`, 2, -1)),
#       overtime_pay = as.numeric(str_sub(`Overtime Pay`, 2, -1)),
#       # all bonus, lump sum, and other adjustments = other pay (payroll exp.)
#       other_pay = as.numeric(str_sub(`Other Pay (Payroll Explorer)`, 2, -1)),
#       total_pay = base_pay + overtime_pay + other_pay,
#       hourly_pay = as.numeric(str_sub(`Hourly or Event Rate`, 2, -1)),
#       proj_annual_salary = as.numeric(str_sub(`Projected Annual Salary`, 2, -1))
#     ) %>%
#   select(
#         year, rec_id, base_pay, overtime_pay, other_pay, total_pay,
#         hourly_pay, proj_annual_salary, dept = `Department Title`, 
#         job_title = `Job Class Title`
#       )

# Make RDS file from 'process':
# saveRDS(process, "process.rds")

process <- 
  readRDS('process.rds') # needs to be this way for publishing
# for reproducibility, must change working directory or this to full path

# Tibbles independent of user input:

# 2. Visualize the total LA City payroll of each year, 
# with breakdown into base pay, overtime pay, and other pay.
pay_by_year <- process %>% 
  group_by(year) %>%
  summarise(
    base = sum(base_pay, na.rm = TRUE), 
    overtime = sum(overtime_pay, na.rm = TRUE),
    other = sum(other_pay, na.rm = TRUE),
    total = sum(total_pay, na.rm = TRUE)
  )

rep_year <- rep(pay_by_year$year, 3)
pay_values <- c(pay_by_year$base, pay_by_year$overtime, pay_by_year$other)
pay_type <- c(rep("Base", 5), rep("Overtime", 5), rep("Other", 5))
app_2_tbl <- cbind(rep_year, pay_values, pay_type)
app_2_tbl <- as_tibble(app_2_tbl)

# 4. Visualize the mean or median payroll, 
# with breakdown into base pay, overtime pay, and other pay, 
# of top n earning departments.
dept_earnings <- process %>%
  group_by(dept, year) %>%
  summarise(
    # Sums of pay types
    base = sum(base_pay, na.rm = TRUE), 
    overtime = sum(overtime_pay, na.rm = TRUE),
    other = sum(other_pay, na.rm = TRUE),
    total = sum(total_pay, na.rm = TRUE),
    # Means of pay types
    mean_base = mean(base_pay, na.rm = TRUE),
    mean_overtime = mean(overtime_pay, na.rm = TRUE),
    mean_other = mean(other_pay, na.rm = TRUE),
    mean_total = mean(total_pay, na.rm = TRUE),
    # Medians of pay types
    median_base = median(base_pay, na.rm = TRUE),
    median_overtime = median(overtime_pay, na.rm = TRUE),
    median_other = median(other_pay, na.rm = TRUE),
    median_total = median(total_pay, na.rm = TRUE)
  )

year_choices <- unique(process$year)

ui <- fluidPage(
  
  titlePanel("LA City Employee Payroll"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_1",
                  "How many employees?",
                  min = 1,
                  max = 50,
                  value = 10),
      
      selectInput(inputId = "num_2", label = "Year", choices = year_choices,
                  selected = 2017),
      
      sliderInput("num_3",
                  "How many departments?",
                  min = 1,
                  max = 50,
                  value = 5),
      
      selectInput(inputId = "num_4", label = "Method", 
                  choices = c("Mean", "Median"),
                  selected = "Median"),
      
      sliderInput("num_5",
                  "How many job titles?",
                  min = 1,
                  max = 50,
                  value = 10)
    
    ),
    
    
    mainPanel(
      plotOutput("static_payroll"),
      plotOutput("employee_earn"),
      plotOutput("dept_earn"),
      plotOutput("dept_cost"),
      plotOutput("total_vs_proj")
    )
  )
)





server <- function(input, output) {
  
  output$static_payroll <- renderPlot({
    
    # 2.
    ggplot(app_2_tbl) + 
      geom_col(mapping = aes(x = rep_year, y = as.numeric(pay_values), 
                             fill = factor(pay_type, 
                                    levels = c("Base", 
                                                "Overtime", 
                                                "Other"))),
               position = "dodge") +
      labs(title = "2. Total LA City Payroll by Year and Type",
           x = "Year",
           y = "Total Pay per Category", 
           fill = "Pay Ctg.")
    
  })

    # 3. 
    output$employee_earn <- renderPlot({
    
    top_earners <- process %>%
      filter(year == input$num_2) %>%
      arrange(desc(total_pay)) %>%
      head(input$num_1) %>% 
      select(rec_id, base_pay, overtime_pay, other_pay)
      
    earnings_vals <- c(top_earners$base_pay, 
                    top_earners$overtime_pay, 
                    top_earners$other_pay)
    pay_val <- c(rep("Base", input$num_1), 
                 rep("Overtime", input$num_1), 
                 rep("Other", input$num_1))
    id_val <- rep(top_earners$rec_id, 3)
    app_3_tbl <- cbind(id_val, pay_val, earnings_vals)
    app_3_tbl <- as_tibble(app_3_tbl)

    ggplot(app_3_tbl, aes(x = reorder(id_val, as.numeric(earnings_vals)), 
                          y = as.numeric(earnings_vals), 
                             fill = factor(pay_val, 
                                           levels = c("Other", 
                                                      "Overtime", 
                                                      "Base")))) + 
      geom_bar(stat = "identity") + coord_flip() +
      labs(title = paste("3. Top", input$num_1, "Earners in LA in", 
                         input$num_2),
          x = "Employee ID",
          y = "Total Pay per Category", 
          fill = "Pay Ctg.") 
  
  
  })
    
    # 4. 
    output$dept_earn <- renderPlot({
      
      if(input$num_4 == "Median"){
        top_depts <- dept_earnings %>%
          filter(year == input$num_2) %>% 
          arrange(desc(median_total)) %>%
          head(input$num_3) %>% 
          select(c(`dept`, `total`, starts_with(tolower(input$num_4))))
        methods_vals <- c(top_depts$median_base, 
                          top_depts$median_overtime, 
                          top_depts$median_other)
      } else {
        top_depts <- dept_earnings %>%
          filter(year == input$num_2) %>% 
          arrange(desc(mean_total)) %>%
          head(input$num_3) %>% 
          select(c(`dept`, `total`, starts_with(tolower(input$num_4))))
        methods_vals <- c(top_depts$mean_base, 
                          top_depts$mean_overtime, 
                          top_depts$mean_other)
      }
      
      pay_val <- c(rep("Base", input$num_3), 
                   rep("Overtime", input$num_3), 
                   rep("Other", input$num_3))
      id_val <- rep(top_depts$dept, 3)
      app_4_tbl <- cbind(id_val, pay_val, methods_vals)
      app_4_tbl <- as_tibble(app_4_tbl)
      
      ggplot(app_4_tbl, aes(x = reorder(id_val, as.numeric(methods_vals)), 
                            y = as.numeric(methods_vals), 
                            fill = factor(pay_val, 
                                          levels = c("Other", 
                                                     "Overtime", 
                                                     "Base")))) + 
        geom_bar(stat = "identity") + coord_flip() +
        labs(title = paste("4. Top", input$num_3, 
                           "Earning Departments in LA in", 
                           input$num_2, "by", input$num_4, "Payroll"),
             x = "Department",
             y = c("Total", input$num_4, "Earnings per Category"), 
             fill = "Pay Ctg.") 
      
      
    })
  
    # 5. 
    output$dept_cost <- renderPlot({
      top_depts <- dept_earnings %>%
        filter(year == input$num_2) %>% 
        arrange(desc(total)) %>%
        head(input$num_3) %>% 
        select(c(`dept`, `total`, `base`, `overtime`, `other`))
        
      tot_vals <- c(top_depts$base, 
                          top_depts$overtime, 
                          top_depts$other)
      
      pay_val <- c(rep("Base", input$num_3), 
                   rep("Overtime", input$num_3), 
                   rep("Other", input$num_3))
      id_val <- rep(top_depts$dept, 3)
      app_5_tbl <- cbind(id_val, pay_val, tot_vals)
      app_5_tbl <- as_tibble(app_5_tbl)
      
      ggplot(app_5_tbl, aes(x = reorder(id_val, as.numeric(tot_vals)), 
                            y = as.numeric(tot_vals), 
                            fill = factor(pay_val, 
                                          levels = c("Other", 
                                                     "Overtime", 
                                                     "Base")))) + 
        geom_bar(stat = "identity") + coord_flip() +
        labs(title = paste("5. Top", input$num_3, 
                           "Most Costly Departments in LA in", 
                           input$num_2, "by Type"),
             x = "Department",
             y = c("Total Earnings per Category"), 
             fill = "Pay Ctg.") 
      
      
    })
    
    # 6. What is the mean difference between total_pay and proj_annual_salary
    # by job_title per year? Give top n.
    output$total_vs_proj <- renderPlot({
      total_proj <- process %>%
        filter(year == input$num_2) %>% 
        mutate(diff_obs_proj = total_pay - proj_annual_salary) %>%
        group_by(job_title) %>%
        summarize(mean_diff = mean(diff_obs_proj)) %>%
        arrange(desc(mean_diff)) %>%
        head(input$num_5)
    
      app_6_tbl <- as_tibble(total_proj)
      
      ggplot(app_6_tbl, aes(x = reorder(job_title, as.numeric(mean_diff)), 
                            y = as.numeric(mean_diff))) + 
        geom_bar(stat = "identity") + coord_flip() +
        labs(title = paste("6. Top", input$num_5, 
                           "Most Underestimated Jobs in LA in", 
                           input$num_2),
             subtitle = "Mean Difference in Projected 
                          Annual Salary and Total Pay",
             x = "Job Title",
             y = "Mean Difference (Proj. Annual Salary - Obs. Pay)") 
      
      
    })
    
  
}


shinyApp(ui = ui, server = server)









