library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

shinyUI(dashboardPage(
  dashboardHeader(title = 'NYC Education Analysis'),

  dashboardSidebar(
    sidebarUserPanel("Sammy Dolgin"),
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("book")),
      menuItem("Support Metrics, Test Scores", tabName = "support-scores", icon = icon("database")),
      menuItem("Economic Need, Test Scores", tabName = "need-scores", icon = icon("database")),
      menuItem("Support Metrics, Attendance", tabName = "support-attendance", icon = icon("database")),
      menuItem("Economic Need, Attendance", tabName = "need-attendance", icon = icon("database")))

  ),

  dashboardBody(
    tabItems(
      
      tabItem(tabName="intro",
        fluidRow(
         box(width = 12,
          tags$h1(strong("Measuring the Impact of Environmental and Socioeconomic Factors on Student Attendance and Academic Performance")),
          tags$h1(p("\n")),
          tags$h4(          
          p("Since it's inception in 2015, not-for-profit organization PASSNYC has leveraged data science to assist under-represented groups of students in gaining the resources they need to achieve academic success. In this application, which relies on public New York City education data (K-12) that has been organized by PASSNYC, we will look at key metrics, such as community support and economic need, and get an understanding of how they influence academic performance and student attendance."),
          p("Additionally, we will observe how racial wealth inequality influences gaps in academic performance, and extrapolate on the potential for students of all races given an equal economic footing."),
          p("All individual datapoints denote a single New York City school, with each individual metric referring to a composite score or aggregated average for that particular facility. The key variables we will be observing include the following:")),

          tags$h2(
          p(strong("Metrics of Community Support"))),
          tags$h4(
          p(strong("- Strong Family-Community Ties Rating"))),
          tags$h5(
          p("Ordinal rating of how well the school forms effective partnerships with families to improve the school"), 
          p(em("(Not Meeting Target, Approaching Target, Meeting Target, Exceeding Target)"))),
          tags$h4(
          p(strong("- Trust Rating"))), 
          tags$h5(
          p("Ordinal rating of whether the relationships between administrators, educators, students, and families are based on trust and respect."),
          p(em("(Not Meeting Target, Approaching Target, Meeting Target, Exceeding Target)"))),
          tags$h4(
          p(strong("- Effective School Leadership Rating"))),
          tags$h5(
          p("Ordinal rating of how well school leadership inspires the school community with a clear instructional vision and effectively distributes leadership to realize this vision."),
          p(em("(Not Meeting Target, Approaching Target, Meeting Target, Exceeding Target)"))),
          tags$h4(
          p(strong("- Supportive Environment Rating"))),
          tags$h5(
          p("Ordinal rating of how well the school establishes a culture where students feel safe, challenged to grow, and supported to meet high expectations."),
          p(em("(Not Meeting Target, Approaching Target, Meeting Target, Exceeding Target)"))),
          tags$h4(
          p(strong("- Collaborative Teachers Rating"))),
          tags$h5(
          p("Ordinal rating of how well teachers participate in opportunities to develop, grow, and contribute to the continuous improvement of the school community."),
          p(em("(Not Meeting Target, Approaching Target, Meeting Target, Exceeding Target)"))),
          tags$h4(
          p(strong("- Rigorous Instruction Rating"))),
          tags$h5(
          p("Ordinal rating of how well the curriculum and instruction engage students, build critical-thinking skills, and are aligned to the Common Core."),
          p(em("(Not Meeting Target, Approaching Target, Meeting Target, Exceeding Target)"))),
          
          tags$h2(
          p(strong("Metrics of Economic Need"))),
          tags$h4(
          p(strong("- Economic Need Index (ENI)"))),
          tags$h5(
          p("Measure of economic need, as designed by PASSNYC, that is calculated as follows:"),
          p(em("ENI = (%temp housing) + (% HRA eligible *0.5) + (% free lunch eligible *0.5)")),
          p("The higher the index, the higher the need.")),
          tags$h2(
          p(strong("Measures of Academic Performance"))),
          tags$h4(
          p(strong("- Average ELA Proficiency"))),
          tags$h5(
          p("Indexed measure of students' English/Language Arts performance, which reflects the extent to which students demonstrate the level of understanding expected at their grade level, based on the New York State P-12 Common Core Learning Standards.")),
          tags$h4(
          p(strong("- Average Math Proficiency"))),
          tags$h5(
          p("Indexed measure of students' Mathematics performance, which reflects the extent to which students demonstrate the level of understanding expected at their grade level, based on the New York State P-12 Common Core Learning Standards.")),
          tags$h2(
          p(strong("Measure of Student Attendance"))),
          tags$h4(
          p(strong("- Student Attendance Rate"))),
          tags$h5(
          p("Refers to the percent of total school days attended by students at each school.")),
          tags$h2(
          p(strong("Measure of Racial Background"))),
          tags$h4(
          p(strong("- Racial Majority"))),
          tags$h5(
          p("Indicates the race that is most commonly found among students within each school."))
          ))),
          
      
       tabItem(tabName="support-scores",
              fluidPage(
                box(width=12, selectizeInput(inputId = "Support2",
                                label = "Support Metric",
                                choices = cols),
                    selectizeInput(inputId = "Subject2",
                                label = "Subject",
                                choices = subj),
                    tags$h4(
                      p(strong("Effect of Support Levels on Academic Scores by Race"))
                    ),
                    tags$h5(
                      p("This plot shows the impact of increasing support levels on a school's academic performance, broken down into groups of schools with different racial majorities. For each group, the boxes increase in amount of support from left to right."),
                      p("We observe that across most combinations of support metric, subject, and race, there is a notable increase in academic performance as the amount of support increases. That said, the magnitude of this impact deviates heavily between racial groups based on what support metric and/or subject we are looking at."),
                    ),
                    fluidRow(plotOutput("support_scores_boxplot")),
                    tags$h4(p(strong("Density of Academic Scores by Level of Support"))),
                    tags$h5(
                      p("This plot shows the density of academic scores, broken down into different levels of support. For ease of visibility, the purple shaded region refers to those schools that exceed targets in the given support metric."),
                      p("We observe that across most support metrics, those schools that exceed support targets tend to have the greatest density of high academic achievment."),
                    ),
                    fluidRow(plotOutput("support_scores_density")),
                
                
                ))),
      
      tabItem(tabName="need-scores",
              fluidPage(
                box(width=12, selectizeInput(inputId = "Subject3",
                                   label = "Subject",
                                   choices = subj),
                    tags$h4(p(strong("Effect of Economic Need on Academic Scores by Race"))),
                    tags$h5(
                    p("This plot shows the impact of increasing student economic need on a school's academic performance, broken down into groups of schools with different racial majorities."),
                    p("Across all races, we observe a clear negative linear relationship between student economic need and academic performance."),
                    p("While there is visible clustering of races at different levels of economic need, the regression lines show comparable academic performance between races when regressed across all levels of economic need.")
                    ),
                    fluidRow(plotOutput("need_scores_scatter")),
                    tags$h5(
                    p(em("Large diamond coordinates refer to the mean values for all schools of each racial majority"))
                             )))),
      
      
      tabItem(tabName="support-attendance",
              fluidPage(
                box(width=12, selectizeInput(inputId = "Support4",
                                   label = "Support",
                                   choices = cols),
                    tags$h4(p(strong("Effect of Support Levels on Attendance Rates by Race"))),
                    tags$h5(
                      p("This plot shows the impact of increasing support levels on a school's average attendance rate, broken down into groups of schools with different racial majorities. For each group, the boxes increase in amount of support from left to right."),
                      p("We observe that across most combinations of support metric and racial majority, there is a notable increase in attendance rates as the amount of support increases. That said, the magnitude of this impact deviates heavily between racial groups based on what support metric we are looking at."),
                    ),
                    fluidRow(plotOutput("support_attendance_boxplot")),
                    tags$h4(p(strong("Density of Attendance Rates by Level of Support"))),
                    tags$h5(
                      p("This plot shows the density of attendance rates, broken down into different levels of support. For ease of visibility, the purple shaded region refers to those schools that exceed targets in the given support metric."),
                      p("We observe that across most support metrics, those schools that exceed support targets tend to have the greatest density of high attendance rates."),
                    ),
                    fluidRow(plotOutput("support_attendance_density")),
                ))),
      
      tabItem(tabName="need-attendance",
              box(width=12, tags$h4(p(strong("Effect of Economic Need on Attendance Rates by Race"))),
                  tags$h5(
                    p("This plot shows the impact of increasing student economic need on a school's mean attendance rate, broken down into groups of schools with different racial majorities."),
                    p("Across all races, we observe a negative relationship between student economic need and mean attendance rate, though the trend appears logarithmic in that increasing economic need has an exponential impact on decreasing attendance rates."),
                    p("While there is visible clustering of races at different levels of economic need, similar to what was observed with academic performance, the regression lines show comparable attendance rates between races when regressed across all levels of economic need.")
                  ),
              fluidPage(
                    fluidRow(plotOutput("need_attendance_scatter"))),
              tags$h5(
              p(em("Large diamond coordinates refer to the mean values for all schools of each racial majority")))
              ))
              
      
    ))))