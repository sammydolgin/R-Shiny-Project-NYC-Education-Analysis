library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)

#-----Importing the dataframe-----#

df <- read_csv("Education.csv")

#-----String manipulation within variable names-----#

newcols = tolower(names(df))
newcols = str_replace_all(string=newcols, pattern=" ", repl="_")
names(df) = newcols
colnames(df)[colnames(df)=="strong_family-community_ties_rating"] = "strong_family_community_ties_rating"

#-----Converting string data to numeric values-----#

df$average_ela_proficiency = as.numeric(df$average_ela_proficiency)
df$average_math_proficiency = as.numeric(df$average_math_proficiency)
df$economic_need_index = as.numeric(df$economic_need_index)
df$student_attendance_rate = as.numeric(str_replace(df$student_attendance_rate, "%", ""))

#-----Removing extraneous columns-----#

df = df[1:41]
df = df[-1:-3]

#-----Creating the racial_majority variable-----#

asian_vector = ((df$percent_asian > df$percent_black) & (df$percent_asian > df$percent_hispanic) & (df$percent_asian > df$percent_white))
black_vector = ((df$percent_black > df$percent_asian) & (df$percent_black > df$percent_hispanic) & (df$percent_black > df$percent_white))
hispanic_vector = ((df$percent_hispanic > df$percent_asian) & (df$percent_hispanic > df$percent_black) & (df$percent_hispanic > df$percent_white))
white_vector = ((df$percent_white > df$percent_asian) & (df$percent_white > df$percent_black) & (df$percent_white > df$percent_hispanic))

df$predominantly_asian = asian_vector
df$predominantly_black = black_vector
df$predominantly_hispanic = hispanic_vector
df$predominantly_white = white_vector
df$racial_majority = " "

for (i in c(1:length(df$racial_majority))) {
  if (df$predominantly_asian[i]) {
    df$racial_majority[i] = "Asian"
  } else if (df$predominantly_black[i]) {
    df$racial_majority[i] = "Black"
  } else if (df$predominantly_hispanic[i]) {
    df$racial_majority[i] = "Hispanic"
  } else if (df$predominantly_white[i]) {
    df$racial_majority[i] = "White"
  }}

#-----Removing N/As, blank values, and outliers-----#

df = filter(
  df, rigorous_instruction_rating!="N/A",
  collaborative_teachers_rating!="N/A",
  supportive_environment_rating!="N/A",
  effective_school_leadership_rating!="N/A",
  strong_family_community_ties_rating!="N/A",
  trust_rating!="N/A"
)

df = filter(df, racial_majority!="Other", racial_majority!=" ", student_attendance_rate>70)

#-----Creating the group_by to plot mean values-----#

gd = df %>% group_by(racial_majority) %>% 
  summarise(economic_need_index = mean(economic_need_index),
            average_ela_proficiency = mean(average_ela_proficiency),
            average_math_proficiency = mean(average_math_proficiency),
            student_attendance_rate = mean(student_attendance_rate))

#-----Creating value hierarchy within Support Metrics to allow for an ordered legend-----#

df$rigorous_instruction_rating = factor(df$rigorous_instruction_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$collaborative_teachers_rating = factor(df$collaborative_teachers_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$supportive_environment_rating = factor(df$supportive_environment_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$effective_school_leadership_rating = factor(df$effective_school_leadership_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$strong_family_community_ties_rating = factor(df$strong_family_community_ties_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$trust_rating = factor(df$trust_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))

#-----Creating string vectors to be used within our SelectizeInput selectors-----#

cols = c(names(df)[25], names(df)[27], names(df)[29], names(df)[31], names(df)[33], names(df)[35])
subj = c(names(df)[37], names(df)[38])

#--------------Shiny Function Initialization--------------#

shinyServer(function(input, output, session){

#--------------Tab 2: Support Metrics, Affect on Academic Performance--------------#  
    
  output$support_scores_boxplot <- renderPlot(
    ggplot(df %>% select(!!sym(input$Support2), racial_majority, !!sym(input$Subject2)),
           aes(x=racial_majority,
              y=!!sym(input$Subject2),
              fill=fct_reorder(!!sym(input$Support2), !!sym(input$Subject2)))) +
    labs(fill="") + 
    geom_boxplot(outlier.size=-1))
  
  output$support_scores_density <- renderPlot(
    ggplot(df %>% select(!!sym(input$Support2), !!sym(input$Subject2)),
           aes(x = !!sym(input$Subject2),
              fill = !!sym(input$Support2),
              color = !!sym(input$Support2))) +
    geom_density(position="identity", alpha=0.15, size=1) +
    scale_fill_manual(values = c("white", "white", "white", "purple")) +
    scale_color_brewer(palette = "Set1") + theme(legend.title=element_blank()))
  
  #--------------Tab 3: Economic Need, affect on Academic Performance--------------#
  
  output$need_scores_scatter <- renderPlot(
    ggplot(df %>% select(!!sym(input$Subject3), economic_need_index, racial_majority), 
           aes(x=economic_need_index, 
              y=!!sym(input$Subject3), 
              color=racial_majority)) + 
    geom_point(size=2, shape=5) +
    geom_point(data=gd, shape = 23, size=10, fill="grey", stroke=1.5) +
    geom_smooth(method=lm, se=TRUE, size=.75, alpha=0.2))
  
  #--------------Tab 4: Support Metrics, affect on Student Attendance--------------#
  
  output$support_attendance_boxplot <- renderPlot(
    ggplot(df %>% select(!!sym(input$Support4), racial_majority, student_attendance_rate),
           aes(x=racial_majority,
            y=student_attendance_rate,
            fill=fct_reorder(!!sym(input$Support4),
            student_attendance_rate))) + 
    labs(fill='') + 
    geom_boxplot(outlier.size=-1)) 
  
  output$support_attendance_density <- renderPlot(
    ggplot(df %>% select(!!sym(input$Support4), student_attendance_rate),
           aes(x = student_attendance_rate,
              fill = !!sym(input$Support4),
              color = !!sym(input$Support4))) +
    geom_density(position="identity", alpha=0.15, size=1) +
    scale_fill_manual(values = c("white", "white", "white", "purple")) +
    scale_color_brewer(palette = "Set1") + 
    theme(legend.title=element_blank())
  )
  
  #--------------Tab 5: Economic Need, affect on Student Attendance--------------#
  
  output$need_attendance_scatter <- renderPlot(
    ggplot(df %>% select(student_attendance_rate, economic_need_index, racial_majority),
           aes(x=economic_need_index,
               y=student_attendance_rate,
               color=racial_majority)) +
    geom_point(size=.5, shape=5) + geom_jitter(height=0.5, size=.5, shape=5) +
    geom_point(data=gd, shape = 23, size=10, fill="grey", stroke=1.5) + geom_smooth(se = TRUE, size=1.5, alpha=0.1))

})
