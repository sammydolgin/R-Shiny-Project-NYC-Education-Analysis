library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

df <- read_csv("Education.csv")

newcols = tolower(names(df))
newcols = str_replace_all(string=newcols, pattern=" ", repl="_")
names(df) = newcols
colnames(df)[colnames(df)=="strong_family-community_ties_rating"] = "strong_family_community_ties_rating"
df = filter(
  df, rigorous_instruction_rating!="N/A",
  collaborative_teachers_rating!="N/A",
  supportive_environment_rating!="N/A",
  effective_school_leadership_rating!="N/A",
  strong_family_community_ties_rating!="N/A",
  trust_rating!="N/A"
)
df$average_ela_proficiency = as.numeric(df$average_ela_proficiency)
df$average_math_proficiency = as.numeric(df$average_math_proficiency)
df = df[1:41]
df = df[-1:-3]
asian_vector = ((df$percent_asian > df$percent_black) & (df$percent_asian > df$percent_hispanic) & (df$percent_asian > df$percent_white))
black_vector = ((df$percent_black > df$percent_asian) & (df$percent_black > df$percent_hispanic) & (df$percent_black > df$percent_white))
hispanic_vector = ((df$percent_hispanic > df$percent_asian) & (df$percent_hispanic > df$percent_black) & (df$percent_hispanic > df$percent_white))
white_vector = ((df$percent_white > df$percent_asian) & (df$percent_white > df$percent_black) & (df$percent_white > df$percent_hispanic))

df$predominantly_asian = asian_vector
df$predominantly_black = black_vector
df$predominantly_hispanic = hispanic_vector
df$predominantly_white = white_vector
df$racial_majority = ""

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

df = filter(df, racial_majority!="Other", racial_majority!="")
df$economic_need_index = as.numeric(df$economic_need_index)

df$student_attendance_rate = as.numeric(str_replace(df$student_attendance_rate, "%", ""))
gd = df %>% group_by(racial_majority) %>% summarise(economic_need_index = mean(economic_need_index), average_ela_proficiency = mean(average_ela_proficiency))

df$rigorous_instruction_rating = factor(df$rigorous_instruction_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$collaborative_teachers_rating = factor(df$collaborative_teachers_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$supportive_environment_rating = factor(df$supportive_environment_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$effective_school_leadership_rating = factor(df$effective_school_leadership_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$strong_family_community_ties_rating = factor(df$strong_family_community_ties_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))
df$trust_rating = factor(df$trust_rating, levels = c("Not Meeting Target", "Approaching Target", "Meeting Target", "Exceeding Target"))


cols = c(names(df)[25], names(df)[27], names(df)[29], names(df)[31], names(df)[33], names(df)[35])
subj = c(names(df)[37], names(df)[38])