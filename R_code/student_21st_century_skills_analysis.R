## 21st-century skills Analysis for 3rd Grade =========

# Load necessary libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(magrittr)
library(lubridate)
library(car)

### Baseline data for student evaluation 2022 --------------

# Load student lookup data
lookup.students <- fread('./Raw_data/pbl_lookup_students.csv', encoding = 'UTF-8')
unique(lookup.students, by = 'student_id')

# Participating municipalities and schools 
total_students <- lookup.students[, .N]

# Calculate the count and percentage for each combination of municipio and school
lookup.students[, .(N = .N, perc = sprintf("%.2f", .N / total_students * 100)), by = .(municipio, school)]

# Load baseline rubric data
rub_student_lb_raw <- fread("./Raw_data/pbl_microdata_21st_century_skills.csv", encoding = 'UTF-8', na.strings = c("", "NA")) 

# Select pretest columns
cols.pre <- grep("student_id|fecha|SQ00[1-4]", colnames(rub_student_lb_raw), value = TRUE)

# Filter out rows with missing student IDs and select data before September 2022
rub_student_lb <- rub_student_lb_raw[!is.na(student_id), .SD, .SDcols = cols.pre][fecha < '2022-09-01', ]

# Label data as Baseline
rub_student_lb[, prueba := "Baseline"]

glimpse(rub_student_lb)

# Save baseline data to Excel
write.xlsx(rub_student_lb, "./Processed_data/lbase_estudiantes_2022.xlsx", overwrite = TRUE)

### Post-test rubric data --------

# Load post-test rubric data
rub_student_post_raw <- fread("./Raw_data/pbl_microdata_21st_century_skills.csv", encoding = 'UTF-8', na.strings = c("", "NA")) 

# Select post-test columns
cols.post <- grep("student_id|fecha|SQ00[1-4]", colnames(rub_student_lb_raw), value = TRUE)

# Filter out rows with missing student IDs and select data after September 2022
rub_student_post <- rub_student_post_raw[!is.na(student_id), .SD, .SDcols = cols.pre][fecha > '2022-09-01', ]

# Label data as Endline
rub_student_post[, prueba := "Endline"]

glimpse(rub_student_post)

### Combine pre and post-test data --------------

# Combine pre and post-test data into a single data table
l.student <- list(rub_student_lb, rub_student_post)
studentPrePost <- rbindlist(l.student, use.names = TRUE, fill = TRUE)

# Select records of students who took both the pretest and post-test
studentPrePost <- studentPrePost[, if (.N > 1) .SD, by = student_id]

# Check the number of students for each test type
studentPrePost[, .N, by = 'prueba']

# Merge demographic information (gender, municipality, school) into studentPrePost
studentPrePost[lookup.students[, .(student_id, student_gender, municipio, school)], 
               `:=`(student_gender = i.student_gender, 
                    municipio = i.municipio, 
                    school = i.school), 
               on = 'student_id']

colnames(studentPrePost)

# Replace missing responses with 0 for skill components
cols.na <- grep('resolucionproblemas|trabajocolaborativo|creatividad|comunicacion|pensamientocritico', 
                names(studentPrePost), value = TRUE)
studentPrePost[, (cols.na) := lapply(.SD, nafill, fill = 0), .SDcols = cols.na]

### Totals by 21st-century skills components --------------

## Problem-Solving Skills --------
problem_cols <- grep('^resolucionproblemas', names(studentPrePost), value = TRUE)
studentPrePost[, total_problems := rowSums(.SD), .SDcols = problem_cols]

# Categorize problem-solving skill levels
studentPrePost[, problem_level := ifelse(total_problems == 0, 'Beginning', 
                                         ifelse(total_problems == 1, 'In Progress',
                                                ifelse(total_problems == 2, 'Achieved',
                                                       ifelse(total_problems == 3, 'Extended', NA))))]

## Collaboration Skills --------
collaboration_cols <- grep('^trabajocolaborativo', names(studentPrePost), value = TRUE)
studentPrePost[, total_collaboration := rowSums(.SD), .SDcols = collaboration_cols]

# Categorize collaboration skill levels
studentPrePost[, collaboration_level := ifelse(total_collaboration == 0, 'Beginning', 
                                               ifelse(total_collaboration == 1, 'In Progress',
                                                      ifelse(total_collaboration == 2, 'Achieved',
                                                             ifelse(total_collaboration == 3, 'Extended', NA))))]

## Creativity Skills --------
creativity_cols <- grep('^creatividad', names(studentPrePost), value = TRUE)
studentPrePost[, total_creativity := rowSums(.SD), .SDcols = creativity_cols]

# Categorize creativity skill levels
studentPrePost[, creativity_level := ifelse(total_creativity == 0, 'Beginning', 
                                            ifelse(total_creativity == 1, 'In Progress',
                                                   ifelse(total_creativity == 2, 'Achieved',
                                                          ifelse(total_creativity == 3, 'Extended', NA))))]

## Communication Skills --------
communication_cols <- grep('^comunicacion', names(studentPrePost), value = TRUE)
studentPrePost[, total_communication := rowSums(.SD), .SDcols = communication_cols]

# Categorize communication skill levels
studentPrePost[, communication_level := ifelse(total_communication == 0, 'Beginning', 
                                               ifelse(total_communication == 1, 'In Progress',
                                                      ifelse(total_communication == 2, 'Achieved',
                                                             ifelse(total_communication == 3, 'Extended', NA))))]

## Critical Thinking Skills --------
thinking_cols <- grep('^pensamientocritico', names(studentPrePost), value = TRUE)
studentPrePost[, total_thinking := rowSums(.SD), .SDcols = thinking_cols]

# Categorize critical thinking skill levels
studentPrePost[, thinking_level := ifelse(total_thinking == 0, 'Beginning', 
                                          ifelse(total_thinking == 1, 'In Progress',
                                                 ifelse(total_thinking == 2, 'Achieved',
                                                        ifelse(total_thinking == 3, 'Extended', NA))))]

### Standardized scale for inferential analyses --------------

# Calculate the z-score for each raw variable
studentPrePost[, z_problem := scale(total_problems)]
studentPrePost[, z_collaborative := scale(total_collaboration)]
studentPrePost[, z_creativity := scale(total_creativity)]
studentPrePost[, z_communication := scale(total_communication)]
studentPrePost[, z_thinking := scale(total_thinking)]

# Convert the z-score to a 0-10 scale
convert_to_0_10 <- function(z) {
  return((z - min(z)) / (max(z) - min(z)) * 10)
}

# Apply the conversion to each z-score
studentPrePost[, std_problem := convert_to_0_10(z_problem)]
studentPrePost[, std_collaborative := convert_to_0_10(z_collaborative)]
studentPrePost[, std_creativity := convert_to_0_10(z_creativity)]
studentPrePost[, std_communication := convert_to_0_10(z_communication)]
studentPrePost[, std_thinking := convert_to_0_10(z_thinking)]

### Overall scale of 21st-century skills --------------

# Step 1: Create an overall score by summing the scores across all skill dimensions
studentPrePost[, overall_score := std_problem + std_collaborative + std_creativity + std_communication + std_thinking]

# Step 2: Convert the overall score to a scale from 0 to 10
studentPrePost[, overall_score_scaled := 10 * (overall_score - min(overall_score)) / (max(overall_score) - min(overall_score))]

# Save the pre-post data
write.csv(studentPrePost, "./Processed_data/21century-prepost.csv")

### Wide dataset for JASP --------------

# Reshape the dataset into a wide format for JASP analysis
wide_21skills <- dcast(studentPrePost, student_id + student_gender ~ prueba, value.var = "overall_score_scaled", fun.aggregate = mean)

# Save the wide dataset
write.csv(wide_21skills, './Processed_data/21_wide.csv')

# View the first few rows of the data table
head(studentPrePost)

### Plot Overall Scale Pre-Post by Gender --------------

# Summarize the data by gender and assessment type (pretest/posttest)
summ_data <- studentPrePost[, .(
  mean_overall_score = mean(overall_score_scaled, na.rm = TRUE),
  se_overall_score = sd(overall_score_scaled, na.rm = TRUE) / sqrt(.N)
), by = .(prueba, student_gender)]

# Create the line chart showing mean overall scores by gender
# Relabel the student_gender column

summ_data$student_gender <- factor(summ_data$student_gender, 
                                   levels = c("F", "M"), 
                                   labels = c("Female students", "Male students"))

ggplot(summ_data, aes(x = prueba, y = mean_overall_score, group = student_gender, color = student_gender)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "Assessment", 
       y = "Mean Overall Scaled Score (0-10)", 
       color = "Gender") +
  theme_minimal()

# Save the plot
ggsave("./plots_paper/fig10_21st_prepost.png", height = 8, width = 10, dpi = 300)
ggsave("./plots_paper/fig10_21st_prepost.pdf", height = 8, width = 10, bg = "whitesmoke")

### Perform T-tests --------------

# Wilcoxon test for overall scores between pretest and posttest
res <- wilcox.test(overall_score_scaled ~ prueba, data = studentPrePost, exact = FALSE)
res

# Paired samples t-test for overall scores between pretest and posttest
res2 <- t.test(studentPrePost$overall_score_scaled[studentPrePost$prueba == "Baseline"],
               studentPrePost$overall_score_scaled[studentPrePost$prueba == "Endline"],
               paired = TRUE)
res2

### ANOVA to Test Effects of Gender and Test Condition --------------

# Convert 'student_gender' and 'prueba' to factors
studentPrePost[, student_gender := factor(student_gender)]
studentPrePost[, prueba := factor(prueba)]

# Perform a two-way ANOVA to test the effects of gender and measurement condition
anova_result <- aov(overall_score_scaled ~ student_gender * prueba, data = studentPrePost)
summary(anova_result)

# Alternatively, perform a Type II ANOVA
anova_result_typeII <- Anova(anova_result, type = "II")
summary(anova_result_typeII)

### Barplot by 21st-Century Skills --------------

# Summarize the data by calculating the mean and standard error for each skill, grouped by 'prueba'
summ_data <- studentPrePost[, .(
  mean_problem = mean(std_problem, na.rm = TRUE),
  se_problem = sd(std_problem, na.rm = TRUE) / sqrt(.N),
  mean_collaborative = mean(std_collaborative, na.rm = TRUE),
  se_collaborative = sd(std_collaborative, na.rm = TRUE) / sqrt(.N),
  mean_creativity = mean(std_creativity, na.rm = TRUE),
  se_creativity = sd(std_creativity, na.rm = TRUE) / sqrt(.N),
  mean_communication = mean(std_communication, na.rm = TRUE),
  se_communication = sd(std_communication, na.rm = TRUE) / sqrt(.N),
  mean_thinking = mean(std_thinking, na.rm = TRUE),
  se_thinking = sd(std_thinking, na.rm = TRUE) / sqrt(.N)
), by = prueba]

# Reshape the data into long format for plotting
melted_summ_data <- melt(summ_data, 
                         id.vars = "prueba", 
                         measure.vars = c("mean_problem", "mean_collaborative", "mean_creativity", "mean_communication", "mean_thinking"),
                         variable.name = "skill", 
                         value.name = "mean_score")

# Reshape the standard error data
se_summ_data <- melt(summ_data, 
                     id.vars = "prueba", 
                     measure.vars = c("se_problem", "se_collaborative", "se_creativity", "se_communication", "se_thinking"),
                     variable.name = "skill", 
                     value.name = "se_score")

# Combine the mean scores and standard errors
final_summ_data <- cbind(melted_summ_data, se_summ_data[, .(se_score)])

# Rename skills for better readability
final_summ_data[, skill := factor(skill, 
                                  levels = c("mean_problem", "mean_collaborative", "mean_creativity", "mean_communication", "mean_thinking"),
                                  labels = c("Problem Solving", "Collaborative Work", "Creativity", "Communication", "Critical Thinking"))]

# Create the bar plot with error bars
ggplot(final_summ_data, aes(x = skill, y = mean_score, fill = prueba)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_score - se_score, ymax = mean_score + se_score),
                position = position_dodge(0.9), width = 0.25) +
  labs(x = "21st Century Skills", 
       y = "Mean Standardized Scores (0-10)",
       fill = "Observation") +
  theme_minimal()

# Save the plot
ggsave("./plots_paper/fig11_21st_competencies.png", height = 8, width = 10, dpi = 300)
ggsave("./plots_paper/fig11_21st_competencies.pdf", height = 8, width = 10, bg = "whitesmoke")
