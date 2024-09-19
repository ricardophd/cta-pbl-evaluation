## Analysis for Mathematics Grade 3 =========

## Libraries ------

library(data.table)
library(tidyverse)
library(openxlsx)
library(xtable)
library(officer)
library(flextable)
library(svglite)

## Student Information ------
## Always update the lookup of students if baseline data is updated.

lookupStudents <- fread('./Raw_data/pbl_lookup_students_math.csv')
glimpse(lookupStudents)

## Baseline Data ------

math3_pre_raw <- fread("./Raw_data/pbl_microdata_math_pretest.csv", encoding = "UTF-8")
unique(math3_pre_raw, by = 'studentid')

total_count <- nrow(math3_pre_raw)

# Select relevant columns for baseline data
cols.pre <- grep("studentid|^Q00[1-9]|^Q01[0-2]", colnames(math3_pre_raw), value = TRUE)

# Subset the baseline data
math3_pre <- math3_pre_raw[!is.na(studentid), .SD, .SDcols = cols.pre]
math3_pre[, medicion := 'L. Base']

glimpse(math3_pre)

## Recoding answers based on the correct answer key ------
math3_pre[, `:=`(Q001 = ifelse(Q001 %in% "A", 3, 0),
                 Q002 = ifelse(Q002 %in% "A", 2, 0),
                 Q003 = ifelse(Q003 %in% "A", 2, 0),
                 Q004 = ifelse(Q004 %in% "C", 2, 0),
                 Q005 = ifelse(Q005 %in% "B", 2, 0),
                 Q006 = ifelse(Q006 %in% "C", 1, 0),
                 Q007 = ifelse(Q007 %in% "B", 2, 0),
                 Q008 = ifelse(Q008 %in% "B", 1, 0),
                 Q009 = ifelse(Q009 %in% "C", 1, 0),
                 Q010 = ifelse(Q010 %in% "B", 1, 0),
                 Q011 = ifelse(Q011 %in% "A", 3, 0),
                 Q012 = ifelse(Q012 %in% "A", 2, 0))]

## Total score by level of complexity ------
# Recall
rec_cols <- c('Q006', 'Q008', 'Q009', 'Q010')
math3_pre[, total_rec := rowSums(.SD), .SDcols = rec_cols]

# Application
app_cols <- c('Q002', 'Q003', 'Q004', 'Q005', 'Q007', 'Q012')
math3_pre[, total_app := rowSums(.SD), .SDcols = app_cols]

# Strategic thinking
estra_cols <- c('Q001', 'Q011')
math3_pre[, total_estr := rowSums(.SD), .SDcols = estra_cols]

## Posttest Data ------

math3_post_raw <- fread("./Raw_data/pbl_microdata_math_posttest.csv", encoding = 'UTF-8')
unique(math3_post_raw, by = 'studentid')

# Select relevant columns for posttest data
cols.post <- grep("studentid|^Q00[1-9]|^Q01[0-2]", colnames(math3_post_raw), value = TRUE)

# Subset the posttest data
math3_post <- math3_post_raw[!is.na(studentid), .SD, .SDcols = cols.post]
math3_post[, medicion := "L. Salida"]

glimpse(math3_post)

## Recoding answers based on the correct answer key ------
math3_post[, `:=`(Q001 = ifelse(Q001 %in% "A", 1, 0),
                  Q002 = ifelse(Q002 %in% "A", 3, 0),
                  Q003 = ifelse(Q003 %in% "A", 2, 0),
                  Q004 = ifelse(Q004 %in% "D", 1, 0),
                  Q005 = ifelse(Q005 %in% "A", 1, 0),
                  Q006 = ifelse(Q006 %in% "A", 3, 0),
                  Q007 = ifelse(Q007 %in% "A", 2, 0),
                  Q008 = ifelse(Q008 %in% "D", 2, 0),
                  Q009 = ifelse(Q009 %in% "C", 2, 0),
                  Q010 = ifelse(Q010 %in% "B", 1, 0),
                  Q011 = ifelse(Q011 %in% "A", 2, 0),
                  Q012 = ifelse(Q012 %in% "C", 2, 0))]

## Total score by level of complexity ------
# Recall
rec_cols <- c('Q001', 'Q004', 'Q005', 'Q010')
math3_post[, total_rec := rowSums(.SD), .SDcols = rec_cols]

# Application
app_cols <- c('Q003', 'Q007', 'Q008', 'Q009', 'Q011', 'Q012')
math3_post[, total_app := rowSums(.SD), .SDcols = app_cols]

# Strategic thinking
estra_cols <- c('Q002', 'Q006')
math3_post[, total_estr := rowSums(.SD), .SDcols = estra_cols]

## Pre-Post Data for Mathematics Grade 3 ------

# Merge baseline and posttest data
l.math3 <- list(math3_pre, math3_post)
math3_pre[math3_post, on = 'studentid', nomatch = NULL]
bind.prepost <- rbindlist(l.math3, use.names = TRUE, fill = TRUE)

# Select records of students who took both tests
math3.PrePost <- bind.prepost[, if (.N > 1) .SD, by = studentid]

math3.PrePost[, area := 'Matemáticas']
math3.PrePost[lookupStudents, sexo := i.sexo, on = 'studentid']
math3.PrePost[, .N, by = 'medicion']

## Calculation of Global Score (T-scale: mean 50, sd 10) ------

math3.PrePost[, global_raw := rowSums(.SD), .SDcols = c('total_rec', 'total_app', 'total_estr')]

# Function to standardize `global_raw`
z.fn <- function(x) scale(x, center = TRUE, scale = TRUE)

math3.PrePost[, global_z := lapply(.SD, z.fn), .SDcols = 'global_raw']
math3.PrePost[, global_scoreT := round(global_z * 10 + 50)]

mean(math3.PrePost$global_scoreT)
sd(math3.PrePost$global_scoreT)

## Performance Levels (Based on T-Score) ------
# Define thresholds for performance levels
math3.PrePost[, performance_level := 
                fifelse(global_scoreT < quantile(global_scoreT, 0.25), "Beginning",
                        fifelse(global_scoreT < quantile(global_scoreT, 0.50), "In Progress",
                                fifelse(global_scoreT < quantile(global_scoreT, 0.75), "Achieved", "Extended")))]

# Save the results
write.xlsx(math3.PrePost, './Processed_data/math-prepost3.xlsx')
saveRDS(math3.PrePost, './Processed_data/math-prepost3.rds')
write.csv(math3.PrePost, './Processed_data/math-prepost3.csv')

## Wide Dataset for JASP ------

wide_math3 <- dcast(math3.PrePost, studentid + area + sexo ~ medicion, value.var = "global_scoreT")
write.csv(wide_math3, './Processed_data/math_wide.csv')

## Plots ------

ggplot(data_summary, aes(x = medicion, y = mean_score, group = sexo, color = sexo)) +
  geom_line(size = 1) +
  geom_point(shape = 21, fill = "white", size = 3) +
  labs(x = "Test", y = "Math Score", color = "Gender") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "whitesmoke", color = NA),
        legend.background = element_rect(fill = "whitesmoke", color = NA),
        legend.key = element_rect(fill = "whitesmoke", color = NA))

ggsave('./plots_paper/fig5_math_prepost.png', width = 8, height = 6, dpi = 300)
ggsave('./plots_paper/fig5_math_prepost.pdf', width = 8, height = 6)

## Statistical Tests ------

## Paired t-test ------
# Subset the data for pretest and posttest scores
pretest_scores <- math3.PrePost[medicion == "L. Base", global_scoreT]
posttest_scores <- math3.PrePost[medicion == "L. Salida", global_scoreT]

# Ensure that both vectors have the same length (i.e., only students who have both pretest and posttest scores)
paired_scores <- merge(
  math3.PrePost[medicion == "L. Base", .(studentid, pretest = global_scoreT)],
  math3.PrePost[medicion == "L. Salida", .(studentid, posttest = global_scoreT)],
  by = "studentid"
)

# Perform the paired t-test
res_paired_ttest <- t.test(paired_scores$pretest, paired_scores$posttest, paired = TRUE)

# Print the result
res_paired_ttest

## Wilcoxon ------------

res <- wilcox.test(global_scoreT ~ medicion, data = math3.PrePost, exact = FALSE)
res

## Perform Repeated Measures ANOVA to test score changes by gender ------
math.cols <- math3.PrePost[lookupStudents[, .(sexo, grado, studentid)], on = 'studentid', nomatch = 0L]
math.cols <- math.cols[, .SD, .SDcols = c("studentid", "sexo", "medicion", "area", "global_scoreT")]

aov_result_sexo <- aov(global_scoreT ~ sexo * medicion + Error(studentid / medicion), data = math.cols)
summary(aov_result_sexo)

# Calculate mean and SD by test condition and gender
math.cols[, c('medicion', 'sexo') := .(factor(medicion), factor(sexo))]
result_table <- math.cols[, .(Mean = round(mean(global_scoreT), 1), SD = round(sd(global_scoreT), 1)), by = .(medicion, sexo)]
print(result_table)

## Plot General Performance with T-Scale ------
math3.PrePost$performance_level <- factor(math3.PrePost$performance_level, levels = c('Extended', 'Achieved', 'In Progress', 'Beginning'))
cols <- c("Beginning" = "firebrick3", "In Progress" = "gold1", "Achieved" = "forestgreen", "Extended" = "dodgerblue1")

# Summarize data
summ.general <- math3.PrePost[ , .N, by = .(medicion, performance_level)][, perc := N / sum(N), by = 'medicion']

# Create plot with custom facet labels
ggplot(summ.general, aes(x = factor(performance_level), perc, fill = factor(performance_level))) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = FALSE) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1L)), vjust = -0.5) +
  labs(x = "Mathematics Competency Level", y = "% Students", fill = "Level") +
  scale_fill_manual(values = cols, limits = c('Extended', 'Achieved', 'In Progress', 'Beginning'), labels = c('Extended', 'Achieved', 'In Progress', 'Beginning')) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(medicion), labeller = as_labeller(c("L. Base" = "Pretest", "L. Salida" = "Posttest"))) +
  theme(axis.title.x = element_text(margin = margin(t = 10)))

# Save plot
ggsave("./plots_paper/fig8_math_competency.png", height = 6, width = 8, dpi = 300)
ggsave("./plots_paper/fig8_math_competency.pdf", height = 6, width = 8)
