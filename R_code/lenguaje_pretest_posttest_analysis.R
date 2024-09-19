
## Analysis Language Grade 3 =========

## Libraries ------
library(data.table)
library(tidyverse)
library(openxlsx)

## Student Information ------
lookupStudentsLg <- fread('./Raw_data/pbl_lookup_students_language.csv')
glimpse(lookupStudents)

## Baseline Data -----------------
lengua_pre_raw <- fread("./Raw_data/pbl_microdata_language_pretest.csv", encoding = "UTF-8")
unique(lengua_pre_raw, by = 'studentid')

# Select columns for pretest
cols.pre <- grep("studentid|^Q00[1-9]|^Q01[0-2]", colnames(lengua_pre_raw), value = TRUE)
lengua_pre <- lengua_pre_raw[!is.na(studentid), .SD, .SDcols = cols.pre]
lengua_pre[, medicion := 'Pretest']

glimpse(lengua_pre)

## Recode Variables for Pretest ---------
lengua_pre[, `:=`(Q001 = ifelse(Q001 %in% "A", 1, 0),
                  Q002 = ifelse(Q002 %in% "C", 1, 0),
                  Q003 = ifelse(Q003 %in% "B", 1, 0),
                  Q004 = ifelse(Q004 %in% "A", 2, 0),
                  Q005 = ifelse(Q005 %in% "A", 2, 0),
                  Q006 = ifelse(Q006 %in% "A", 2, 0),
                  Q007 = ifelse(Q007 %in% "A", 1, 0),
                  Q008 = ifelse(Q008 %in% "A", 2, 0),
                  Q009 = ifelse(Q009 %in% "A", 3, 0),
                  Q010 = ifelse(Q010 %in% "A", 2, 0),
                  Q011 = ifelse(Q011 %in% "B", 2, 0),
                  Q012 = ifelse(Q012 %in% "A", 3, 0))]

## Total Score by Complexity Level ---------
# Recall
rec_cols <- c('Q001', 'Q002', 'Q003', 'Q007')
lengua_pre[, total_rec := rowSums(.SD), .SDcols = rec_cols]

# Application
app_cols <- c('Q004', 'Q005', 'Q006', 'Q008', 'Q010', 'Q011')
lengua_pre[, total_app := rowSums(.SD), .SDcols = app_cols]

# Strategic Thinking
estra_cols <- c('Q009', 'Q012')
lengua_pre[, total_estr := rowSums(.SD), .SDcols = estra_cols]

# Performance Level
lengua_pre[, `:=`(desempeno = ifelse(total_rec < 3, "Beginning",
                                     ifelse(total_rec > 2 & total_app < 7, "In Progress",
                                            ifelse(total_rec > 2 & total_app > 6 & total_estr < 4, "Achieved",
                                                   ifelse(total_rec > 2 & total_app > 6 & total_estr > 3, "Extended", "Beginning")))))]

lengua_pre[, .N, by = desempeno]

## Posttest Data ---------------
lengua_post_raw <- fread("resultados_salida_lenguaje.csv", encoding = 'UTF-8')

# Recode Posttest Data
lengua_post_raw[, `:=`(Q004 = case_when(Q004 == 'A' ~ 'A',
                                        Q004 == 'B' ~ 'A',
                                        TRUE ~ '99'),
                       Q005 = case_when(Q005 == 'A' ~ 'A',
                                        Q005 == 'B' ~ 'A',
                                        TRUE ~ '99'),
                       Q010 = case_when(Q010 == 'A' ~ 'A',
                                        Q010 == 'B' ~ 'A',
                                        TRUE ~ '99'))]

# Process Multiple Choice Responses for Q007
lengua_post_raw[, Q007 := ifelse(`Q007[SQ001_SQ001]` == 1 & `Q007[SQ002_SQ001]` == 1 | 
                                   `Q007[SQ001_SQ001]` == 1 & `Q007[SQ003_SQ001]` == 1 | 
                                   `Q007[SQ002_SQ001]` == 1 & `Q007[SQ003_SQ001]` == 1 , 'A', NA)]

unique(lengua_post_raw, by = 'studentid')

# Select Columns for Posttest
cols.post <- grep("studentid|^Q00[1-9]$|^Q01[0-2]$", colnames(lengua_post_raw), value = TRUE)
lengua_post <- lengua_post_raw[!is.na(studentid), .SD, .SDcols = cols.post]
lengua_post[, medicion := "Posttest"]
glimpse(lengua_post)

## Recode Responses for Posttest --------
lengua_post[, `:=`(Q001 = ifelse(Q001 %in% "B", 1, 0),
                   Q002 = ifelse(Q002 %in% "C", 1, 0),
                   Q004 = ifelse(Q004 %in% "A", 2, 0),
                   Q005 = ifelse(Q005 %in% "A", 2, 0),
                   Q006 = ifelse(Q006 %in% "A", 2, 0),
                   Q007 = ifelse(Q007 %in% "A", 1, 0),
                   Q008 = ifelse(Q008 %in% "A", 2, 0),
                   Q009 = ifelse(Q009 %in% "A", 3, 0),
                   Q010 = ifelse(Q010 %in% "A", 2, 0),
                   Q011 = ifelse(Q011 %in% "A", 2, 0),
                   Q012 = ifelse(Q012 %in% "A", 3, 0))]

# Total Score by Complexity Level

# Recall
rec_cols <- c('Q001', 'Q002', 'Q011')
lengua_post[, total_rec := rowSums(.SD), .SDcols = rec_cols]

# Application
app_cols <- c('Q004', 'Q005', 'Q006', 'Q007', 'Q010', 'Q012')
lengua_post[, total_app := rowSums(.SD), .SDcols = app_cols]

# Strategic Thinking
estra_cols <- c('Q008', 'Q009')
lengua_post[, total_estr := rowSums(.SD), .SDcols = estra_cols]

# Performance Level
lengua_post[, `:=`(desempeno = ifelse(total_rec < 2, "Beginning",
                                      ifelse(total_rec > 1 & total_app < 7, "In Progress",
                                             ifelse(total_rec > 1 & total_app > 6 & total_estr < 4, "Achieved",
                                                    ifelse(total_rec > 1 & total_app > 6 & total_estr > 3, "Extended", "Beginning")))))]

lengua_post[, .N, by = desempeno]

## Combined Pretest and Posttest Data for Language Grade 3 ----------------------

l.lengua <- list(lengua_pre, lengua_post)

# Combine the Datasets into One
bind.prepost <- rbindlist(l.lengua, use.names = TRUE, fill = TRUE)

# Select Students with Both Pretest and Posttest Results
lengua3.PrePost <- bind.prepost[, if (.N > 1) .SD, by = studentid]
lengua3.PrePost[, area := 'Lenguaje']

## Add Gender Information to the Dataset ------
lengua3.PrePost[lookupStudentsLg, sexo := i.sexo, on = 'studentid']

## Calculate Overall Global Score (T scale, mean 50, SD 10) --------------
lengua3.PrePost[, global_raw := rowSums(.SD), .SDcols = c('total_rec', 'total_app', 'total_estr')]

# Function for Standardization
z.fn <- function(x) scale(x, center = TRUE, scale = TRUE)
lengua3.PrePost[, global_z := lapply(.SD, z.fn), .SDcols = 'global_raw']

# Calculate T Scale Score
lengua3.PrePost[, global_scoreT := ceiling(global_z * 10 + 50)]

## Performance Levels Based on T-Score --------
lengua3.PrePost[, performance_level := 
                  fifelse(global_scoreT < quantile(global_scoreT, 0.25), "Beginning",
                          fifelse(global_scoreT < quantile(global_scoreT, 0.50), "In Progress",
                                  fifelse(global_scoreT < quantile(global_scoreT, 0.75), "Achieved", "Extended")))]

# Save Results
write.xlsx(lengua3.PrePost, './Processed_data/leng-prepost3.xlsx')
write.csv(lengua3.PrePost, './Processed_data/leng-prepost3-rev.csv')
saveRDS(lengua3.PrePost, './Processed_data/leng-prepost3.rds')

lengua3.PrePost[, .N, by = 'medicion']

## Wide Dataset for JASP ------
wide_lengua3 <- dcast(lengua3.PrePost, studentid + area + sexo ~ medicion, value.var = "global_scoreT")
write.csv(wide_lengua3, './Processed_data/lenguaje_wide.csv')

## Plots ------
# Line Chart for Pretest vs Posttest

ggplot(data_summary, aes(x = medicion, y = mean_score, group = sexo, color = sexo)) +
  geom_line(size = 1) +
  geom_point(shape = 21, fill = "white", size = 3) +
  labs(x = "Test", y = "Language Score", color = "Gender") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "whitesmoke", color = NA),
        legend.background = element_rect(fill = "whitesmoke", color = NA),
        legend.key = element_rect(fill = "whitesmoke", color = NA))

ggsave('./plots_paper/fig4_language_prepost.png', width = 8, height = 6, dpi = 300)
ggsave('./plots_paper/fig4_language_prepost.pdf', width = 8, height = 6)

## Paired T-Test ------
paired_scores <- merge(lengua3.PrePost[medicion == "Pretest", .(studentid, pretest = global_scoreT)],
                       lengua3.PrePost[medicion == "Posttest", .(studentid, posttest = global_scoreT)],
                       by = "studentid")

# Perform Paired T-Test
res_paired_ttest <- t.test(paired_scores$pretest, paired_scores$posttest, paired = TRUE)
res_paired_ttest

## Wilcoxon Test ------
res <- wilcox.test(global_scoreT ~ medicion, data = lengua3.PrePost, exact = FALSE)
res

## ANOVA for Gender and Test Condition ------
leng.cols <- lengua3.PrePost[lookupStudentsLg[, .(sexo, grado, studentid)], on = 'studentid', nomatch = 0L]
aov_result_sexo <- aov(global_scoreT ~ sexo * medicion + Error(studentid / medicion), data = leng.cols)
summary(aov_result_sexo)

## Plot General Performance with T-Scale -------------
lengua3.PrePost$medicion <- factor(lengua3.PrePost$medicion, levels = c("Pretest", "Posttest"), labels = c("Pretest", "Posttest"))

# Reorganize Factors in `performance_level` Variable
lengua3.PrePost$performance_level <- factor(lengua3.PrePost$performance_level, levels=c('Extended', 'Achieved','In Progress','Beginning'))

cols <- c("Beginning" = "firebrick3","In Progress" = "gold1","Achieved" = "forestgreen", "Extended" = "dodgerblue1")

summ.general <- lengua3.PrePost[ , .N, by = .(medicion, performance_level)][, perc := N/sum(N), by = 'medicion']

ggplot(summ.general, aes(x = factor(performance_level), perc, fill = factor(performance_level))) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = FALSE) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1L)), vjust = -0.5) +
  labs(x = "Language Competency Level", y = "% Students", fill = "Level") +
  scale_fill_manual(values = cols,
                    limits = c('Extended', 'Achieved', 'In Progress', 'Beginning'),
                    labels = c('Extended', 'Achieved', 'In Progress', 'Beginning')) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(medicion)) +
  theme(axis.title.x = element_text(margin = margin(t = 10)))

ggsave("./plots_paper/fig7_language_competency.png", height = 6, width = 8, dpi = 300)
ggsave("./plots_paper/fig7_language_competency.pdf", height = 6, width = 8)
