## Science Analysis for 3rd Grade =========

## Libraries ------
library(data.table)
library(tidyverse)
library(openxlsx)

## Load Student Information ------
lookupStudents <- fread('./Raw_data/pbl_lookup_students_science.csv')
glimpse(lookupStudents)

## Baseline Data (Pretest) -----------------
ciencias_pre_raw <- fread("./Raw_data/pbl_microdata_science_pretest.csv", encoding = "UTF-8")
unique(ciencias_pre_raw, by = 'studentid')

# Select relevant columns (studentid and Q001-Q020)
cols.pre <- grep("studentid|^Q00[1-9]$|^Q01[0-9]$|^Q020$", colnames(ciencias_pre_raw), value = TRUE)
ciencias_pre <- ciencias_pre_raw[!is.na(studentid), .SD, .SDcols = cols.pre]
ciencias_pre[, medicion := 'L. Base']  # Label for baseline test

glimpse(ciencias_pre)

## Recoding responses based on correct answers ------
ciencias_pre[, `:=`(Q001 = ifelse(Q001 %in% "C", 1, 0),
                    Q002 = ifelse(Q002 %in% "D", 2, 0),
                    Q003 = ifelse(Q003 %in% "A", 3, 0),
                    Q004 = ifelse(Q004 %in% "C", 2, 0),
                    Q005 = ifelse(Q005 %in% "B", 1, 0),
                    Q006 = ifelse(Q006 %in% "A", 2, 0),
                    Q007 = ifelse(Q007 %in% "B", 1, 0),
                    Q008 = ifelse(Q008 %in% "C", 2, 0),
                    Q009 = ifelse(Q009 %in% "D", 3, 0),
                    Q010 = ifelse(Q010 %in% "C", 2, 0),
                    Q011 = ifelse(Q011 %in% "D", 3, 0),
                    Q012 = ifelse(Q012 %in% "A", 1, 0),
                    Q013 = ifelse(Q013 %in% "C", 3, 0),
                    Q014 = ifelse(Q014 %in% "B", 2, 0),
                    Q015 = ifelse(Q015 %in% "C", 2, 0),
                    Q016 = ifelse(Q016 %in% "D", 2, 0),
                    Q017 = ifelse(Q017 %in% "B", 1, 0),
                    Q018 = ifelse(Q018 %in% "A", 2, 0),
                    Q019 = ifelse(Q019 %in% "C", 2, 0),
                    Q020 = ifelse(Q020 %in% "D", 2, 0))]

## Calculate total scores based on complexity level ------
# Recall
rec_cols <- c('Q001', 'Q005', 'Q007', 'Q012', 'Q017')
ciencias_pre[, total_rec := rowSums(.SD), .SDcols = rec_cols]

# Application
app_cols <- c('Q002', 'Q004', 'Q006', 'Q008', 'Q010', 'Q014', 'Q015', 'Q016', 'Q018', 'Q019', 'Q020')
ciencias_pre[, total_app := rowSums(.SD), .SDcols = app_cols]

# Strategic Thinking
estra_cols <- c('Q003', 'Q009', 'Q011', 'Q013')
ciencias_pre[, total_estr := rowSums(.SD), .SDcols = estra_cols]

## Performance Level Classification ------
ciencias_pre[, desempeno := ifelse(total_rec < 3, "Beginning",
                                   ifelse(total_rec > 2 & total_app < 12, "In Progress",
                                          ifelse(total_rec > 2 & total_app > 11 & total_estr < 7, "Achieved",
                                                 ifelse(total_rec > 2 & total_app > 11 & total_estr > 6, "Extended", "Beginning"))))]

## Posttest Data -----------------
ciencias_post_raw <- fread("./Raw_data/pbl_microdata_science_posttest.csv", encoding = 'UTF-8')
unique(ciencias_post_raw, by = 'studentid')

# Select relevant columns for posttest
cols.post <- grep("studentid|^Q00[1-9]$|^Q01[0-9]$|Q020$", colnames(ciencias_post_raw), value = TRUE)
ciencias_post <- ciencias_post_raw[!is.na(studentid), .SD, .SDcols = cols.post]
ciencias_post[, medicion := "L. Salida"]

glimpse(ciencias_post)

## Recoding responses for posttest based on correct answers ------
ciencias_post[, `:=`(Q001 = ifelse(Q001 %in% "A", 1, 0),
                     Q002 = ifelse(Q002 %in% "B", 1, 0),
                     Q003 = ifelse(Q003 %in% "B", 1, 0),
                     Q004 = ifelse(Q004 %in% "B", 1, 0),
                     Q005 = ifelse(Q005 %in% "B", 1, 0),
                     Q006 = ifelse(Q006 %in% "C", 2, 0),
                     Q007 = ifelse(Q007 %in% "D", 2, 0),
                     Q008 = ifelse(Q008 %in% "A", 2, 0),
                     Q009 = ifelse(Q009 %in% "D", 2, 0),
                     Q010 = ifelse(Q010 %in% "B", 2, 0),
                     Q011 = ifelse(Q011 %in% "A", 2, 0),
                     Q012 = ifelse(Q012 %in% "C", 1, 0),
                     Q013 = ifelse(Q013 %in% "A", 2, 0),
                     Q014 = ifelse(Q014 %in% "C", 2, 0),
                     Q015 = ifelse(Q015 %in% "A", 2, 0),
                     Q016 = ifelse(Q016 %in% "B", 3, 0),
                     Q017 = ifelse(Q017 %in% "A", 3, 0),
                     Q018 = ifelse(Q018 %in% "A", 3, 0),
                     Q019 = ifelse(Q019 %in% "D", 2, 0),
                     Q020 = ifelse(Q020 %in% "A", 3, 0))]

## Calculate total scores for posttest ------
# Recall
rec_cols <- c('Q001', 'Q002', 'Q003', 'Q004', 'Q005', 'Q012')
ciencias_post[, total_rec := rowSums(.SD), .SDcols = rec_cols]

# Application
app_cols <- c('Q006', 'Q007', 'Q008', 'Q009', 'Q010', 'Q011', 'Q013', 'Q014', 'Q015', 'Q019')
ciencias_post[, total_app := rowSums(.SD), .SDcols = app_cols]

# Strategic Thinking
estra_cols <- c('Q016', 'Q017', 'Q018', 'Q020')
ciencias_post[, total_estr := rowSums(.SD), .SDcols = estra_cols]

## Performance Level Classification for posttest ------
ciencias_post[, desempeno := ifelse(total_rec < 4, "Beginning",
                                    ifelse(total_rec > 3 & total_app < 11, "In Progress",
                                           ifelse(total_rec > 3 & total_app > 10 & total_estr < 7, "Achieved",
                                                  ifelse(total_rec > 3 & total_app > 10 & total_estr > 6, "Extended", "Beginning"))))]

## Pretest and Posttest Data Combined ----------------------
l.ciencias <- list(ciencias_pre, ciencias_post)
bind.prepost <- rbindlist(l.ciencias, use.names = TRUE, fill = TRUE)

## Select students with both pretest and posttest ------
ciencias3.PrePost <- bind.prepost[, if (.N > 1) .SD, by = studentid]
ciencias3.PrePost[, area := 'Ciencias']

## Add gender information ------
ciencias3.PrePost[lookupStudents, sexo := i.sexo, on = 'studentid']

## Calculate global score on T-scale (mean = 50, SD = 10) ------
ciencias3.PrePost[, global_raw := rowSums(.SD), .SDcols = c('total_rec', 'total_app', 'total_estr')]
z.fn <- function(x) scale(x, center = TRUE, scale = TRUE)  # Function to standardize `global_raw`
ciencias3.PrePost[, global_z := lapply(.SD, z.fn), .SDcols = 'global_raw']
ciencias3.PrePost[, global_scoreT := round(global_z * 10 + 50)]

## Performance levels based on T-score --------
ciencias3.PrePost[, performance_level := fifelse(global_scoreT < quantile(global_scoreT, 0.25), "Beginning",
                                                 fifelse(global_scoreT < quantile(global_scoreT, 0.50), "In Progress",
                                                         
                                                         
                                                         fifelse(global_scoreT < quantile(global_scoreT, 0.75), "Achieved", "Extended")))]

## Save processed data ------
write.xlsx(ciencias3.PrePost, './Processed_data/ciencias-prepost3.xlsx')
saveRDS(ciencias3.PrePost, './Processed_data/ciencias-prepost3.rds')
write_csv(ciencias3.PrePost, './Processed_data/ciencias-prepost3.csv')

ciencias3.PrePost[, .N, by = 'medicion']


## Create wide dataset for JASP ------
wide_ciencias3 <- dcast(ciencias3.PrePost, studentid + area + sexo ~ medicion, value.var = "global_scoreT")
write.csv(wide_ciencias3, './Processed_data/ciencias_wide.csv')

## Line Chart ------
# Prepare data for plotting
ciencias3.PrePost$medicion <- factor(ciencias3.PrePost$medicion, levels = c("L. Base", "L. Salida"), labels = c("Pretest", "Posttest"))
ciencias3.PrePost$sexo <- factor(ciencias3.PrePost$sexo, levels = c("F", "M"), labels = c("Female students", "Male students"))

# Summarize data by gender and test condition
data_summary <- ciencias3.PrePost %>%
  filter(!is.na(sexo)) %>%
  group_by(medicion, sexo) %>%
  summarise(mean_score = mean(global_scoreT, na.rm = TRUE))

# Plot the summarized data
ggplot(data_summary, aes(x = medicion, y = mean_score, group = sexo, color = sexo)) +
  geom_line(size = 1) +
  geom_point(shape = 21, fill = "white", size = 3) +
  labs(x = "Test", y = "Science Score", color = "Gender") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "whitesmoke", color = NA),
        plot.background = element_rect(fill = "whitesmoke", color = NA),
        legend.background = element_rect(fill = "whitesmoke", color = NA),
        legend.key = element_rect(fill = "whitesmoke", color = NA))

ggsave('./plots_paper/fig6_science_prepost.png', width = 8, height = 6, dpi = 300)
ggsave('./plots_paper/fig6_science_prepost.pdf', width = 8, height = 6)

## Paired t-test ------
# Extract pretest and posttest scores
paired_scores <- merge(ciencias3.PrePost[medicion == "Pretest", .(studentid, pretest = global_scoreT)],
                       ciencias3.PrePost[medicion == "Posttest", .(studentid, posttest = global_scoreT)],
                       by = "studentid")

# Perform paired t-test
res_paired_ttest <- t.test(paired_scores$pretest, paired_scores$posttest, paired = TRUE)
res_paired_ttest

## Wilcoxon test ------
res <- wilcox.test(global_scoreT ~ medicion, data = ciencias3.PrePost, exact = FALSE)
res

## Repeated measures ANOVA ------
science.cols <- ciencias3.PrePost[lookupStudents[, .(sexo, grado, studentid)], on = 'studentid', nomatch = 0L]
science.cols <- science.cols[, .SD, .SDcols = c("studentid", "sexo", "medicion", "area", "global_scoreT")]

## Repeated measures ANOVA ------

# Ensure 'sexo' and 'medicion' are treated as factors and check their levels
science.cols[, sexo := as.factor(sexo)]
science.cols[, medicion := as.factor(medicion)]

# Check for unique levels in 'sexo' and 'medicion'
if (length(unique(science.cols$sexo)) < 2) {
  stop("The 'sexo' variable must have at least two levels.")
}
if (length(unique(science.cols$medicion)) < 2) {
  stop("The 'medicion' variable must have at least two levels.")
}

# Perform repeated measures ANOVA
aov_result_sexo <- aov(global_scoreT ~ sexo * medicion + Error(studentid / medicion), data = science.cols)

# Print the ANOVA summary
summary(aov_result_sexo)


# Calculate mean and SD by gender and test condition
science.cols[, c('medicion', 'sexo') := .(factor(medicion), factor(sexo))]
result_table <- science.cols[, .(Mean = round(mean(global_scoreT), 1), SD = round(sd(global_scoreT), 1)), by = .(medicion, sexo)]
print(result_table)

## Plot general performance levels ------
# Reorganize levels for `performance_level`
ciencias3.PrePost$performance_level <- factor(ciencias3.PrePost$performance_level, levels = c('Extended', 'Achieved', 'In Progress', 'Beginning'))

# Color palette for levels
cols <- c("Beginning" = "firebrick3", "In Progress" = "gold1", "Achieved" = "forestgreen", "Extended" = "dodgerblue1")

# Summarize data by performance level and test condition
summ.general <- ciencias3.PrePost[ , .N, by = .(medicion, performance_level)][, perc := N / sum(N), by = 'medicion']

# Plot performance levels
ggplot(summ.general, aes(x = factor(performance_level), perc, fill = factor(performance_level))) +
  geom_bar(stat = "identity", position = 'dodge', show.legend = FALSE) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1L)), vjust = -0.5) +
  labs(x = "Science Competency Level", y = "% Students", fill = "Level") +
  scale_fill_manual(values = cols, limits = c('Extended', 'Achieved', 'In Progress', 'Beginning'),
                    labels = c('Extended', 'Achieved', 'In Progress', 'Beginning')) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(medicion)) +
  theme(axis.title.x = element_text(margin = margin(t = 10)))

ggsave("./plots_paper/fig9_science_competency.png", height = 6, width = 8, dpi = 300)
ggsave("./plots_paper/fig9_science_competency.pdf", height = 6, width = 8)
