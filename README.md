# cta-pbl-evaluation
Data, documentation, and R scripts for Evaluating the impact of an educational intervention using PBL

# Project Overview

This repository contains the R code, data, and codebooks for a research study investigating the impact of a project-based learning (PBL) intervention on post-pandemic recovery in rural Colombia. The study aims to assess the effectiveness of PBL in enhancing students' academic performance.

## Repository Structure

* **PBL_data:** Contains anonymized data collected from the study, including pre- and post-intervention assessments and student information (grade and sex).
* **R_code:** Contains the R scripts used for data analysis, visualization, and statistical analyses.
* **PBL_documentation:** Provides descriptions of the variables in the datasets, including variable names, definitions, and measurement scales.

## Data Description

* **pbl_lookup_students.csv:** Contains information about student identifiers, schools, municipalities, grade, and gender.
* **pbl_microdata_21st_century_skills.csv:** Contains data on students' 21st-century skills, as assessed by observers using an observation rubric.
* **pbl_microdata_language_posttest.csv:** Contains student responses to the language posttest.
* **pbl_microdata_language_pretest.csv:** Contains student responses to the language pretest.
* **pbl_microdata_math_posttest.csv:** Contains student responses to the math posttest.
* **pbl_microdata_math_pretest.csv:** Contains student responses to the math pretest.
* **pbl_microdata_science_posttest.csv:** Contains student responses to the science posttest.
* **pbl_microdata_science_pretest.csv:** Contains student responses to the science pretest.

## R Code

* **R_code:** Contains the R scripts used for data preparation, analysis, visualization, and statistical analyses.

## Codebooks

The codebooks folder includes the following files:

* **pbl_codebook_21st_century_skills.pdf:** Provides descriptions of the variables in the `pbl_microdata_21st_century_skills.csv` dataset.
* **pbl_codebook_language_posttest.pdf:** Provides descriptions of the variables in the `pbl_microdata_language_posttest.csv` dataset.
* **pbl_codebook_language_pretest.pdf:** Provides descriptions of the variables in the `pbl_microdata_language_pretest.csv` dataset.
* **pbl_codebook_math_posttest.pdf:** Provides descriptions of the variables in the `pbl_microdata_math_posttest.csv` dataset.
* **pbl_codebook_math_pretest.pdf:** Provides descriptions of the variables in the `pbl_microdata_math_pretest.csv` dataset.
* **pbl_codebook_science_posttest.pdf:** Provides descriptions of the variables in the `pbl_microdata_science_posttest.csv` dataset.
* **pbl_codebook_science_pretest.pdf:** Provides descriptions of the variables in the `pbl_microdata_science_pretest.csv` dataset.

The codebooks were created using the function `Survey logic file` in LimeSurvey

## Usage

1. **Clone the repository:** Use a version control system like Git to clone this repository to your local machine.
2. **Install R packages:** Ensure that all necessary R packages are installed. You can use the `install.packages()` function to install missing packages.
3. **Run R scripts:** Execute the R scripts in the appropriate order to reproduce the data analysis and visualization.

**Notes:**

* Please refer to the codebooks for specific details about the variables and their definitions.
