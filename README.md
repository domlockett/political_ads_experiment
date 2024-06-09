# Political ads project

## Summary
In this project, we investigated social media users’ perceptions of digital political ads. We measured opinions on how platforms should design political ad UX and policies to establish a baseline understanding of user opinions, including the permissibility of political ads and microtargeting, and transparency in funding.

### Research Goals
The primary objective was to understand which ad factors (and user traits) contribute to perceptions of how 'political' digital ads are. We conducted a conjoint experiment with artificial Facebook ads, altering their source, content, and political orientation to isolate the effects on perceptions. A within-between experiment evaluated real ads from the Facebook Ad Library, asking respondents to rate their political perception.

### Findings
Our conjoint analysis supported hypotheses that the source, strength, and orientation of the message matter. Candidate ads were viewed as inherently political, whereas message strength mattered more for ads from companies and advocacy organizations.

## Methods
I joined post-survey implementation, maintaining and overseeing data integrity. I identified a random assignment error, leading to a survey reissuance. I implemented data analysis in R, using libraries such as `dplyr`, `magrittr`, and `tidyverse` for data cleaning, processing, and creating relevant variables.

## Main Contributions
- Collaborated with a multidisciplinary team, providing insights and recommendations based on key findings.
- Conducted exploratory data analysis using data visualization tools such as `ggplot2` and `plotly`.
- Identified a distribution error necessitating a survey reissuance.
- Implemented data transformation techniques including variable recoding, data aggregation, and normalization.
- Presented research findings at the Annual Conference for Political Methodology.


# Detailed Methods

## Data Cleaning and Transformations
### `political_ads_experiment/Publishing materials/rep_cleaning-data.R`
The `rep_cleaning-data.R` script is essential for preparing the dataset and ensuring its quality and consistency.

**Key Tasks:**
- **Variable Recoding:** Standardizes categorical variables, such as political affiliations, for consistency across datasets.
- **Data Aggregation:** Aggregates data at various levels, like individual respondent and ad levels, to facilitate different types of analysis.
- **Normalization:** Uses z-score normalization to scale numerical variables, making them comparable.
- **Missing Data Imputation:** Applies predictive mean matching (PMM) to handle missing data, minimizing bias.

## Statistical Modeling
### `political_ads_experiment/Publishing materials/rep_main-models.R`
This script builds and evaluates statistical models to understand the relationship between ad characteristics and political perceptions.

**Key Tasks:**
- **Model Specification:** Utilizes linear and logistic regression models to analyze data.
- **Model Fitting:** Employs `lm` for linear regression and `glm` for logistic regression, using robust standard errors with the `lm_robust` function from the `estimatr` package to account for heteroscedasticity and improve model reliability.
- **Diagnostic Checks:** Conducts checks for multicollinearity, heteroscedasticity, and influential observations to ensure model validity.
- **Output Presentation:** Uses the `texreg` package to create regression tables, preferred over `stargazer` for its advanced formatting options and ease of integration with LaTeX documents.

## Visualization

![Research Poster](https://github.com/domlockett/political_ads_experiment/blob/main/images/poster.jpg)

### `political_ads_experiment/Publishing materials/rep_main-plots.R`
The `rep_main-plots.R` script creates detailed visualizations to effectively communicate findings.

**Key Tasks:**
- **Result Visualization:** Generates point-range plots, interaction plots, and more using `ggplot2` and `plotly`.
- **Customization:** Enhances plots with labels, legends, and themes for better clarity.
- **Integration:** Calls models from `rep_main-models.R` to ensure consistency in visualizations with statistical analyses.

**Visualizations:**
- **Point-Range Plots:** Illustrate estimated effects and their confidence intervals for various ad characteristics.
- **Interaction Plots:** Show how different variables interact to influence political perceptions.
- **Advanced Techniques:** Uses `facet_wrap` in `ggplot2` for multi-panel plots, `plotly` for interactive visualizations, and `dplyr` for efficient data manipulation.

## Acknowledgments

This project is a collaborative effort involving significant contributions from various scholars. The provided files and scripts reflect the extensive work done to understand and address the impacts of fake news on political behavior. The project was created for academic purposes and contains anonymized data to ensure privacy and confidentiality. The was created for the Washington University in Saint Louis Political Science Department.
