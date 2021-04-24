# DSC-499-Capstone
## Project Title: Detecting Medical Errors in the Cancer Screening Process: An In-Depth Review of Lung Cancer Screening and At-Risk Qualifications
### Author: Brianna Johnson
### Date: 24 April 2021

## Project Description
Globally, 1.3% of all individuals will receive a cancer diagnosis at some point in their lives. That number jumps to 5.5% in the United States alone, accounting for over 600,000 lives being lost each year. Lung Cancer specifically, is the leading cause of death among cancers, above colon, breast, and prostate cancers combined. Current screening protocols only test smokers and former smokers who have quit within the past 15 years. However, nearly 20% of all Lung Cancer cases are being found in those who have never smoked. This investigation aims to look at the environmental and medical history factors that could lead to diagnosing Lung Cancer when smoking is not the cause. Machine Learning Classification methods like Logistic Regression and Classification and Regression Trees will be used to pinpoint which nonsmokers should be screened. These results will then be tested to analyze the predicted lifespan achieved through these implemented screening protocols. 

## Dataset
The Lung Cancer Dataset used throughout this report was contributed through the National Cancer Institute's Cancer Data Access System. Access to the PLCO Lung Dataset was requested on February 18, 2021 and approved on February 22, 2021 for use until March 9, 2024. An overview of the dataset, including access to the Data Dictionaries can be viewed here: https://cdas.cancer.gov/datasets/plco/21/

## Results Obtained
Apart from Exploratory Data Analysis, Machine Learning Classification Methods were primarily used to identify the most influential factors in an individual developing Lung Cancer, apart from Smoking. The Pruned Classification and Regression Tree Model slightly outperformed the Logistic Regression, with an accuracy of 91.5%. The CART Model also helps serve as a more realistic determination that would be used in the Medical Field, as it consists of 5 layers of yes or no questions. It is important to note that Sensitivity was low, as this model was very cautious of not producing high False Positive Rates, due to the unnecessary exposure to Radiation through yearly CT Scans. However, the True Positive Rate, of 14.2% closely resembles the current rate of Lung Cancer found in Smokers, which as of 2020 is 15%.

## Programming Language and Version
R Programming version 4.0.4 was used in this research
