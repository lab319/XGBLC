Baoshan Ma, Ge Yan, Bingjie Chai and Xiaoyu Hou. XGBLC: An Improved Survival Prediction Model Based on XGBoost (Under review)

This repository contains python and R implementation of the algorithms proposed in "XGBLC: An Improved Survival Prediction Model Based on XGBoost".


## The version of Python, R and packages

Python version=3.8

R version=4.0.3

survivalsvm version=0.0.5
glmnet version=4.0-2
xgboost version=1.2.0.1
randomForestSRC version=2.9.3


## The datasets of the program

The data used in this research are collected from The Cancer Genome Atlas (TCGA) project and that are publicly available at https://portal.gdc.cancer.gov.
The describe of the program


## The program is divided into three sections saved in this repository.

1) Data preprocessing: preprocessed data can be used to test the models, the programs can directly process the data downloaded by TCGA.

2) Other_machine_learning_models: several popular survival analysis algorithms are compared with XGBLC. The files in the repository can be used to construct different predictive models to analyze the patients' survival of 20 cancer datasets.

3) XGBLC_model: survival prediction model based on XGBLC was presented to analyze the patients' survival of 20 cancer datasets patients.
