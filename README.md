# Heart-Failure-Prediction
Dataset is from https://www.kaggle.com/fedesoriano/heart-failure-prediction. Contains EDA and a binary logistic regression model.

Clicking on the .rm file should load the r-markdown as a complete scrollable web page within github.

The project initially explores the dataset and investigates correlations among variables, then moves onto building and refining a GLM to predict the probability of Heart Disease. Using an ROC curve and maximising the AUC made a threshold value of 0.456, and maintains a high level of sensitivity and specificity with a stong accuracy of 87.4%. ggplot is used throughout for plotting and the modelling is done with base R lm and glm functions.
