# Dissertation-horse-racing
This repositories include the code for the horse racing prediction models. 

## DATA
'Features_2013-2020.csv' is the dataset with basic features, which can be used to test the ICA data in 'features_extraction_ICA_and_win_rate.ipynb'. 
'Features_2013-2020_ML.csv' is the dataset with all features that prepared to train the models.

## FUNCTIONS
'function.r' contains the functions that achieve RPS, ACE, posterior predictvie mean winning probabilities, and some essential Encapsulation function for model training. 

## MODELS
'LinearModel_windows.R' is the code to train the linear regresison models and generate the corresponding mean winning probabilities for all windows. Configue different likelihood functions can be easily done by change the family parameter in INLA().
'MultinomialModel_windows.R' is the code to train the linear regresison models and generate the corresponding mean winning probabilities for all windows. 
