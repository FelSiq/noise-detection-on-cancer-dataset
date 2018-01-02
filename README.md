# Noise Detection and Removal on Cancer Dataset
This is my Scientific Initiation Project repository. The purpose of this work is to check how noise affects the performance of machine learning predictive models, more specifically with genomic data related to the cancer disease. 

# How it works
This script get a sequence of datasets, and may or may not balance the classes of the instances (all datasets used has only two classes). The two experiments are made separately. 

Then, artificial noise is inputted on the dataset at 0.2 ratio, using a random methodology (each instance has the given fixed ratio of getting it class label exchanged). 

The corrupted dataset is feed into each noise filter at a time, expecting that they remove at least the majority of the artificial inputted noise, and some noise instances originally on the dataset.

To verify the performance of each noise filter, different predictive models are fit with the original dataset, the corrupted dataset and the filtered dataset, and these three accuracies got to be are compared.

# Technical details:
The experiment is reproducible, because the random seed is fixed with the arbitrarily selected '101010' value, which means that all script runs should hold the same results. 

If you're interested on the results, just check out the 'results' subdirectory.

If you want to change some experiment parameters, like the datasets, noise filters, classifiers, noise input ratio, number of folds on cross validation etc, just check out the './src/config.R' file and edit by hand whatever and whenever you want. More specific configuration must be edited on the 'main.R' file and it shouldn't be done unless you're sure of what you're doing, do with your own risk.

# How to run:
The whole experiment should take days or weeks to be completed, and running it isn't recommended unless you want some metadata which is not already available on the 'result' subdirectory.

Just open a R section and type:
```
source('main.R')
```

The results will be automatically appended into the 'NoiseResults.dat' output file. If it does not exists, then it will be created. Make sure that the R section have all the permission it needs on your machine.

# Filters used:
Will may find all these filters, alongside all relevant information about then, at https://CRAN.R-project.org/package=NoiseFiltersR.

- AENN ('All-k Edited Nearest Neighbors')
- INFFC ('Iterative Noise Filter based on the Fusion of Classifiers')
- HARF ('High Agreement Random Forest')
- ENG ('Editing with Nearest Graphs')

# Classifiers used (R package):
- k-Nearest Neighbors ('class')
- Random Forest ('randomForest')
- Support Vector Machine with Linear Kernel ('e1071')