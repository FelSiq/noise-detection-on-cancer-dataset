# Noise Detection and Removal on Cancer Dataset
This is my Scientific Initiation Project repository. The purpose of this work is to check how noise affects the performance of machine learning predictive models, more specifically with genomic data related to the cancer disease. 

# How it works
This script get a sequence of datasets, and may or may not balance the classes of the instances (all datasets used has only two classes). The two experiments are made separately. 

Then, artificial noise is inputted on the dataset at 0.2 ratio, using a random methodology (each instance has the given fixed ratio of getting it class label exchanged). 

The corrupted dataset is feed into each noise filter at a time, expecting that they remove at least the majority of the artificial inputted noise, and some noise instances originally on the dataset.

To verify the performance of each noise filter, different predictive models are fit with the original dataset, the corrupted dataset and the filtered dataset, and these three accuracies got to be are compared.

# Filters used:
Will may find all these filters, alongside all relevant information about then, at https://CRAN.R-project.org/package=NoiseFiltersR.

- AENN ('All-k Edited Nearest Neighbors')
- INFCC ('Iterative Noise Filter based on the Fusion of Classifiers')
- HARF ('High Agreement Random Forest')
- ENG ('Editing with Nearest Graphs')

# Classifiers used (R package):
- k-Nearest Neighbors ('class')
- Random Forest ('randomForest')
- Support Vector Machine with Linear Kernel ('e1071')