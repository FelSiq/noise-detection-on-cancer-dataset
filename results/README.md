# Results subdirectory
This subdirectory is destined to compile all gotten results of the experiments. It has two subsections: Overall results and By-Class Results. 

# Overall Results
The metadata is organized by the type of the process of each dataset, and the columns is organized by the sequence which follows:

```
Date|# Fold of CV|Dataset name|Classifier|Filter|SMOTE|AccOriginal|AccCorrupted|AccFiltered|PValueOriginal|PValueCorrupted|PValueFiltered|
```

Example:

```
Wed Jan 10 15:52:30 2018|1|CHOL.mirnaseq.txt|RF|HARF|FALSE|1|1|1|0.1073742|0.1073742|0.1073742|
```

You can run each subscript separately, or you may run just ''run-all.R'' in order to execute then all.

# By-Class Results:
It compiles more specific results, for each experiment, like accuracy by class, F1 Score, Specificity, Sensitivity, etc.
