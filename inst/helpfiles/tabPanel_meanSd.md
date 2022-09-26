### Mean-sd plots

The panel shows the mean-sd (standard deviation) plots for 

 - normalized+batch corrected+transformed (**transformed**), and 
 - normalized+batch corrected+transformed+imputed (**imputed**) assay values.
 
The **imputed** values are only displayed if there are missing values (`NA`s) in the 
**raw** data set.

The sd and mean are calculated feature-wise from the values of the
respective data set. The plot allows to visualize if there is a dependence
of the sd on the mean. The red line depicts the running median estimator 
(window-width 10%). In case of sd-mean independence, the running median
should be approximately horizontal. 
