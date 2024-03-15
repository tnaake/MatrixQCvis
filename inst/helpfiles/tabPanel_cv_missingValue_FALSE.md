### Coefficient of variation among the samples

This panel shows the coefficient of variation values for raw (**raw**), 
normalized (**normalized**), normalized+transformed (**transformed**),
and normalized+transformed+batch corrected (**batch corrected**)
assay values. 
The different methods for normalization, transformation, and batch 
correction are specified in the side panel. 

The panel displays the coefficient of variation values from the samples of the
 `SummarizedExperiment` object. The coefficients of variation are calculated 
 according to the formula `sd(x) / mean(x) * 100` with `x` the sample values 
 and `sd` the standard deviation.
 