### Coefficient of variation among the samples

This panel shows the coefficient of variation values for 

 - raw (**raw**), 
 - normalized (**normalized**), 
 - normalized+batch corrected (**batch corrected**),
 - normalized+batch corrected+transformed (**transformed**), and
 - normalized+batch corrected+transformed+imputed (**imputed**) 
 
assay values. The **imputed** values are only displayed if there are 
missing values (`NA`s) in the **raw** data set. The different methods for 
normalization, transformation, batch correction and imputation are 
specified in the side panel. 

The panel displays the coefficient of variation values from the samples of the
 `SummarizedExperiment` object (in percent). The coefficients of variation are 
 calculated according to the formula `sd(x) / mean(x) * 100` with `x` 
 the sample values and `sd` the standard deviation.
 