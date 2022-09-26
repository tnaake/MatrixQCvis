### Trend line for aggregated values to indicate drifts/trends in data acquisition

This panel shows the sum- or median-aggregated values (specified in
**Select aggregation**). The plot allows to display trends in data
acquisition that originate e.g. from differences in instrument 
sensitivity. The panel allows to display aggregated values for 

 - raw (**raw**), 
 - normalized (**normalized**), 
 - normalized+batch corrected (**batch corrected**),
 - normalized+batch corrected+transformed (**transformed**), and
 - normalized+batch corrected+transformed+imputed (**imputed**) 

assay values. The **imputed** data set can only be selected if there are 
missing values (`NA`s) in the **raw** data set. The different methods for 
normalization, transformation, batch correction, and imputation are
specified in the side panel. 

The smoothing is calculated from a selection of samples that are specified 
by the drop-down menus **Select variable** and **Select level to highlight**. 
The menu **Select variable** corresponds to the `colnames` in 
`colData(se)`. Here, we can select for the higher-order variable, e.g.
the sample type (containing for example `Type_1`, `Type_2`, ..., `QC`). 
The drop-down menu **Select level to highlight** will specify the actual 
selection from which
the trend line will be calculated (e.g. `Type_1`, `Type_2`, ..., `QC`). Also,
the menu will always include the level `all`, which will use all points to 
calculate the trend line. As an example, if we want to calculate the trend line of 
aggregated values of all samples belonging to the type `QC`, we select
`QC` in the drop-down menu. 

The panel allows for further customization after expanding the collapsed box.
The data input is selected in the drop-down menu under **Select data input**.
The smoothing method (either LOESS or linear model) is selected in the drop-down
menu under **Select smoothing method**. The aggregation method is selected
in the drop-down menu **Select smoothing method**.

With the drop-down menu **Select categorical variable to order samples**, 
the samples (x-axis) will be ordered alphanumerically according to the 
selected level (and the sample name). 