### Features

The first plot shows the values for each samples along the data 
processing steps.
The feature to be displayed is selected via the drop-down menu 
**Select feature**. 

The second plot shows the coefficient of variation values for all features
in the data set along the data processing steps. The features of same 
identity can be connected by lines by clicking on **lines**. 

The element in the bottom of the tab panel (**Select features**) will specify 
the selection of features in the data set (that will be propagated through 
all tabs):

- `all`: when selected, all features in the uploaded `SummarizedExperiment` 
object will used,
- `exclude`: when selected, the features specified in the text input field
will be excluded from the uploaded `SummarizedExperiment`,
- `select`: when selected, the features specified in the text input field 
will be selected from the uploaded `SummarizedExperiment` 
(note: a minimum of three features needs to be selected in order for the 
selection to take place).

