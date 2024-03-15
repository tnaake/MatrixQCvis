### Empirical distribution function

The plot shows the empirical distribution function (ECDF) of the values of the sample 
$i$ and the feature-wise mean values of a group $j$ of samples specified 
by the drop-down menu **Group**. The sample for calculating $I_i$ is
excluded from the group $j$. The group can be set to `"all"`
(i.e. all samples except sample $i$ are used to calculate
$I_j$) or any other column in `colData(se)`. For any group except `"all"` the
group is taken to which the sample $i$ belongs to and the sample $i$ is 
excluded from the feature-wise calculation.

The underlying data set can be selected by the drop-down menu 
**Data set for the MA plot**. The sample $i$ can be selected by
the drop-down menu **Sample**. The group can be selected by 
the drop-down menu **Group**.
