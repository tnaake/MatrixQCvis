### MA plot and Hoeffding's D statistic

In the first part of the panel the `A` vs. `M` plots per sample are depicted.

The values are defined as follows
- $A = 1/2 \cdot (I_i + I_j)$ and
- $M = I_i- I_j$,
where $I_i$ and $I_j$ are transformed values. In the case of **raw** or
**normalized** the values are `log2`-transformed prior to 
calculating `A` and `M`. In case of **transformed** or
**batch corrected**
the values are taken as they are (N.B. when the transformation
method is set to **none** the values are not `log2`-transformed).

The values for $I_i$ are taken from the sample $i$. For $I_j$, the feature-wise 
means are calculated from the values of the **group** $j$ of samples specified 
by the drop-down menu **group**. The sample for calculating $I_i$ is
excluded from the group $j$. The group can be set to `"all"`
(i.e. all samples except sample $i$ are used to calculate
$I_j$) or any other column in `colData(se)`. For any group except `"all"` the
group is taken to which the sample $i$ belongs to and the sample $i$ is 
excluded from the feature-wise calculation. 

The MA values for all samples are by default displayed facet-wise. The MA plot 
can be set to specific samples by changing the selected value in the 
input menu **plot**.

The underlying data set can be selected by the drop-down menu 
(**Data set for the MA plot**). 


In the second part of the tab, the Hoeffding's D statistic values are 
visualized for the different data sets **raw**, **normalized**, 
**transformed**, and **batch corrected**. 

`D` is a measure of the distance between `F(A, M)` and `G(A)H(M)`, where
`F(A, M)` is the joint cumulative distribution function (CDF) of 
`A` and `M`, and `G` and `H` are marginal CDFs. 
The higher the value of `D`, the more dependent are `A` and `M`. 