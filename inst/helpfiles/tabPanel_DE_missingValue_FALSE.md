### Differential expression

Currently, two methods/tests are implemented for calculating differential 
expression between conditions: moderated t-tests from `limma` and the
Wald test from `proDA`. 
Both method will take the normalized+transformed+batch corrected 
(**batch corrected**) data set as input.


#### Levels
Enter here the formula for the levels. The formula has to start with a `~` 
(tilde) and the `R`-specific symbolic form:
- `+` to add terms,
- `:` to denote an interaction term, 
- `*` to denote factor crossing (`a*b` is interpreted as `a+b+a:b`),
- `-` to remove the specified term, e.g. `~ x - 1` to specify no intercept,
- `+ 0` to alternatively specify a model without intercept.

The `colnames` of `Sample meta-data` can be added as terms.  

#### Contrasts
The `colnames` of the *Model matrix* can be used to calculate contrasts, e.g. 
`a - b` to specify the contrast between `a` and `b`. 


The tabs *Model matrix* and *Contrast matrix* show the model and contrast
matrix upon correct specification of the levels and contrasts. 
The tab *Top DE* shows the differential features in a tabular format, 
while the tab *Volcano plot* displays the information of 
log fold change (for `limma`)/difference (for `proDA`) against the 
p-values (displayed as `-log10(p-value)`. 