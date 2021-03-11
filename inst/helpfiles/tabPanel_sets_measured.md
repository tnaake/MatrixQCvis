### Retrieve the features specified by intersection of sets

This panel builds upon the *Upset* panel. By selecting the check boxes,
the names of the features (taken from the `rownames` of the features) 
are printed as text that fulfill the defined intersection of sets.

Example: 
Five sets (A, B, C, D, and E) are found for a specified variable (e.g., sample 
type). When selecting the boxes for A and B (while not selecting the boxes 
for C, D, and E) the features that are presen for A and B (but not 
in the sets C, D, and E) are returned. 

Presence is defined by a feature being measured in at least one sample of a
set. 
