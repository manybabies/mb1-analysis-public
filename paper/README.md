# README

## Note on building the MB1 paper

To build the final ms, you must first knit files `01_` though `03_` in the project root directory. These will create the full dataset. Next, you can knit the manuscript. 

*Running chunks alone will not build the paper*

Both `03_exclusions.Rmd` and the manuscript call `paper/exclusions.Rmd`. The paper needs access to both the statistics of the exclusions and also to the final sample; an efficient way to ensure that these come from the same source is to create one exclusions routine that is called in two places. THe first place is to generate the cached data, but the second place is that it is called in the manuscript itself to generate the precise exclusions text. 

## Stage 1 vs. stage 2

`mb1-diff.docx` gives an MS word track changes version of the manuscript, documenting changes from stage 1 to stage 2. 
