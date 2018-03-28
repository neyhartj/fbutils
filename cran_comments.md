## Test environment



## R CMD Check comments

checking examples ... ERROR
Running examples in 'fbutils-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: fb_visualize
> ### Title: Visualize a summary of the field book data
> ### Aliases: fb_visualize
> 
> ### ** Examples
> 
> data("fbt.sample")
Warning in data("fbt.sample") : data set 'fbt.sample' not found
> 
> # For boxplots
> fb_visualize(fbt = fbt.sample)
Error in eval(lhs, parent, parent) : object 'fbt.sample' not found
Calls: fb_visualize -> fb_traits -> %>% -> eval -> eval
Execution halted

checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'grid_cor'
  'p_obs' 'rows' 'cols' 'grid.rows' 'grid.cols' 'layers'

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter 'Writing R documentation files' in the 'Writing R
Extensions' manual.

checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'fbutils_shiny'

checking dependencies in R code ... NOTE
There are ::: calls to the package's namespace in its code. A package
  almost never needs to use ::: for its own objects:
  'grid_cor'

checking R code for possible problems ... NOTE
as_field_book_table: no visible binding for global variable '.'
fb_heatmap: no visible binding for global variable '.'
fb_heatmap: no visible binding for global variable 'column'
fb_heatmap: no visible binding for global variable 'trait'
fb_heatmap: no visible binding for global variable 'value'
fb_outliers: possible error in fb_traits(x = fbt, "numeric"): unused
  argument (x = fbt)
fb_outliers: no visible binding for global variable '.'
fb_outliers: no visible binding for global variable 'trait'
... 42 lines ...
fb_visualize: no visible binding for global variable 'trait'
fb_visualize: no visible binding for global variable 'value'
read_fb: no visible global function definition for 'read.csv'
Undefined global functions or variables:
  . V_P V_R adjusted column fit g.cols g.layer g.row grid_type head
  is_outlier line_name lm lower_bound read.csv rel_eff sd sigma trait
  type unadjusted unique_id upper_bound value var
Consider adding
  importFrom("stats", "lm", "sd", "sigma", "var")
  importFrom("utils", "head", "read.csv")
to your NAMESPACE file.

checking Rd files ... NOTE
prepare_Rd: fb_spatial_adj.Rd:43-45: Dropping empty section \details

checking for unstated dependencies in vignettes ... NOTE
'library' or 'require' call not declared from: 'tidyverse'



##