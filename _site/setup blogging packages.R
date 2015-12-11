install.packages(c("knitr", "servr", "devtools"))     # To process .Rmd files
devtools::install_github("hadley/lubridate")         # brocks reqs dev version
devtools::install_github("brendan-r/brocks")         # My lazy wrapper funs

# blogging
# http://www.r-bloggers.com/blogging-with-rmarkdown-knitr-and-jekyll/

library(brocks)
new_post("My first blog post!")

new_post("blogging to github not going so well")

