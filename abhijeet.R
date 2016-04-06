

# example of how to use dplyr to add column with results of cume_dist function 
library(dplyr)
str(iris)  # database for  testing
iris %>% 
  group_by( Species ) %>%  
  mutate( Sepal.Width.dist = 1 - cume_dist(Sepal.Width))
           

# Convert above to add column with a dynamic name 
# that has results of function on named variable
str(iris)
var = "Sepal.Width"
iris %>% 
  group_by_( "Species" ) %>%  
  mutate_( .dots = setNames( paste0("1-cume_dist(", var, ")"), paste0( var, ".dist") )
           # first argument is string of function; second is the the name 
           ) 

# create a function from above code
add_cumalitive_dist_column <- function(df, var, grouping ){
  df = df %>% 
    group_by_( grouping ) %>%  
    mutate_( .dots = setNames( paste0("1-cume_dist(", var, ")"), paste0( var, ".dist") )
    ) 
  df
}

# Iterate through width and length variables, adding new column with output of function
vars = names(iris)[1:4]
for (var in vars){
  new_iris = add_cumalitive_dist_column(df = iris, var = vars, grouping = "Species")
}

# See result
new_iris