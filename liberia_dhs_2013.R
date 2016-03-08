
svy = load_survey_object( .country = "Liberia", .survey_year = "Standard DHS 2013")

x = svy[[4]]

table(x$weight.h>0, useNA = 'always') 
table(x$weight.w>0, useNA = 'always') 
table(x$weight.c>0, useNA = 'always') 


table(x$hv021, useNA = 'always') 
table(x$hv023, useNA = 'always') 
table(x$hv025, useNA = 'always') 

table(x$hml1, useNA = 'always') 

svy.h = svy[[1]]
svy.c = svy[[2]]
  
svymean( ~ hml1.own , svy.h , na.rm = TRUE  )
svytotal( ~hml1.own , svy.h , na.rm = TRUE )
svytable( ~hml1.own , svy.h  )
svyby( ~one , ~hml1.own , svy.h , unwtd.count )


svymean( ~ hml12.anynet , svy.h , na.rm = TRUE  )

svytable( ~hv025 , svy.h  )
svytotal( ~hv025 , svy.h  )

svyby( ~one , ~hv104 , svy.h , unwtd.count )
svytotal( ~hv104 , svy.h , na.rm = TRUE )
svytable( ~hv104 , svy.h  )
