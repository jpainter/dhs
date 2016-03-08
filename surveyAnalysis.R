
library(survey)

# for mali dhs 2012 set = 62 and run data protion of dhs_data.R

# check psu completeness of else get error "missing values in `id'"
table(hmcwh$hv021, useNA = 'always')
sum(!(hmcwh$hv021 == hmcwh$v021), na.rm = TRUE)

# why are some hv021 missing???

x = hmcwh %>% mutate(
  weight.h = as.numeric( hv005 / 1000000 ),
  weight.c = as.numeric( v005 / 1000000 ),
  hml32 = if( exists('hml32', where = hmcwh)) { ifelse(hml32>1, NA, hml32) } else { NA},
  hml35 = if( exists('hml35', where = hmcwh)) { ifelse(hml35>1, NA, hml35)  } else { NA}
)
        
        
#### alternative
# test if strata exists; some surveys have no strata (e.g. madagascar mis 2011)
has.strata.023 = nrow( as.data.frame.table( table(hmcwh$hv023) ) ) > 1
has.strata.025 = nrow( as.data.frame.table( table(hmcwh$hv025) ) ) > 1
sum(!(hmcwh$hv023 == hmcwh$v023), na.rm = TRUE)
sum(!(hmcwh$hv025 == hmcwh$v025), na.rm = TRUE)

if (has.strata.025) { # urban/rural
  if (has.strata.023)  { strataformula = as.formula("~hv025 + hv023")
  } else {
    strataformula = as.formula("~hv025")
  }
  } else { 
    if (has.strata.023) {strataformula = as.formula("~hv023")
    } else { strataformula = NULL }
  }

# create survey design object (svy) damico
        # svy <- 
        #             svydesign( 
        #               ~hv021 , 
        #               strata = ~hv023 , 
        #               data = x , 
        #               weights = ~weight.h
        #             )

# see Vanderelst/Speybroeck (this is different from Damico)
 x.h = x %>% filter(!is.na(hv021))
 svy.h <- 
            svydesign( 
              ~hv021 + hv002 , # psu + household
              strata = strataformula , 
              data = x.h , 
              weights = ~weight.h
            )
        
  svy.h <- update( one = 1 , svy.h )
  
  svytotal( ~one , svy.h ) # ????
  svyby( ~one , ~one , svy.h , unwtd.count )
  svyby( ~one , ~hv025 , svy.h , unwtd.count )
  # TODO what are population estimates for urban/rural?

  
# childrens...
  x.c = x %>% filter(weight.c > 0)
        svy.c <- 
            svydesign( 
              ~v021  , # psu + household
              strata = ~v025 + v023 , 
              data =  x.c , 
              weights = ~weight.c
            )   
        
  svy.c <- update( one = 1 , 
                   b78 = ifelse( is.na(b8), b7, b8), # age(ys) of live and dead
                   b78 = ifelse( b78>4, NA, b78),
                   svy.c 
                   )

  svytotal( ~one , svy.c ) # ????
  svyby( ~one , ~one , svy.c , unwtd.count )


# Malaria
  # by slide 
   svyby(~ one,  ~hml32 ,  svy.h , unwtd.count )
   svymean( ~hml32 ,  svy.h ,   na.rm = TRUE )
   svyby( ~hml32 ,  ~hv023, svy.h , svymean ,  na.rm = TRUE  ) #psu
   svyby( ~hml32 ,  ~hv025, svy.h , svymean ,  na.rm = TRUE  ) #urban/rural
   
   # by rdt
   svyby( ~one , ~hml35 ,  svy.h , unwtd.count )
   svymean( ~hml35 ,  svy.h ,   na.rm = TRUE )
   svyby( ~hml35 ,  ~hv023, svy.h , svymean ,  na.rm = TRUE  ) #psu
   svyby( ~hml35 ,  ~hv025, svy.h , svymean ,  na.rm = TRUE  ) #urban/rural

# note potential difference in slide/rdt by urban/rural
   svytable(~ hml35 + hml32 + v025, svy.h, round=TRUE) 
   # TODO compute agreement/kappa
   
# Mortality (b5 is actually survived, mortality is 1- result below)
  svyby( ~ one , ~ b5 ,  svy.c , unwtd.count )
  svymean(~ b5, svy.c, na.rm = TRUE)
  svytable(~ b5 + v025, svy.c, round=TRUE) 
  svyby( ~ b5 ,  ~ v025, svy.c , svymean ,  na.rm = TRUE  )
  svyby( ~ b5 ,  ~ b78, svy.c , svymean ,  na.rm = TRUE  )
  
# bednets
   svyby( ~one , ~hml1 ,  svy.h , unwtd.count )
   svymean( ~hml1 ,  svy.h ,   na.rm = TRUE )
   svyby( ~one , ~hml2 ,  svy.h , unwtd.count )
   svymean( ~hml2 ,  svy.h ,   na.rm = TRUE )
   svyby( ~one , ~hv228 ,  svy.h , unwtd.count )
   svymean( ~hv228 ,  svy.h ,   na.rm = TRUE )
   
   svyby( ~hv228 ,  ~v025, svy.h , svymean ,  na.rm = TRUE  ) 
   svyby( ~hv228 ,  ~b8, svy.h , svymean ,  na.rm = TRUE  ) 

  