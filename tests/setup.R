# census, not survey.
# dewey decimal index
# finger to lips shush
library(haven)

this_tf <- tempfile()

spss_url <- "https://www.imls.gov/sites/default/files/2023-06/pls_fy2021_spss.zip"

download.file( spss_url , this_tf, mode = 'wb' )

unzipped_files <- unzip( this_tf , exdir = tempdir() )
		
administrative_entity_spss_fn <-
	unzipped_files[ grepl( 'AE(.*)sav$' , basename( unzipped_files ) ) ]

pls_tbl <- read_spss( administrative_entity_spss_fn )

pls_df <- data.frame( pls_tbl )

names( pls_df ) <- tolower( names( pls_df ) )

pls_df[ , 'one' ] <- 1
for( this_col in names( pls_df ) ){

	if( class( pls_df[ , this_col ] ) == 'character' ){
	
		pls_df[ pls_df[ , this_col ] %in% 'M' , this_col ] <- NA
		
	}
	
	if( 
		( class( pls_df[ , this_col ] ) == 'numeric' ) | 
		( this_col %in% c( 'phone' , 'startdat' , 'enddate' ) ) 
	){
	
		pls_df[ pls_df[ , this_col ] %in% c( -1 , -3 , -4 , -9 ) , this_col ] <- NA
		
	}
	
}
# pls_fn <- file.path( path.expand( "~" ) , "PLS" , "this_file.rds" )
# saveRDS( pls_df , file = pls_fn , compress = FALSE )
# pls_df <- readRDS( pls_fn )
pls_df <- 
	transform( 
		pls_df , 
		
		c_relatn = 
			factor( c_relatn , levels = c( "HQ" , "ME" , "NO" ) ,
				c( "HQ-Headquarters of a federation or cooperative" ,
				"ME-Member of a federation or cooperative" ,
				"NO-Not a member of a federation or cooperative" )
			) ,
			
		more_than_one_librarian = as.numeric( libraria > 1 )
				
	)	
nrow( pls_df )

table( pls_df[ , "stabr" ] , useNA = "always" )
mean( pls_df[ , "popu_lsa" ] , na.rm = TRUE )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( pls_df[ , "c_relatn" ] ) )

prop.table(
	table( pls_df[ , c( "c_relatn" , "stabr" ) ] ) ,
	margin = 2
)
sum( pls_df[ , "popu_lsa" ] , na.rm = TRUE )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( pls_df[ , "popu_lsa" ] , 0.5 , na.rm = TRUE )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_pls_df <- subset( pls_df , visits > 1000000 )
mean( sub_pls_df[ , "popu_lsa" ] , na.rm = TRUE )
var( pls_df[ , "popu_lsa" ] , na.rm = TRUE )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	var ,
	na.rm = TRUE 
)
t.test( popu_lsa ~ more_than_one_librarian , pls_df )
this_table <- table( pls_df[ , c( "more_than_one_librarian" , "c_relatn" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		popu_lsa ~ more_than_one_librarian + c_relatn , 
		data = pls_df
	)

summary( glm_result )
# remove closed and temporarily closed libraries
results <- table( pls_df[ !( pls_df[ , 'statstru' ] %in% c( '03' , '23' ) ) , 'c_relatn' ] )

stopifnot( results[ "HQ-Headquarters of a federation or cooperative" ] == 112 )
stopifnot( results[ "ME-Member of a federation or cooperative" ] == 6859 )
stopifnot( results[ "NO-Not a member of a federation or cooperative" ] == 2236 )
library(dplyr)
pls_tbl <- as_tibble( pls_df )
pls_tbl %>%
	summarize( mean = mean( popu_lsa , na.rm = TRUE ) )

pls_tbl %>%
	group_by( stabr ) %>%
	summarize( mean = mean( popu_lsa , na.rm = TRUE ) )
library(data.table)
pls_dt <- data.table( pls_df )
pls_dt[ , mean( popu_lsa , na.rm = TRUE ) ]

pls_dt[ , mean( popu_lsa , na.rm = TRUE ) , by = stabr ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'pls' , pls_df )
dbGetQuery( con , 'SELECT AVG( popu_lsa ) FROM pls' )

dbGetQuery(
	con ,
	'SELECT
		stabr ,
		AVG( popu_lsa )
	FROM
		pls
	GROUP BY
		stabr'
)
