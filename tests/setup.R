if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "pls" , output_dir = file.path( getwd() ) )
pls_df <- readRDS( file.path( getwd() , "2014/pls_fy_ae_puplda.rds" ) )

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
mean( pls_df[ , "popu_lsa" ] )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	mean 
)
prop.table( table( pls_df[ , "c_relatn" ] ) )

prop.table(
	table( pls_df[ , c( "c_relatn" , "stabr" ) ] ) ,
	margin = 2
)
sum( pls_df[ , "popu_lsa" ] )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	sum 
)
quantile( pls_df[ , "popu_lsa" ] , 0.5 )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	quantile ,
	0.5 
)
sub_pls_df <- subset( pls_df , visits > 1000000 )
mean( sub_pls_df[ , "popu_lsa" ] )
var( pls_df[ , "popu_lsa" ] )

tapply(
	pls_df[ , "popu_lsa" ] ,
	pls_df[ , "stabr" ] ,
	var 
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
library(dplyr)
pls_tbl <- tbl_df( pls_df )
pls_tbl %>%
	summarize( mean = mean( popu_lsa ) )

pls_tbl %>%
	group_by( stabr ) %>%
	summarize( mean = mean( popu_lsa ) )
