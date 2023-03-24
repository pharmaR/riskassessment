CREATE TABLE IF NOT EXISTS decision_categories (
   id 		      INTEGER PRIMARY KEY AUTOINCREMENT, 
   decision 	  CHAR,
   lower_limit 	DECIMAL(3, 2) NULL,
   upper_limit 	DECIMAL(3, 2) NULL
);