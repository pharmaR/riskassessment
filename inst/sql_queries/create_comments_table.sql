CREATE TABLE IF NOT EXISTS comments (
   id 		     CHAR NOT NULL, 
   user_name 	 CHAR,
   user_role 	 CHAR,
   comment 		 CHAR,
   comment_type CHAR,
   added_on 	 DATE
);