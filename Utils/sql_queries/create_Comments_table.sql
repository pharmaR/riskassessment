CREATE TABLE IF NOT EXISTS Comments (
   comm_id 		 CHAR NOT NULL, 
   user_name 	 CHAR,
   user_role 	 CHAR,
   comment 		 CHAR,
   comment_type CHAR,
   added_on 	 DATE
);
