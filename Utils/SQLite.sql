CREATE TABLE Select_packages(
   select_packages CHAR
);

CREATE TABLE Packageinfo(
   package        CHAR PRIMARY KEY NOT NULL, 
   version        CHAR,
   title          CHAR,
   description    TEXT,
   maintainer     CHAR,
   author         CHAR,
   license        CHAR,
   published_on   CHAR,
   score          INT,
   decision       CHAR
);

CREATE TABLE MaintenanceMetrics (
   mm_id   								       CHAR NOT NULL, 
   package_has_vignettes				    CHAR,
   package_has_news                     CHAR,
   news_is_current					       CHAR,
   package_has_website				       CHAR,
   has_bug_reports					       CHAR,
   has_a_package_maintainer			    CHAR,
   source_code_is_public   				 CHAR,
   exported_objects_with_documentation  CHAR,
   status_of_last_30_reported_bugs  	 CHAR,
   FOREIGN KEY(mm_id) REFERENCES Packageinfo(package)
);

CREATE TABLE CommunityUsageMetrics (
  cum_id 					      CHAR NOT NULL, 
  no_of_downloads_last_year   INT,
  month 					         CHAR,
  no_of_downloads 			   INT,
  ver_release 				      CHAR,
  position					      INT,
  time_since_first_release    INT,
  time_since_version_release  INT,
   FOREIGN KEY(cum_id) REFERENCES Packageinfo(package)
);

CREATE TABLE TestMetrics (
   tm_id 		  CHAR NOT NULL, 
   test_coverage CHAR,
   FOREIGN KEY(tm_id) REFERENCES Packageinfo(package)
);

CREATE TABLE Comments (
   comm_id 		 CHAR NOT NULL, 
   user_name 	 CHAR,
   user_role 	 CHAR,
   comment 		 CHAR,
   comment_type CHAR,
   added_on 	 DATE
);


	
