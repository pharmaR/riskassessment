CREATE TABLE IF NOT EXISTS MaintenanceMetrics (
   mm_id   								       CHAR NOT NULL, 
   mm_ver                        CHAR NOT NULL,
   package_has_vignettes				    CHAR,
   package_has_news                     CHAR,
   news_is_current					       CHAR,
   package_has_website				       CHAR,
   has_bug_reports					       CHAR,
   has_a_package_maintainer			    CHAR,
   source_code_is_public   				 CHAR,
   exported_objects_with_documentation  CHAR,
   status_of_last_30_reported_bugs  	 CHAR,
   FOREIGN KEY(mm_id, mm_ver) REFERENCES Packageinfo(package, version)
);