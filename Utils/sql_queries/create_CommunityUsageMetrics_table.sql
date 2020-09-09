CREATE TABLE IF NOT EXISTS CommunityUsageMetrics (
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