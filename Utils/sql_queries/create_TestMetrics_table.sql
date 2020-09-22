CREATE TABLE IF NOT EXISTS TestMetrics (
   tm_id 		  CHAR NOT NULL, 
   tm_ver     CHAR NOT NULL,
   test_coverage CHAR,
   FOREIGN KEY(tm_id, tm_ver) REFERENCES Packageinfo(package, version)
);