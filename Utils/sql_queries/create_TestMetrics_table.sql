CREATE TABLE IF NOT EXISTS TestMetrics (
   tm_id 		  CHAR NOT NULL, 
   test_coverage CHAR,
   FOREIGN KEY(tm_id) REFERENCES Packageinfo(package)
);