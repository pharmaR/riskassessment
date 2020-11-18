CREATE TABLE IF NOT EXISTS MaintenanceMetrics (
   mm_id   								       CHAR NOT NULL, 
   mm_ver                        CHAR NOT NULL,
   mm_name                       CHAR,
   mm_value                      CHAR,
   mm_label                      CHAR,
   FOREIGN KEY(mm_id, mm_ver) REFERENCES Packageinfo(package, version)
);
