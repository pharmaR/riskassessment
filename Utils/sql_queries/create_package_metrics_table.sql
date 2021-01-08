CREATE TABLE IF NOT EXISTS package_metrics(
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   package_id   INT, 
   metric_id    INT,
   value        CHAR, /*a value equal to 'pkg_metric_error' indicates an error*/
   weight       INT,
   FOREIGN KEY (package_id) REFERENCES package(id),
   FOREIGN KEY (metric_id) REFERENCES metric(id)
);