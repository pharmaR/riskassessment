CREATE TABLE IF NOT EXISTS package_metrics (
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   package_id   INT, 
   metric_id    INT,
   value        CHAR,
   /* value == 'pkg_metric_error' indicates an error. */
   /* value == NA indicates metric is not applicable for this package. */
   metric_score CHAR,
   encode       BLOB,
   FOREIGN KEY (package_id) REFERENCES package(id),
   FOREIGN KEY (metric_id) REFERENCES metric(id)
);