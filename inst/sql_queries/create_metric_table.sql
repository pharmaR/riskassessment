CREATE TABLE IF NOT EXISTS metric (
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   name         CHAR,
   long_name    CHAR,    /* Represents the title of the metrixBox. */
   is_url       INTEGER, /* Indicates whether the metric value is a url. 0: indicates FALSE, 1: indicates TRUE */
   is_perc      INTEGER, /* Indicates whether the metric value is a percentage. 0: indicates FALSE, 1: indicates TRUE */
   description  CHAR,
   class        CHAR,    /* class = maintenance or test */
   weight       REAL
);

INSERT INTO metric 
  (name, long_name, description, is_perc, is_url, class, weight)
VALUES 
  ('has_vignettes',       'Vignettes',         'Number of vignettes',              0, 0, 'maintenance', 1),
  ('has_news',            'NEWS file',         'Number of NEWS files',             0, 0, 'maintenance', 1),
  ('news_current',        'NEWS current',      'NEWS contains current version',    0, 0, 'maintenance', 1),
  ('has_bug_reports_url', 'Report Bugs',       'URL to report bugs exists',    0, 0, 'maintenance', 1),
  ('has_website',         'Website',           'Package public website',           0, 1, 'maintenance', 1),
  ('has_maintainer',      'Maintainer',        'Package maintainers',              0, 0, 'maintenance', 1),
  ('has_source_control',  'Source Control',    'Package source control url',       0, 1, 'maintenance', 1),
  ('export_help',         'Documentation',     '% of documented objects',          1, 0, 'maintenance', 1),
  ('bugs_status',         'Bugs Closure Rate', '% of the last 30 bugs closed',     1, 0, 'maintenance', 1),
  ('license',             'License',           "Package's license",                0, 0, 'maintenance', 1),
  ('covr_coverage',       'Test Coverage',     'Percentage of objects tested',     0, 1, 'test', 1),
  ('downloads_1yr',       'Downloads',         'Number of package downloads in the last year', 0, 0, 'community', 1)
;

CREATE TABLE IF NOT EXISTS metric_audit_log (
   id            INTEGER PRIMARY KEY AUTOINCREMENT,
   metric_id     INTEGER NOT NULL,
   metric_name   CHAR,
   old_weight    REAL,
   new_weight    REAL,
   dml_type      CHAR NOT NULL,
   dml_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TRIGGER metric_update_audit_trigger
AFTER UPDATE ON metric FOR EACH ROW
BEGIN
  INSERT INTO metric_audit_log (
    metric_id,
    metric_name,
    old_weight,
    new_weight,
    dml_type
  )
  VALUES(
    NEW.id,
    NEW.name,
    OLD.weight,
    NEW.weight,
    'UPDATE'
  );
END