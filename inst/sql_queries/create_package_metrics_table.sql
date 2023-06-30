CREATE TABLE IF NOT EXISTS package_metrics (
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   package_id   INT, 
   metric_id    INT,
   value        CHAR,
   /* value == 'pkg_metric_error' indicates an error. */
   /* value == NA indicates metric is not applicable for this package. */
   encode       BLOB,
   FOREIGN KEY (package_id) REFERENCES package(id),
   FOREIGN KEY (metric_id) REFERENCES metric(id)
);

CREATE TABLE IF NOT EXISTS package_metrics_audit_log (
   id                 INTEGER PRIMARY KEY AUTOINCREMENT,
   package_metrics_id INTEGER NOT NULL,
   old_row_data       JSON,
   new_row_data       JSON,
   dml_type           CHAR NOT NULL,
   dml_timestamp      DATETIME DEFAULT UTC_TIMESTAMP,
   dml_created_by     CHAR
);

CREATE TRIGGER package_metrics_insert_audit_trigger
AFTER INSERT ON package_metrics FOR EACH ROW
BEGIN
  INSERT INTO package_metrics_audit_log (
    package_metrics_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    NEW.id,
    NULL,
    JSON_OBJECT(
      "package_id", NEW.package_id,
      "package_name", (SELECT name FROM package WHERE id = NEW.package_id LIMIT 1),
      "metric_id", NEW.metric_id,
      "metric_name", (SELECT name FROM metric WHERE id = NEW.metric_id LIMIT 1),
      "value", NEW.value,
      "weight", NEW.weight
    ),
    'INSERT',
    (SELECT user FROM _variables ORDER BY id DESC LIMIT 1)
  );
END

CREATE TRIGGER package_metrics_update_audit_trigger
AFTER UPDATE ON package_metrics FOR EACH ROW
BEGIN
  INSERT INTO package_metrics_audit_log (
    package_metrics_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    NEW.id,
    JSON_OBJECT(
      "package_id", OLD.package_id,
      "package_name", (SELECT name FROM package WHERE id = OLD.package_id LIMIT 1),
      "metric_id", OLD.metric_id,
      "metric_name", (SELECT name FROM metric WHERE id = OLD.metric_id LIMIT 1),
      "value", OLD.value,
      "weight", OLD.weight
    ),
    JSON_OBJECT(
      "package_id", NEW.package_id,
      "package_name", (SELECT name FROM package WHERE id = NEW.package_id LIMIT 1),
      "metric_id", NEW.metric_id,
      "metric_name", (SELECT name FROM metric WHERE id = NEW.metric_id LIMIT 1),
      "value", NEW.value,
      "weight", NEW.weight
    ),
    'UPDATE',
    (SELECT user FROM _variables ORDER BY id DESC LIMIT 1)
  );
END

CREATE TRIGGER package_metrics_delete_audit_trigger
AFTER DELETE ON package_metrics FOR EACH ROW
BEGIN
  INSERT INTO package_metrics_audit_log (
    package_metrics_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    OLD.id,
    JSON_OBJECT(
      "package_id", OLD.package_id,
      "package_name", (SELECT name FROM package WHERE id = OLD.package_id LIMIT 1),
      "metric_id", OLD.metric_id,
      "metric_name", (SELECT name FROM metric WHERE id = OLD.metric_id LIMIT 1),
      "value", OLD.value,
      "weight", OLD.weight
    ),
    NULL,
    'DELETE',
    (SELECT user FROM _variables ORDER BY id DESC LIMIT 1)
  );
END