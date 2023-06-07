CREATE TABLE IF NOT EXISTS decision_categories (
   id 		      INTEGER PRIMARY KEY AUTOINCREMENT, 
   decision 	  CHAR,
   color        CHAR,
   lower_limit 	DECIMAL(3, 2) NULL,
   upper_limit 	DECIMAL(3, 2) NULL
);

CREATE TABLE IF NOT EXISTS decision_categories_audit_log (
   id                     INTEGER PRIMARY KEY AUTOINCREMENT,
   decision_categories_id INTEGER NOT NULL,
   old_row_data           JSON,
   new_row_data           JSON,
   dml_type               CHAR NOT NULL,
   dml_timestamp          DATETIME DEFAULT CURRENT_TIMESTAMP,
   dml_created_by         CHAR
);

CREATE TRIGGER decision_categories_update_audit_trigger
AFTER UPDATE ON decision_categories FOR EACH ROW
BEGIN
  INSERT INTO decision_categories_audit_log (
    decision_categories_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    NEW.id,
    JSON_OBJECT(
      "decision", OLD.decision,
      "color", OLD.color,
      "lower_limit", OLD.lower_limit,
      "upper_limit", OLD.upper_limit
    ),
    JSON_OBJECT(
      "decision", NEW.decision,
      "color", NEW.color,
      "lower_limit", NEW.lower_limit,
      "upper_limit", NEW.upper_limit
    ),
    'UPDATE',
    (SELECT user FROM _variables LIMIT 1)
  );
END