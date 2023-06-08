CREATE TABLE IF NOT EXISTS package (
   id             INTEGER PRIMARY KEY AUTOINCREMENT,
   name           CHAR, 
   version        CHAR,
   title          CHAR,
   description    TEXT,
   maintainer     CHAR,
   author         CHAR,
   license        CHAR,
   published_on   CHAR,
   score          REAL,
   weighted_score REAL,
   decision_id    INT,
   decision_by    CHAR,
   decision_date  DATE NULL,
   date_added     DATE,
   FOREIGN KEY (decision_id) REFERENCES decision_categories(id)
);

CREATE TABLE IF NOT EXISTS package_audit_log (
   id                 INTEGER PRIMARY KEY AUTOINCREMENT,
   package_id INTEGER NOT NULL,
   old_row_data       JSON,
   new_row_data       JSON,
   dml_type           CHAR NOT NULL,
   dml_timestamp      DATETIME DEFAULT UTC_TIMESTAMP,
   dml_created_by     CHAR
);

CREATE TRIGGER package_insert_audit_trigger
AFTER INSERT ON package FOR EACH ROW
BEGIN
  INSERT INTO package_audit_log (
    package_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    NEW.id,
    NULL,
    JSON_OBJECT(
      "name", NEW.name,
      "version", NEW.version,
      "title", NEW.title,
      "description", NEW.description,
      "maintainer", NEW.maintainer,
      "author", NEW.author,
      "license", NEW.license,
      "published_on", NEW.published_on,
      "score", NEW.score,
      "weighted_score", NEW.weighted_score,
      "decision_id", NEW.decision_id,
      "decision", (SELECT decision FROM decision_categories WHERE id = NEW.decision_id LIMIT 1),
      "decision_by", NEW.decision_by,
      "decision_date", NEW.decision_date,
      "date_added", NEW.date_added
    ),
    'INSERT',
    (SELECT user FROM _variables ORDER BY id DESC LIMIT 1)
  );
END

CREATE TRIGGER package_update_audit_trigger
AFTER UPDATE ON package FOR EACH ROW
BEGIN
  INSERT INTO package_audit_log (
    package_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    NEW.id,
    JSON_OBJECT(
      "name", OLD.name,
      "version", OLD.version,
      "title", OLD.title,
      "description", OLD.description,
      "maintainer", OLD.maintainer,
      "author", OLD.author,
      "license", OLD.license,
      "published_on", OLD.published_on,
      "score", OLD.score,
      "weighted_score", OLD.weighted_score,
      "decision_id", OLD.decision_id,
      "decision", (SELECT decision FROM decision_categories WHERE id = OLD.decision_id LIMIT 1),
      "decision_by", OLD.decision_by,
      "decision_date", OLD.decision_date,
      "date_added", OLD.date_added
    ),
    JSON_OBJECT(
      "name", NEW.name,
      "version", NEW.version,
      "title", NEW.title,
      "description", NEW.description,
      "maintainer", NEW.maintainer,
      "author", NEW.author,
      "license", NEW.license,
      "published_on", NEW.published_on,
      "score", NEW.score,
      "weighted_score", NEW.weighted_score,
      "decision_id", NEW.decision_id,
      "decision", (SELECT decision FROM decision_categories WHERE id = NEW.decision_id LIMIT 1),
      "decision_by", NEW.decision_by,
      "decision_date", NEW.decision_date,
      "date_added", NEW.date_added
    ),
    'UPDATE',
    (SELECT user FROM _variables ORDER BY id DESC LIMIT 1)
  );
END

CREATE TRIGGER package_delete_audit_trigger
AFTER DELETE ON package FOR EACH ROW
BEGIN
  INSERT INTO package_audit_log (
    package_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    OLD.id,
    JSON_OBJECT(
      "name", OLD.name,
      "version", OLD.version,
      "title", OLD.title,
      "description", OLD.description,
      "maintainer", OLD.maintainer,
      "author", OLD.author,
      "license", OLD.license,
      "published_on", OLD.published_on,
      "score", OLD.score,
      "weighted_score", OLD.weighted_score,
      "decision_id", OLD.decision_id,
      "decision", (SELECT decision FROM decision_categories WHERE id = OLD.decision_id LIMIT 1),
      "decision_by", OLD.decision_by,
      "decision_date", OLD.decision_date,
      "date_added", OLD.date_added
    ),
    NULL,
    'DELETE',
    (SELECT user FROM _variables ORDER BY id DESC LIMIT 1)
  );
END