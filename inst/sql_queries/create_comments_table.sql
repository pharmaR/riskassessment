CREATE TABLE IF NOT EXISTS comments (
   id 		     CHAR NOT NULL, 
   user_name 	 CHAR,
   user_role 	 CHAR,
   comment 		 CHAR,
   comment_type CHAR,
   added_on 	 DATE
);

CREATE TABLE IF NOT EXISTS comments_audit_log (
   id             INTEGER PRIMARY KEY AUTOINCREMENT,
   comment_id     CHAR NOT NULL, 
   old_row_data   JSON,
   new_row_data   JSON,
   dml_type       CHAR NOT NULL,
   dml_timestamp  DATETIME DEFAULT UTC_TIMESTAMP,
   dml_created_by CHAR
);

CREATE TRIGGER comments_update_audit_trigger
AFTER UPDATE ON comments FOR EACH ROW
BEGIN
  INSERT INTO comments_audit_log (
    comment_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    NEW.id,
    JSON_OBJECT(
      "user_name", OLD.user_name,
      "user_role", OLD.user_role,
      "comment", OLD.comment,
      "comment_type", OLD.comment_type,
      "added_on", OLD.added_on
    ),
    JSON_OBJECT(
      "user_name", NEW.user_name,
      "user_role", NEW.user_role,
      "comment", NEW.comment,
      "comment_type", NEW.comment_type,
      "added_on", NEW.added_on
    ),
    'UPDATE',
    (SELECT user FROM _variables LIMIT 1)
  );
END

CREATE TRIGGER comments_delete_audit_trigger
AFTER DELETE ON comments FOR EACH ROW
BEGIN
  INSERT INTO comments_audit_log (
    comment_id,
    old_row_data,
    new_row_data,
    dml_type,
    dml_created_by
  )
  VALUES(
    OLD.id,
    JSON_OBJECT(
      "user_name", OLD.user_name,
      "user_role", OLD.user_role,
      "comment", OLD.comment,
      "comment_type", OLD.comment_type,
      "added_on", OLD.added_on
    ),
    NULL,
    'DELETE',
    (SELECT user FROM _variables LIMIT 1)
  );
END