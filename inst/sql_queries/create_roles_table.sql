CREATE TABLE IF NOT EXISTS roles (
   id                    INTEGER PRIMARY KEY AUTOINCREMENT,
   user_role 	           CHAR,
   admin                 INTEGER DEFAULT 0 NOT NULL,
   weight_adjust         INTEGER DEFAULT 0 NOT NULL,
   auto_decision_adjust  INTEGER DEFAULT 0 NOT NULL,
   final_decision        INTEGER DEFAULT 0 NOT NULL,
   revert_decision       INTEGER DEFAULT 0 NOT NULL,
   add_package           INTEGER DEFAULT 0 NOT NULL,
   delete_package        INTEGER DEFAULT 0 NOT NULL,
   overall_comment       INTEGER DEFAULT 0 NOT NULL,
   general_comment       INTEGER DEFAULT 0 NOT NULL
);