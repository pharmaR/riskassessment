CREATE TABLE IF NOT EXISTS rules (
   id                        INTEGER PRIMARY KEY AUTOINCREMENT,
   rule_type                 CHAR,
   metric_id                 INT DEFAULT 0 NOT NULL,
   condition                 CHAR,
   decision_id               INT DEFAULT 0 NOT NULL,
   FOREIGN KEY (metric_id)   REFERENCES metric(id)
   FOREIGN KEY (decision_id) REFERENCES decision_categories(id)
);