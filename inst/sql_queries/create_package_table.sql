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
   url            CHAR,
   FOREIGN KEY (decision_id) REFERENCES decision_categories(id)
);