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
   decision       CHAR,
   date_added     DATE
);