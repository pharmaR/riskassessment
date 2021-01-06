CREATE TABLE IF NOT EXISTS package(
   id             INTEGER PRIMARY KEY AUTOINCREMENT,
   name           CHAR, 
   version        CHAR,
   title          CHAR,
   description    TEXT,
   maintainer     CHAR,
   author         CHAR,
   license        CHAR,
   published_on   CHAR,
   score          INT,
   weigthed_score INT,
   decision       CHAR
);