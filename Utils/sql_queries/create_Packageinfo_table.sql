CREATE TABLE IF NOT EXISTS Packageinfo(
   package        CHAR PRIMARY KEY NOT NULL, 
   version        CHAR,
   title          CHAR,
   description    TEXT,
   maintainer     CHAR,
   author         CHAR,
   license        CHAR,
   published_on   CHAR,
   score          INT,
   decision       CHAR
);