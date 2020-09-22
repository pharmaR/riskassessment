CREATE TABLE IF NOT EXISTS Packageinfo(
   package        CHAR NOT NULL, 
   version        CHAR NOT NULL,
   title          CHAR,
   description    TEXT,
   maintainer     CHAR,
   author         CHAR,
   license        CHAR,
   published_on   CHAR,
   score          INT,
   decision       CHAR,
   primary key(package, version)
);
CREATE UNIQUE INDEX pack_idx ON Packageinfo(package, version);
);