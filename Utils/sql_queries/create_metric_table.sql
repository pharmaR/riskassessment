CREATE TABLE IF NOT EXISTS metric(
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   name         CHAR, 
   description  CHAR,
   class        CHAR, /*class = maintenance or test*/
   weight       INT
);