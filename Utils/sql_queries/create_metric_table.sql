CREATE TABLE IF NOT EXISTS metric(
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   name         CHAR, 
   description  CHAR,
   class        CHAR, /*class = maintenance or code_coverage*/
   weight       INT
);