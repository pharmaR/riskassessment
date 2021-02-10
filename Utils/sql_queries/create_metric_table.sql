CREATE TABLE IF NOT EXISTS metric(
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   name         CHAR, 
   comment      CHAR, /* derived from maint metrics code */
   label        CHAR, /* label from pkg_assess() */
   description  CHAR, /* label from pkg_score()  */
   info_type    CHAR, /* currently "percent" or "binary" */
   class        CHAR, /* currently "maintenance" or "test" */
   weight       REAL
);
