CREATE TABLE IF NOT EXISTS metric(
   id           INTEGER PRIMARY KEY AUTOINCREMENT,
   name         CHAR, 
   as_label     CHAR, /* label from pkg_assess() */
   sc_descr     CHAR, /* label from pkg_score()  */
   is_thumb     INT,  /* SQLite does not have a separate Boolean storage class*/
   class        CHAR, /* class = maintenance or test */
   weight       REAL   /* initialized to 1, but could be any number from 0-1 */
);

