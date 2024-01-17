CREATE TABLE IF NOT EXISTS metric (
   id            INTEGER PRIMARY KEY AUTOINCREMENT,
   name          CHAR,
   long_name     CHAR,    /* Represents the title of the metrixBox. */
   is_perc       INTEGER, /* Indicates whether the metric value is a percentage. 0: indicates FALSE, 1: indicates TRUE */
   is_url        INTEGER, /* Indicates whether the metric value is a url. 0: indicates FALSE, 1: indicates TRUE */
   is_riskmetric INTEGER, /* Indicates whether the metric is a riskmetric metric. 0: indicates FALSE, 1: indicates TRUE */
   description   CHAR,
   class         CHAR,    /* class = maintenance or test */
   weight        REAL
);