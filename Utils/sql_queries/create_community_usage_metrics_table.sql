CREATE TABLE IF NOT EXISTS community_usage_metrics (
  cum_id 					           CHAR NOT NULL, 
  month_year					       CHAR, /* Format: Jun 2022 */
  no_of_downloads 			     INT,
  ver_release 				       CHAR,
  time_since_first_release   INT, /* Number of months since the first package release.*/
  time_since_version_release INT, /* Number of months since last version release.*/
  FOREIGN KEY(cum_id) REFERENCES package(name)
);