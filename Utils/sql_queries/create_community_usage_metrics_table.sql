CREATE TABLE IF NOT EXISTS community_usage_metrics (
  cum_id 					           CHAR NOT NULL, 
  month					             INT, /* Format: 01 */
  year					             INT, /* Format: 2022 */
  downloads 			           INT,
  version    				         CHAR,
  FOREIGN KEY(cum_id) REFERENCES package(name)
);