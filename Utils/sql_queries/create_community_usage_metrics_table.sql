CREATE TABLE IF NOT EXISTS community_usage_metrics (
  id 					               CHAR NOT NULL, 
  month					             INT, /* Format: 01 */
  year					             INT, /* Format: 2022 */
  downloads 			           INT,
  version    				         CHAR,
  FOREIGN KEY(id) REFERENCES package(name)
);