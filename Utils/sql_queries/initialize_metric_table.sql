INSERT INTO metric (name, description, class, weight) values 
('news_current', 'News is current?', 'maintenance', 1),
('has_vignettes', 'Presence of vignettes?', 'maintenance', 1),
('has_bug_reports_url', 'Bugs publicly documented?', 'maintenance', 1),
('bugs_status', 'Percent of last 30 bugs closed', 'maintenance', 1),
('export_help', 'Percent of exported objects documented', 'maintenance', 1),
('has_website', 'Associated website URL?', 'maintenance', 1),
('has_maintainer', 'Has a maintainer?', 'maintenance', 1),
('has_news', 'Number of discovered NEWS files', 'maintenance', 1),
('has_source_control', 'Source code public?', 'maintenance', 1),
('covr_coverage', 'Package unit test coverage', 'test', 1);