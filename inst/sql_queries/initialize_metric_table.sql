INSERT INTO metric 
(name, long_name, description, is_perc, is_url, class, weight)
VALUES 
('has_vignettes',       'Vignettes',         'Number of vignettes',              0, 0, 'maintenance', 1),
('has_news',            'NEWS file',         'Number of NEWS files',             0, 0, 'maintenance', 1),
('news_current',        'NEWS current',      'NEWS contains current version',    0, 0, 'maintenance', 1),
('has_bug_reports_url', 'Report Bugs',       'URL to report bugs exists',    0, 0, 'maintenance', 1),
('has_website',         'Website',           'Package public website',           0, 1, 'maintenance', 1),
('has_maintainer',      'Maintainer',        'Package maintainers',              0, 0, 'maintenance', 1),
('has_source_control',  'Source Control',    'Package source control url',       0, 1, 'maintenance', 1),
('export_help',         'Documentation',     '% of documented objects',          1, 0, 'maintenance', 1),
('bugs_status',         'Bugs Closure Rate', '% of the last 30 bugs closed',     1, 0, 'maintenance', 1),
('license',             'License',           "Package's license",                0, 0, 'maintenance', 1),
('covr_coverage',       'Test Coverage',     '% of objects tested',     1, 0, 'maintenance', 1),
('downloads_1yr ', 'Package Downloads', 'Number of downloads in last 12 months', 0, 0, 'community', 1);