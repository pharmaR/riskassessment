INSERT INTO metric 
(name, long_name, description, is_url, is_perc, class, weight)
VALUES 
('has_vignettes',       'Vignettes',         'Number of vignettes',              0, 0, 'maintenance', 1),
('has_news',            'NEWS file',         'Number of NEWS files',             0, 0, 'maintenance', 1),
('news_current',        'NEWS current',      'NEWS contains current version',    0, 0, 'maintenance', 1),
('has_bug_reports_url', 'Report Bugs',       'URL to report bugs exists',        1, 0, 'maintenance', 1),
('has_website',         'Website',           'Package public website',           1, 0, 'maintenance', 1),
('has_maintainer',      'Maintainer',        'Package maintainers',              0, 0, 'maintenance', 1),
('has_source_control',  'Source Control',    'Package source control url',       1, 0, 'maintenance', 1),
('export_help',         'Documentation',     '% of documented objects',          0, 1, 'maintenance', 1),
('bugs_status',         'Bugs Closure Rate', '% of the last 30 bugs closed',     0, 1, 'maintenance', 1),
('license',             'License',           "Package's license",                0, 0, 'maintenance', 1),
('dependencies',        'Dependencies',      'Number of Package Dependencies',   0, 0, 'maintenance', 1),  
('reverse_dependencies','Reverse Dependencies', 'Number of Reverse Dependencies', 0, 0, 'maintenance', 1),
('covr_coverage',       'Test Coverage',     'Percentage of objects tested',     0, 1, 'test', 0),
('downloads_1yr',       'Downloads',         'Number of package downloads in the last year', 0, 0, 'community', 1);
