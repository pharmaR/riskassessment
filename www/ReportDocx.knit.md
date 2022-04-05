---
title:    "R Package Risk Assessment"
subtitle: "Report for samplesizeCMH"
author:   "Author (Role): admin (admin)"
date:     "Report Date: April 05, 2022"
always_allow_html: true
output:
  word_document:
    md_extensions: +raw_html-markdown_in_html_blocks
    pandoc_args: ['--lua-filter', 'read_html.lua']
params:
  pkg: NA
  riskmetric_version: NA
  user_name: NA
  user_role: NA
  overall_comments: NA
  mm_comments: NA
  cm_comments: NA
  maint_metrics: NA
  com_metrics: NA
  com_metrics_raw: NA
  downloads_plot_data: NA
---






<br>


```{=html}
<strong>Package:</strong>
<br/>
samplesizeCMH
<br/>
<br/>
<strong>Version:</strong>
<br/>
0.0.0
<br/>
<br/>
<strong>Title:</strong>
<br/>
samplesizeCMH: Power and Sample Size Calculation for the
Cochran-Mantel-Haenszel Test
<br/>
<br/>
<strong>Description:</strong>
<br/>
   Calculates the power and sample size for Cochran-Mantel-Haenszel tests.    There are also several helper functions for working with probability,   odds, relative risk, and odds ratio values.
<br/>
<br/>
<strong>Author:</strong>
<br/>
Paul Egeler [aut, cre], Spectrum Health, Grand Rapids, MI [cph]
<br/>
<br/>
<strong>Maintainer:</strong>
<br/>
Paul Egeler  &lt;paul.egeler at spectrumhealth.org&gt;
<br/>
<br/>
<strong>License:</strong>
<br/>
GPL-2 | GPL-3
<br/>
<br/>
<strong>Published:</strong>
<br/>
2017-12-21
<br/>
<br/>
<strong>Overall Risk:</strong>
<br/>
Pending
```



```{=html}
<h2 style="padding-bottom:10px;">Overall Comments</h2>
<div class="well"><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-30; 17:09:54 America/New_York<br>kk's</div></div>
```


\newpage

```{=html}
<br/>
<h2>Maintenance Metrics</h2>
```



Metric Name         Metric Description              Metric Value                                     
------------------  ------------------------------  -------------------------------------------------
Vignettes           Number of vignettes             3                                                
NEWS file           Number of NEWS files            0                                                
NEWS current        NEWS contains current version   NA                                               
Report Bugs         Public url to report bugs       https://github.com/pegeler/samplesizeCMH/issues  
Website             Package public website          https://github.com/pegeler/samplesizeCMH         
Maintainer          Package maintainers             Paul Egeler  <paul.egeler at spectrumhealth.org> 
Source Control      Package source control url      https://github.com/pegeler/samplesizeCMH         
Documentation       % of documented objects         NA                                               
Bugs Closure Rate   % of the last 30 bugs closed    0                                                
License             Package's license               GPL-2 | GPL-3                                    



```{=html}
<br/>
<h2>Comments</h2>
<div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-30; 17:09:12 America/New_York<br>he's got a loud snore</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-30; 17:09:00 America/New_York<br>found it in "my" closet</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 17:23:21 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.58.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 14:18:45 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 14:18:19 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 10:00:35 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.44. Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 09:59:24 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46. Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:17:05 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:15:50 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.57.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:13:47 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.5.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:05:33 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-22; 09:35:41 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-22; 09:28:22 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-22; 09:10:46 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div>
```


\newpage

```{=html}
<br/>
<h2>Community Usage Metrics</h2>
```



Metric Name              Metric Description                         Metric Value 
-----------------------  -----------------------------------------  -------------
First Version Release    Time passed since first version release    4 Years Ago  
Latest Version Release   Time passed since latest version release   4 Years Ago  
Package Downloads        Number of downloads since last year        5,359        



```{=html}
<br/>
<h2>Number of Downloads by Month/Year</h2>
```


![](C:/Users/aclark5/AppData/Local/Temp/1/Rtmp4QsLPF/samplesizeCMH_0.0.0_Risk_Assessment_files/figure-docx/community_metrics_plot-1.png)<!-- -->



```{=html}
<br/>
<h2>Comments</h2>
<div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-30; 17:09:30 America/New_York<br>hey!</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-30; 17:09:25 America/New_York<br>farter's</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 17:23:21 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.58.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 14:18:45 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 14:18:19 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 10:00:35 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.44. Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-28; 09:59:24 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46. Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:17:05 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:15:50 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.57.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:13:47 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.5.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-24; 11:05:33 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-22; 09:35:41 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-22; 09:28:22 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.46.</div><div class='well'><img src='images/user-tie.png' width='16' height='16'> user: admin, <img src='images/user-shield.png' width='16' height='16'> role: admin, <img src='images/calendar-alt.png' width='16' height='16'> date: 2022-03-22; 09:10:46 America/New_York<br>Metric re-weighting has occurred.
The previous risk score was 0.54.</div>
```

