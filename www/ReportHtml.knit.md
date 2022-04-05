---
title:    "R Package Risk Assessment"
subtitle: "Report for stringr"
author:   "Author (Role): admin (admin)"
date:     "Report Date: April 05, 2022"
always_allow_html: true
output: 
  html_document:
    theme:
      bootswatch: lux
runtime: shiny
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

`<h5>General Information</h5>`{=html}
```{=html}
<h6>Package:</h6>
stringr
<h6>Version:</h6>
1.4.0
<h6>Title:</h6>
stringr: Simple, Consistent Wrappers for Common String Operations
<h6>Description:</h6>
A consistent, simple and easy to use set of   wrappers around the fantastic stringi package. All function and   argument names (and positions) are consistent, all functions deal with   NAs and zero length vectors in the same way, and the output from   one function is easy to feed into the input of another.
<h6>Author:</h6>
Hadley Wickham [aut, cre, cph], RStudio [cph, fnd]
<h6>Maintainer:</h6>
Hadley Wickham  &lt;hadley at rstudio.com&gt;
<h6>License:</h6>
GPL-2 | file LICENSE
<h6>Published:</h6>
2019-02-10
<h6>Overall Risk:</h6>
Pending
```



```{=html}
<h5 style="padding-bottom:10px;">Overall Comments</h5>
<div class="well">No comments</div>
```

<br>
<hr>
<br>


```{=html}
<br/>
<h5 style="text-align: center;">Maintenance Metrics</h5>
<br/>
<br/>
<div class="row card-group" style="padding-right: 10px">
<div class="col-sm-4">
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Vignettes</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">2</p>
</div>
</div>
<div class="card-footer bg-transparent">Number of vignettes</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">NEWS file</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">1</p>
</div>
</div>
<div class="card-footer bg-transparent">Number of NEWS files</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">NEWS current</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">Yes</p>
</div>
</div>
<div class="card-footer bg-transparent">NEWS contains current version</div>
</div>
</div>
</div>
<div class="col-sm-4">
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Report Bugs</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">
<a href="https://github.com/tidyverse/stringr/issues">https://github.com/tidyverse/...</a>
</p>
</div>
</div>
<div class="card-footer bg-transparent">Public url to report bugs</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Website</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">
<a href="http://stringr.tidyverse.org">http://stringr.tidyverse.org...</a>
</p>
</div>
</div>
<div class="card-footer bg-transparent">Package public website</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Maintainer</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">Hadley Wickham &lt;hadley@rstudio.com&gt;</p>
</div>
</div>
<div class="card-footer bg-transparent">Package maintainers</div>
</div>
</div>
</div>
<div class="col-sm-4">
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Source Control</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">
<a href="https://github.com/tidyverse/stringr">https://github.com/tidyverse/...</a>
</p>
</div>
</div>
<div class="card-footer bg-transparent">Package source control url</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-percent text-info" role="presentation" aria-label="percent icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Documentation</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">100%</p>
</div>
</div>
<div class="card-footer bg-transparent">% of documented objects</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-percent text-info" role="presentation" aria-label="percent icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Bugs Closure Rate</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">53.3%</p>
</div>
</div>
<div class="card-footer bg-transparent">% of the last 30 bugs closed</div>
</div>
</div>
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-check text-success" role="presentation" aria-label="check icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">License</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">GPL-2 | file LICENSE</p>
</div>
</div>
<div class="card-footer bg-transparent">Package's license</div>
</div>
</div>
</div>
</div>
<br/>
<br/>
<h5 style="padding-bottom:10px;">Comments</h5>
<div class="well"><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 17:23:23 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.12.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 14:18:47 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.11.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 14:18:19 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 10:00:35 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.09.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 09:59:25 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1. Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:17:05 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.11.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:15:51 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:13:50 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.12.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:05:33 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.13.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-22; 09:35:43 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-22; 09:28:22 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-22; 09:10:46 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.13.</div></div>
```

<br>
<hr>
<br>


```{=html}
<br/>
<h5 style="text-align: center;">Community Usage Metrics</h5>
<br/>
<br/>
<div class="row card-group" style="padding-right: 10px">
<div class="col-sm-4">
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fab fa-black-tie text-info" role="presentation" aria-label="black-tie icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">First Version Release</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">12 Years Ago</p>
</div>
</div>
<div class="card-footer bg-transparent">Time passed since first version release</div>
</div>
</div>
</div>
<div class="col-sm-4">
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-meteor text-info" role="presentation" aria-label="meteor icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Latest Version Release</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">3 Years Ago</p>
</div>
</div>
<div class="card-footer bg-transparent">Time passed since latest version release</div>
</div>
</div>
</div>
<div class="col-sm-4">
<div class="card mb-3 text-center border-info" style="max-width: 400px; max-height: 250px; overflow-y: scroll;">
<div class="row no-gutters">
<div class="col-md-4 text-center border-info">
<i class="fa fa-box-open text-info" role="presentation" aria-label="box-open icon" style="padding-top: 40%; font-size:60px; padding-left: 20%;"></i>
</div>
<div class="col-md-8">
<h5 class="card-header bg-transparent" style="font-size: 1vw">Package Downloads</h5>
<div class="card-body text-info">
<p class="card-title" style="font-size: 1.5vw">9,841,507</p>
</div>
</div>
<div class="card-footer bg-transparent">Number of downloads since last year</div>
</div>
</div>
</div>
</div>
<br/>
<br/>
<div class="row">
<div class="col-sm-12" style="padding-left: 20px; padding-right: 20px;" height="500px">
<div id="htmlwidget-e09791125678e0cd9cc2" style="width:100%;height:400px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-e09791125678e0cd9cc2">{"x":{"visdat":{"56b4753995d":["function () ","plotlyVisDat"]},"cur_data":"56b4753995d","attrs":{"56b4753995d":{"x":{},"y":{},"mode":"lines+markers","line":{"color":"#1F9BCF"},"marker":{"color":"#1F9BCF"},"hoverinfo":"text","text":{},"name":"# Downloads","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"56b4753995d.1":{"x":{},"y":{},"mode":"lines","line":{"color":"#4BBF73"},"marker":{"color":"#1F9BCF"},"hoverinfo":"text","text":{},"name":"Version Release","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","xend":{},"yend":{},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":100,"r":10},"title":"NUMBER OF DOWNLOADS BY MONTH: stringr","showlegend":false,"yaxis":{"domain":[0,1],"automargin":true,"title":"Downloads"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"date","tickformat":"%b %Y","range":[1255737600000,1644969600000],"rangeselector":{"buttons":[{"count":148,"label":"First Release","step":"month","stepmode":"todate"},{"count":37,"label":"Last Release","step":"month","stepmode":"backward"},{"count":25,"label":"2 yr","step":"month","stepmode":"backward"},{"count":13,"label":"1 yr","step":"month","stepmode":"backward"},{"count":7,"label":"6 mo","step":"month","stepmode":"backward"}]},"rangeslider":{"visible":true}},"annotations":[{"text":"0.1.10","yref":"paper","xref":"x","y":0.5,"x":"2009-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2009-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"0.3","yref":"paper","xref":"x","y":0.5,"x":"2010-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"0.4","yref":"paper","xref":"x","y":0.5,"x":"2010-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2010-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"0.5","yref":"paper","xref":"x","y":0.5,"x":"2011-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2011-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"0.6","yref":"paper","xref":"x","y":0.5,"x":"2011-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"0.6.1","yref":"paper","xref":"x","y":0.5,"x":"2012-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2012-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"0.6.2","yref":"paper","xref":"x","y":0.5,"x":"2012-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2013-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2014-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"1.0.0","yref":"paper","xref":"x","y":0.5,"x":"2015-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2015-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"1.1.0","yref":"paper","xref":"x","y":0.5,"x":"2016-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2016-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"1.2.0","yref":"paper","xref":"x","y":0.5,"x":"2017-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2017-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"1.3.0","yref":"paper","xref":"x","y":0.5,"x":"2018-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"1.3.1","yref":"paper","xref":"x","y":0.5,"x":"2018-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2018-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"1.4.0","yref":"paper","xref":"x","y":0.5,"x":"2019-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2019-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2020-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-03-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-04-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-05-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-06-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-07-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-08-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-09-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-10-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-11-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2021-12-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2022-01-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}},{"text":"","yref":"paper","xref":"x","y":0.5,"x":"2022-02-01","xanchor":"left","showarrow":false,"textangle":270,"font":{"size":14,"color":"#4BBF73"}}],"hovermode":"closest"},"source":"A","config":{"showSendToCloud":false,"displayModeBar":false},"data":[{"x":["2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-04-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01","2013-03-01","2013-04-01","2013-05-01","2013-06-01","2013-07-01","2013-08-01","2013-09-01","2013-10-01","2013-11-01","2013-12-01","2014-01-01","2014-02-01","2014-03-01","2014-04-01","2014-05-01","2014-06-01","2014-07-01","2014-08-01","2014-09-01","2014-10-01","2014-11-01","2014-12-01","2015-01-01","2015-02-01","2015-03-01","2015-04-01","2015-05-01","2015-06-01","2015-07-01","2015-08-01","2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01","2016-04-01","2016-05-01","2016-06-01","2016-07-01","2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01","2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01","2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01","2022-01-01","2022-02-01"],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,5173,7017,13019,13720,14465,17165,16875,16156,16843,17282,25195,35136,31737,23126,31995,34987,42337,59535,51117,65076,52635,54874,68406,77349,71452,62902,86606,87199,103874,120387,156249,148417,126629,131739,173214,174954,182530,142077,170622,200288,189641,171733,214283,202258,183439,262413,333957,312524,316208,233963,294737,358604,407488,343436,347352,305910,273001,283396,362517,400733,387092,292983,374919,394333,525094,537827,566011,480896,469071,438523,571410,656926,593478,440698,529074,625415,672469,610084,639266,502004,548150,580984,771141,817077,816277,623875,755079,704976,736390,806662,765502,723609,638452,651753,832061,894916,770111,631944,723434,735999,891968,787462,757494,785925,732012,788664,926589,972805,932158,806997,849399,846102],"mode":"lines+markers","line":{"color":"#1F9BCF"},"marker":{"color":"#1F9BCF","line":{"color":"rgba(31,119,180,1)"}},"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"text":["No. of Downloads:       0<br />November 2009","No. of Downloads:       0<br />December 2009","No. of Downloads:       0<br />January 2010","No. of Downloads:       0<br />February 2010","No. of Downloads:       0<br />March 2010","No. of Downloads:       0<br />April 2010","No. of Downloads:       0<br />May 2010","No. of Downloads:       0<br />June 2010","No. of Downloads:       0<br />July 2010","No. of Downloads:       0<br />August 2010","No. of Downloads:       0<br />September 2010","No. of Downloads:       0<br />October 2010","No. of Downloads:       0<br />November 2010","No. of Downloads:       0<br />December 2010","No. of Downloads:       0<br />January 2011","No. of Downloads:       0<br />February 2011","No. of Downloads:       0<br />March 2011","No. of Downloads:       0<br />April 2011","No. of Downloads:       0<br />May 2011","No. of Downloads:       0<br />June 2011","No. of Downloads:       0<br />July 2011","No. of Downloads:       0<br />August 2011","No. of Downloads:       0<br />September 2011","No. of Downloads:       0<br />October 2011","No. of Downloads:       0<br />November 2011","No. of Downloads:       0<br />December 2011","No. of Downloads:       0<br />January 2012","No. of Downloads:       0<br />February 2012","No. of Downloads:       0<br />March 2012","No. of Downloads:       0<br />April 2012","No. of Downloads:       0<br />May 2012","No. of Downloads:       0<br />June 2012","No. of Downloads:       0<br />July 2012","No. of Downloads:       0<br />August 2012","No. of Downloads:       0<br />September 2012","No. of Downloads:     125<br />October 2012","No. of Downloads:   5,173<br />November 2012","No. of Downloads:   7,017<br />December 2012","No. of Downloads:  13,019<br />January 2013","No. of Downloads:  13,720<br />February 2013","No. of Downloads:  14,465<br />March 2013","No. of Downloads:  17,165<br />April 2013","No. of Downloads:  16,875<br />May 2013","No. of Downloads:  16,156<br />June 2013","No. of Downloads:  16,843<br />July 2013","No. of Downloads:  17,282<br />August 2013","No. of Downloads:  25,195<br />September 2013","No. of Downloads:  35,136<br />October 2013","No. of Downloads:  31,737<br />November 2013","No. of Downloads:  23,126<br />December 2013","No. of Downloads:  31,995<br />January 2014","No. of Downloads:  34,987<br />February 2014","No. of Downloads:  42,337<br />March 2014","No. of Downloads:  59,535<br />April 2014","No. of Downloads:  51,117<br />May 2014","No. of Downloads:  65,076<br />June 2014","No. of Downloads:  52,635<br />July 2014","No. of Downloads:  54,874<br />August 2014","No. of Downloads:  68,406<br />September 2014","No. of Downloads:  77,349<br />October 2014","No. of Downloads:  71,452<br />November 2014","No. of Downloads:  62,902<br />December 2014","No. of Downloads:  86,606<br />January 2015","No. of Downloads:  87,199<br />February 2015","No. of Downloads: 103,874<br />March 2015","No. of Downloads: 120,387<br />April 2015","No. of Downloads: 156,249<br />May 2015","No. of Downloads: 148,417<br />June 2015","No. of Downloads: 126,629<br />July 2015","No. of Downloads: 131,739<br />August 2015","No. of Downloads: 173,214<br />September 2015","No. of Downloads: 174,954<br />October 2015","No. of Downloads: 182,530<br />November 2015","No. of Downloads: 142,077<br />December 2015","No. of Downloads: 170,622<br />January 2016","No. of Downloads: 200,288<br />February 2016","No. of Downloads: 189,641<br />March 2016","No. of Downloads: 171,733<br />April 2016","No. of Downloads: 214,283<br />May 2016","No. of Downloads: 202,258<br />June 2016","No. of Downloads: 183,439<br />July 2016","No. of Downloads: 262,413<br />August 2016","No. of Downloads: 333,957<br />September 2016","No. of Downloads: 312,524<br />October 2016","No. of Downloads: 316,208<br />November 2016","No. of Downloads: 233,963<br />December 2016","No. of Downloads: 294,737<br />January 2017","No. of Downloads: 358,604<br />February 2017","No. of Downloads: 407,488<br />March 2017","No. of Downloads: 343,436<br />April 2017","No. of Downloads: 347,352<br />May 2017","No. of Downloads: 305,910<br />June 2017","No. of Downloads: 273,001<br />July 2017","No. of Downloads: 283,396<br />August 2017","No. of Downloads: 362,517<br />September 2017","No. of Downloads: 400,733<br />October 2017","No. of Downloads: 387,092<br />November 2017","No. of Downloads: 292,983<br />December 2017","No. of Downloads: 374,919<br />January 2018","No. of Downloads: 394,333<br />February 2018","No. of Downloads: 525,094<br />March 2018","No. of Downloads: 537,827<br />April 2018","No. of Downloads: 566,011<br />May 2018","No. of Downloads: 480,896<br />June 2018","No. of Downloads: 469,071<br />July 2018","No. of Downloads: 438,523<br />August 2018","No. of Downloads: 571,410<br />September 2018","No. of Downloads: 656,926<br />October 2018","No. of Downloads: 593,478<br />November 2018","No. of Downloads: 440,698<br />December 2018","No. of Downloads: 529,074<br />January 2019","No. of Downloads: 625,415<br />February 2019","No. of Downloads: 672,469<br />March 2019","No. of Downloads: 610,084<br />April 2019","No. of Downloads: 639,266<br />May 2019","No. of Downloads: 502,004<br />June 2019","No. of Downloads: 548,150<br />July 2019","No. of Downloads: 580,984<br />August 2019","No. of Downloads: 771,141<br />September 2019","No. of Downloads: 817,077<br />October 2019","No. of Downloads: 816,277<br />November 2019","No. of Downloads: 623,875<br />December 2019","No. of Downloads: 755,079<br />January 2020","No. of Downloads: 704,976<br />February 2020","No. of Downloads: 736,390<br />March 2020","No. of Downloads: 806,662<br />April 2020","No. of Downloads: 765,502<br />May 2020","No. of Downloads: 723,609<br />June 2020","No. of Downloads: 638,452<br />July 2020","No. of Downloads: 651,753<br />August 2020","No. of Downloads: 832,061<br />September 2020","No. of Downloads: 894,916<br />October 2020","No. of Downloads: 770,111<br />November 2020","No. of Downloads: 631,944<br />December 2020","No. of Downloads: 723,434<br />January 2021","No. of Downloads: 735,999<br />February 2021","No. of Downloads: 891,968<br />March 2021","No. of Downloads: 787,462<br />April 2021","No. of Downloads: 757,494<br />May 2021","No. of Downloads: 785,925<br />June 2021","No. of Downloads: 732,012<br />July 2021","No. of Downloads: 788,664<br />August 2021","No. of Downloads: 926,589<br />September 2021","No. of Downloads: 972,805<br />October 2021","No. of Downloads: 932,158<br />November 2021","No. of Downloads: 806,997<br />December 2021","No. of Downloads: 849,399<br />January 2022","No. of Downloads: 846,102<br />February 2022"],"name":"# Downloads","type":"scatter","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-11-01","2009-11-01",null,"2010-02-01","2010-02-01",null,"2010-08-01","2010-08-01",null,"2011-06-01","2011-06-01",null,"2011-12-01","2011-12-01",null,"2012-07-01","2012-07-01",null,"2012-12-01","2012-12-01",null,"2015-04-01","2015-04-01",null,"2016-08-01","2016-08-01",null,"2017-02-01","2017-02-01",null,"2018-02-01","2018-02-01",null,"2018-05-01","2018-05-01",null,"2019-02-01","2019-02-01"],"y":[0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1,null,0,992261.1],"mode":"lines+markers","line":{"color":"#4BBF73"},"marker":{"color":"#1F9BCF","line":{"color":"rgba(255,127,14,1)"}},"hoverinfo":["text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text"],"text":["Version 0.1.10","Version 0.1.10",null,"Version 0.3","Version 0.3",null,"Version 0.4","Version 0.4",null,"Version 0.5","Version 0.5",null,"Version 0.6","Version 0.6",null,"Version 0.6.1","Version 0.6.1",null,"Version 0.6.2","Version 0.6.2",null,"Version 1.0.0","Version 1.0.0",null,"Version 1.1.0","Version 1.1.0",null,"Version 1.2.0","Version 1.2.0",null,"Version 1.3.0","Version 1.3.0",null,"Version 1.3.1","Version 1.3.1",null,"Version 1.4.0","Version 1.4.0"],"name":"Version Release","type":"scatter","error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
</div>
</div>
<br/>
<br/>
<h5 style="padding-bottom:10px;">Comments</h5>
<div class="well"><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 17:23:23 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.12.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 14:18:47 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.11.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 14:18:19 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 10:00:35 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.09.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-28; 09:59:25 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1. Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:17:05 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.11.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:15:51 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:13:50 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.12.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-24; 11:05:33 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.13.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-22; 09:35:43 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-22; 09:28:22 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.1.</div><div class='well'><i class="fa fa-user-tie" role="presentation" aria-label="user-tie icon"></i> user: admin, <i class="fa fa-user-shield" role="presentation" aria-label="user-shield icon"></i> role: admin, <i class="fa fa-calendar-alt" role="presentation" aria-label="calendar-alt icon"></i> date: 2022-03-22; 09:10:46 America/New_York<br/><br/>Metric re-weighting has occurred.
The previous risk score was 0.13.</div></div>
```

<style>
.title {
font-size: 1.5rem;
}

.subtitle, .author, .date {
font-size: 1.1rem;
}
</style>
