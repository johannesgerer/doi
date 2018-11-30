# doi [![Build Status](https://travis-ci.org/johannesgerer/doi.svg?branch=master)](https://travis-ci.org/johannesgerer/doi) [![Hackage](https://img.shields.io/hackage/v/doi.svg)](https://hackage.haskell.org/package/doi)
Extract Bibtex entries and download fulltext of scientific articles automatically for a given DOI or URL

Read the (sparse) documentation on [Hackage](https://hackage.haskell.org/package/doi/docs/DOI.html).

# Installation

Clone this repo and run `stack install` in the cloned directory.

# Usage

```shell
johannes@debussy /tmp$ doi http://link.springer.com/article/10.1007/s11846-016-0219-7          
Downloading Bibsonomy BibTeX for http://link.springer.com/article/10.1007/s11846-016-0219-7


curl -L 'http://localhost:8080/service?format=bibtex&selection=&url=http%3A%2F%2Flink.springer.com%2Farticle%2F10.1007%2Fs11846-016-0219-7'

  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1666  100  1666    0     0  38877      0 --:--:-- --:--:-- --:--:-- 39666
"@Article{Vel\225squez2016,\nauthor=\"Vel{\\'a}squez, Santiago\nand Kanniainen, Juho\nand M{\\\"a}kinen,Saku\nand Valli, Jaakko\",\ntitle=\"Layoff announcements and intra-day market reactions\",\njournal=\"Review of Managerial Science\",\nyear=\"2016\",\npages=\"1--26\",\nabstract=\"This paper examines investor intra-day reactions related to two types of layoff announcements, the first one at the start of layoff negotiations and the other at the final layoff decisions. We provide statistically significant evidence that, on average, investors have strongly negative reaction to layoff negotiations within the first 10\160min. However, we also provide strong evidence that the first negative reaction is reversed by an upward post-drift in aggregated cumulative abnormal returns in the following hours, perhaps because markets need hours to process such unpredictable and complex information and its consequences---even if their first reaction was strong and immediate. Moreover, on the aggregated level, final layoff announcements do not generally convey information that is exceptionally useful to investors, except when reactions to associated initial announcements have not been statistically significant. Importantly, our analysis demonstrates the importance of the use of intra-day data: The reactions, which can be strong but short-lived, are identifiable with intra-day data only. Finally, we find that intra-day reactions cannot be explained by various company background characteristics, such as the number of employees, sales, profitability, and assets/liabilities ratio.\",\nissn=\"1863-6691\",\ndoi=\"10.1007/s11846-016-0219-7\",\nurl=\"https://doi.org/10.1007/s11846-016-0219-7\"\n}\n\n"

Downloading CrossRef BibTeX for https://doi.org/10.1007/s11846-016-0219-7


curl -LH 'Accept: application/x-bibtex' 'https://doi.org/10.1007/s11846-016-0219-7'

  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   189  100   189    0     0    820      0 --:--:-- --:--:-- --:--:--   818
100   377  100   377    0     0    671      0 --:--:-- --:--:-- --:--:--   671
"@article{Vel_squez_2016,\n\tdoi = {10.1007/s11846-016-0219-7},\n\turl = {https://doi.org/10.1007%2Fs11846-016-0219-7},\n\tyear = 2016,\n\tmonth = {oct},\n\tpublisher = {Springer Nature},\n\tauthor = {SantiagoVel{\\'{a}}squez and Juho Kanniainen and Saku M\228kinen and Jaakko Valli},\n\ttitle = {Layoff announcements and intra-day market reactions},\n\tjournal = {Review of Managerial Science}\n}"

Downloading Website for 10.1007/s11846-016-0219-7

/tmp/wget_doi1804289383846930886
--2017-01-22 17:27:18--  https://doi.org/10.1007/s11846-016-0219-7
Resolving dx.doi.org (dx.doi.org)... 2a00:1a48:7805:113:be76:4eff:fe09:2bc4, 2001:550:100:6::138:163, 2001:550:100:6::138:162, ...
Connecting to dx.doi.org (dx.doi.org)|2a00:1a48:7805:113:be76:4eff:fe09:2bc4|:80... connected.
HTTP request sent, awaiting response... 303 See Other
Location: http://link.springer.com/10.1007/s11846-016-0219-7 [following]
--2017-01-22 17:27:18--  http://link.springer.com/10.1007/s11846-016-0219-7
Resolving link.springer.com (link.springer.com)... 104.85.247.183
Connecting to link.springer.com (link.springer.com)|104.85.247.183|:80... connected.
HTTP request sent, awaiting response... 302 Moved Temporarily
Location: http://link.springer.com/article/10.1007%2Fs11846-016-0219-7 [following]
--2017-01-22 17:27:18--  http://link.springer.com/article/10.1007%2Fs11846-016-0219-7
Reusing existing connection to link.springer.com:80.
HTTP request sent, awaiting response... 200 OK
Length: unspecified [text/html]
Saving to: ‘/tmp/wget_doi1804289383846930886’

/tmp/wget_doi1804289383846     [ <=>                                  ] 185.70K  --.-KB/s    in 0.08s

2017-01-22 17:27:19 (2.38 MB/s) - ‘/tmp/wget_doi1804289383846930886’ saved [190160]

Converting links in /tmp/wget_doi1804289383846930886... 192-45
Converted links in 1 files in 0.007 seconds.

Downloading PDF from for http://link.springer.com/content/pdf/10.1007%2Fs11846-016-0219-7.pdf

--2017-01-22 17:27:19--  http://link.springer.com/content/pdf/10.1007%2Fs11846-016-0219-7.pdf
Resolving link.springer.com (link.springer.com)... 104.85.247.183
Connecting to link.springer.com (link.springer.com)|104.85.247.183|:80... connected.
HTTP request sent, awaiting response... 302 Moved Temporarily
Location: http://download.springer.com/static/pdf/957/art%253A10.1007%252Fs11846-016-0219-7.pdf?originUrl=http%3A%2F%2Flink.springer.com%2Farticle%2F10.1007%2Fs11846-016-0219-7&token2=exp=1485103639~acl=%2Fstatic%2Fpdf%2F957%2Fart%25253A10.1007%25252Fs11846-016-0219-7.pdf%3ForiginUrl%3Dhttp%253A%252F%252Flink.springer.com%252Farticle%252F10.1007%252Fs11846-016-0219-7*~hmac=7630347ecb27ad8b54c163fd8925d7c3fe1abedd7707a5afb0cc57269c356527 [following]
--2017-01-22 17:27:19--  http://download.springer.com/static/pdf/957/art%253A10.1007%252Fs11846-016-0219-7.pdf?originUrl=http%3A%2F%2Flink.springer.com%2Farticle%2F10.1007%2Fs11846-016-0219-7&token2=exp=1485103639~acl=%2Fstatic%2Fpdf%2F957%2Fart%25253A10.1007%25252Fs11846-016-0219-7.pdf%3ForiginUrl%3Dhttp%253A%252F%252Flink.springer.com%252Farticle%252F10.1007%252Fs11846-016-0219-7*~hmac=7630347ecb27ad8b54c163fd8925d7c3fe1abedd7707a5afb0cc57269c356527
Resolving download.springer.com (download.springer.com)... 88.134.183.56, 88.134.183.16
Connecting to download.springer.com (download.springer.com)|88.134.183.56|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: 728231 (711K) [application/pdf]
Saving to: ‘/home/data/promotion/Literatur/doi/10.1007SLASHs11846-016-0219-7.pdf’

/home/data/promotion/Liter 100%[=====================================>] 711.16K  3.02MB/s    in 0.2s

2017-01-22 17:27:20 (3.02 MB/s) - ‘/home/data/promotion/Literatur/doi/10.1007SLASHs11846-016-0219-7.pdf’saved [728231/728231]

@Article{Velásquez2016,
  abstract = {This paper examines investor intra-day reactions related to two types of layoff announcements, the first one at the start of layoff negotiations and the other at the final layoff decisions. We provide statistically significant evidence that, on average, investors have strongly negative reaction to layoff negotiations within the first 10 min. However, we also provide strong evidence that the first negative reaction is reversed by an upward post-drift in aggregated cumulative abnormal returns in the following hours, perhaps because markets need hours to process such unpredictable and complex information and its consequences---even if their first reaction was strong and immediate. Moreover, on the aggregated level, final layoff announcements do not generally convey information that is exceptionally useful to investors, except when reactions to associated initial announcements have not been statistically significant. Importantly, our analysis demonstrates the importance of the use of intra-day data: The reactions, which canbe strong but short-lived, are identifiable with intra-day data only. Finally, we find that intra-day reactions cannot be explained by various company background characteristics, such as the number of employees, sales, profitability, and assets/liabilities ratio.},
  author = {Vel{\'a}squez, Santiago
and Kanniainen, Juho
and M{\"a}kinen, Saku
and Valli, Jaakko},
  author = {Santiago Vel{\'{a}}squez and Juho Kanniainen and Saku Mäkinen and Jaakko Valli},
  doi = {10.1007/s11846-016-0219-7},
  issn = {1863-6691},
  journal = {Review of Managerial Science},
  month = {oct},
  pages = {1--26},
  publisher = {Springer Nature},
  title = {Layoff announcements and intra-day market reactions},
  url = {https://doi.org/10.1007/s11846-016-0219-7},
  url = {https://doi.org/10.1007%2Fs11846-016-0219-7},
  year = {2016},
  file = {doi/10.1007SLASHs11846-016-0219-7.pdf},
  timestamp = {2017-01-22T17:27:20},
}
```
