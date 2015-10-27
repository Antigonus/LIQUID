#lang racket
(provide webi-dblp-scrape-test-data-0)

;; an dblp htmle page
;;

(define webi-dblp-scrape-test-data-0

  '(*TOP* 
    (*DECL* DOCTYPE html)
    "\n"
    (html
     (@ (lang "en"))
     "\n"
     (head
      (meta (@ (charset "UTF-8")))
      (title "dblp: CompleteSearch")
      (link (@
             (id "favicon")
             (rel "shortcut icon")
             (type "image/x-icon")
             (href "http://dblp.uni-trier.de/img/favicon.ico")))
      (link (@
             (rel "stylesheet")
             (type "text/css")
             (href "http://dblp.uni-trier.de/css/dblp-2014-10-23.css")))
      (*COMMENT*
       "[if lt IE 9]><link rel=\"stylesheet\" href=\"http://dblp.uni-trier.de/css/dblp-ie8-fix.css\" /><script src=\"http://dblp.uni-trier.de/js/html5shiv-printshiv-3.6.2.min.js\" type=\"application/javascript\"></script><![endif]")
      (link (@
             (href "http://dblp.uni-trier.de/css/open-sans.css")
             (rel "stylesheet")
             (type "text/css")))
      (meta
       (@
        (name "description")
        (content
         "The dblp computer science bibliography is the on-line reference for open bibliographic information on computer science journals and proceedings.")))
      (meta
       (@
        (name "keywords")
        (content
         "dblp, compter science, bibliograhy, publication, conferences, journals, books, theses, open data, bibtex, CompleteSearch")))
      (script
       (@ (type "application/ld+json"))
       "{\"@context\": \"http://schema.org\",\"@type\": \"WebSite\",\"url\": \"http://dblp.uni-trier.de\",\"potentialAction\": {\"@type\": \"SearchAction\",\"target\": \"http://dblp.uni-trier.de/search?q={search_term_string}\",\"query-input\": \"required name=search_term_string\"}}"))
     "\n"
     (body
      (@ (class "no-js completesearch-page"))
      "\n"
      (script
       (@ (type "application/javascript"))
       "var el = document.getElementsByTagName(\"body\")[0];el.className = \"js completesearch-page\";")
      "\n"
      (script
       (@
        (src "http://dblp.uni-trier.de/js/jquery-1.11.0.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/jquery.dblp-2014-08-07.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/jquery.cookie-1.2.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/jquery.infinitescroll-2.0.2.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/encoder-2012-09-23.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/buckets-1.0.min.js")
        (type "application/javascript")))
      (*COMMENT* " banner ")
      "\n"
      (div
       (@ (id "banner"))
       "\n"
       (*COMMENT*
        "[if lt IE 9]><div class=\"message fancy\" data-version=\"2014-01-01\">Sorry, but you need <a href=\"http://windows.microsoft.com/en-us/internet-explorer/download-ie\">Internet Explorer 9 or newer</a> to view our pages without any error. Please upgrade your web browser.</div><![endif]")
       "\n"
       (div
        (@ (class "message fancy permanent") (data-version "2014-06-16"))
        "This is a "
        (strong "public beta version")
        " of the new dblp web pages. You can find the "
        (a (@ (href "http://dblp.org/db/")) "old (stable) dblp website here.")
        " If you experience any trouble while using the new pages or if you do have any comments "
        (a (@ (href "mailto:dblp-website@dagstuhl.de")) "please let us know!"))
       "\n")
      "\n"
      (div
       (@ (id "main"))
       "\n"
       (button
        (@
         (style
          "overflow: visible !important; height: 0 !important; width: 0 !important; margin: 0 !important; border: 0 !important; padding: 0 !important; display: block !important;")
         (form "completesearch-form")
         (type "submit")))
       (*COMMENT* " logo ")
       (div
        (@ (id "logo"))
        (a
         (@ (href "http://dblp.uni-trier.de/db/"))
         (img
          (@
           (src "http://dblp.uni-trier.de/img/logo.png")
           (alt "dblp computer science bibliography")))))
       "\n"
       (*COMMENT* " top menu ")
       (nav
        (@ (class "top"))
        (ul
         (li
          (@ (class "drop-down"))
          (div
           (@ (class "head"))
           (a (@ (href "http://dblp.uni-trier.de/db/")) "home"))
          (div
           (@ (class "body"))
           (ul
            (li (a (@ (href "http://dblp.uni-trier.de/news/")) "news"))
            (li
             (a
              (@ (href "http://dblp.uni-trier.de/statistics/"))
              "statistics")))))
         (li
          (@ (class "drop-down"))
          (div
           (@ (class "head"))
           (a (@ (href "http://dblp.uni-trier.de/db/")) "browse"))
          (div
           (@ (class "body"))
           (ul
            (li (a (@ (href "http://dblp.uni-trier.de/pers/")) "persons"))
            (li (a (@ (href "http://dblp.uni-trier.de/db/conf/")) "conferences"))
            (li
             (a (@ (href "http://dblp.uni-trier.de/db/journals/")) "journals"))
            (li (a (@ (href "http://dblp.uni-trier.de/db/series/")) "series")))))
         (li
          (@ (class "drop-down"))
          (div
           (@ (class "head"))
           (a (@ (href "http://dblp.uni-trier.de/search/")) "search"))
          (div
           (@ (class "body"))
           (ul
            (li (a (@ (href "http://dblp.uni-trier.de/search/")) "search dblp"))
            (li
             (a
              (@ (href "http://dblp.uni-trier.de/search/publ/"))
              "CompleteSearch"))
            (li (@ (class "separator")))
            (li (a (@ (href "http://dblp.uni-trier.de/doi/")) "doi look-up"))
            (li (a (@ (href "http://dblp.uni-trier.de/isbn/")) "isbn look-up"))
            (li (@ (class "separator"))))))
         (li
          (@ (class "drop-down"))
          (div
           (@ (class "head"))
           (a (@ (href "http://dblp.uni-trier.de/db/about/")) "about"))
          (div
           (@ (class "body"))
           (ul
            (li (a (@ (href "http://dblp.uni-trier.de/faq/")) "f.a.q."))
            (li
             (a (@ (href "http://dblp.uni-trier.de/db/about/team.html")) "team"))
            (li
             (a
              (@ (href "http://dblp.uni-trier.de/db/copyright.html"))
              "legal bits")))))))
       "\n"
       (*COMMENT* " search form ")
       (div
        (@ (id "search"))
        (img
         (@
          (src "http://dblp.uni-trier.de/img/search.dark.16x16.png")
          (class "icon no-js-only")
          (title "search dblp")
          (alt "search dblp")))
        (div
         (@
          (class "drop-down js-only")
          (style "vertical-align:-6px; margin-right:1px;"))
         (div
          (@ (class "head"))
          (img
           (@
            (src "http://dblp.uni-trier.de/img/search.dark.16x16.png")
            (alt "search dblp"))))
         (div
          (@ (class "body"))
          (p (b "default search action"))
          (ul
           (li
            (input
             (@
              (id "search-mode-combined")
              (type "radio")
              (name "search-mode")
              (value "c")
              (checked)))
            (label (@ (for "search-mode-combined")) "combined dblp search"))
           (li
            (input
             (@
              (id "search-mode-author")
              (type "radio")
              (name "search-mode")
              (value "a")))
            (label (@ (for "search-mode-author")) "author search"))
           (li
            (input
             (@
              (id "search-mode-venue")
              (type "radio")
              (name "search-mode")
              (value "v")))
            (label (@ (for "search-mode-venue")) "venue search"))
           (li
            (input
             (@
              (id "search-mode-publ")
              (type "radio")
              (name "search-mode")
              (value "p")))
            (label (@ (for "search-mode-publ")) "publication search")))))
        (form
         (@ (method "get") (action "http://dblp.uni-trier.de/search"))
         (input (@ (type "text") (name "q") (maxlength "127")))
         (div
          (@ (class "results js-only"))
          (div
           (@ (class "authors"))
           (b "Authors:")
           (ul (@ (class "matches")) (li (i "no matches")))
           (ul
            (@ (class "waiting"))
            (li
             (img
              (@
               (src "http://dblp.uni-trier.de/img/waiting.anim.gif")
               (alt "waiting..."))))))
          (div
           (@ (class "venues"))
           (b "Venues:")
           (ul (@ (class "matches")) (li (i "no matches")))
           (ul
            (@ (class "waiting"))
            (li
             (img
              (@
               (src "http://dblp.uni-trier.de/img/waiting.anim.gif")
               (alt "waiting..."))))))
          (div
           (@ (class "publs"))
           (b "Publications:")
           (p
            (a
             (@
              (id "complete-search-link")
              (href "http://dblp.uni-trier.de/search/publ"))
             "search using CompleteSearch"))))
         (img
          (@
           (class "clear js-only")
           (src "http://dblp.uni-trier.de/img/clear-mark.medium.16x16.png")
           (alt "clear")
           (title "clear")))))
       "\n"
       (*COMMENT* " head line ")
       (header
        (@ (id "headline") (class "noline"))
        (h1 "CompleteSearch")
        (div
         (@ (class "note-line"))
         "extended search capabilities for dblp, courtesy of "
         (a
          (@ (href "http://dblp.uni-trier.de/pers/hd/b/Bast:Hannah"))
          "Hannah Bast")))
       "\n"
       (*COMMENT* " inpage navigation menu ")
       (nav
        (@ (class "side"))
        (ul
         (li
          (a
           (@ (href "#") (title "jump to top"))
           (img
            (@
             (alt "top")
             (src "http://dblp.uni-trier.de/img/top.dark.16x16.png")
             (class "icon")))))
         (li
          (a
           (@ (href "#footer") (title "jump to bottom"))
           (img
            (@
             (alt "bottom")
             (src "http://dblp.uni-trier.de/img/bottom.dark.16x16.png")
             (class "icon")))))))
       "\n"
       (*COMMENT* " mirror selector ")
       (nav
        (@ (id "mirror-selector") (class "selector-box"))
        (img
         (@
          (src "http://dblp.uni-trier.de/img/datastock.dark.16x16.png")
          (alt "mirror")
          (title "use mirror server")))
        (div
         (@ (class "drop-down selector"))
         (div (@ (class "head")) "Trier 2")
         (div
          (@ (class "body"))
          (ul
           (@ (class "options"))
           (li
            (a
             (@
              (href
               "http://www.informatik.uni-trier.de/~ley/search/publ?q=author%3ACali+year%3A2014"))
             "Trier 1"))
           (li
            (a
             (@
              (href
               "http://dblp.dagstuhl.de/search/publ?q=author%3ACali+year%3A2014"))
             "Dagstuhl"))))))
       "\n"
       (*COMMENT* " completesearch hits selector ")
       (nav
        (@ (id "completesearch-hits-selector") (class "selector-box no-js-only"))
        (img
         (@
          (src "http://dblp.uni-trier.de/img/eye.dark.16x16.png")
          (alt "order")
          (title "display a maximum of")))
        (div
         (@ (class "drop-down selector"))
         (div (@ (class "head")) (& le) " 30 hits")
         (div
          (@ (class "body"))
          (ul
           (@ (class "options"))
           (li
            (button
             (@
              (value "100")
              (name "newhits")
              (class "text")
              (form "completesearch-form"))
             (& le)
             " 100 hits"))
           (li
            (button
             (@
              (value "300")
              (name "newhits")
              (class "text")
              (form "completesearch-form"))
             (& le)
             " 300 hits"))
           (li
            (button
             (@
              (value "1000")
              (name "newhits")
              (class "text")
              (form "completesearch-form"))
             (& le)
             " 1000 hits"))))))
       "\n"
       (*COMMENT* " breadcrumbs ")
       (div
        (@ (id "breadcrumbs") (class "section"))
        (ul (li "> " (a (@ (href "http://dblp.uni-trier.de/db/")) "Home") " ")))
       "\n"
       (div (@ (class "clear-both section")))
       "\n"
       (*COMMENT* " CompleteSearch ")
       (div
        (@ (id "completesearch-facets") (class "panel hideable hidden"))
        (header (@ (class "hide-head")) (h2 "Facets"))
        (div
         (@ (class "hide-body"))
         (div
          (@ (class "refine-by author"))
          (p (b "refine by author"))
          (ul
           (@ (class "options"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Calin_Belta:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Calin Belta"
             (& nbsp)
             "(14)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Andrea_Cal&#236;:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Andrea Cal"
             #\ì
             (& nbsp)
             "(6)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Radu_Calinescu:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Radu Calinescu"
             (& nbsp)
             "(6)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Massimo_Poncino:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Massimo Poncino"
             (& nbsp)
             "(5)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Enrico_Macii:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Enrico Macii"
             (& nbsp)
             "(5)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Sylvain_Calinon:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Sylvain Calinon"
             (& nbsp)
             "(5)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Andrea_Calimera:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Andrea Calimera"
             (& nbsp)
             "(5)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Ebru_Aydin_Gol:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Ebru Aydin Gol"
             (& nbsp)
             "(5)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Marius-Calin_Silaghi:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Marius-Calin Silaghi"
             (& nbsp)
             "(5)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "author:Darwin_G._Caldwell:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Darwin G. Caldwell"
             (& nbsp)
             "(5)")))
          (ul (@ (class "more-options")) (li (em "skipping 303 more options")))
          (ul
           (@ (class "no-options") (style "display: none;"))
           (li (em "no options")))
          (ul
           (@ (class "waiting") (style "display: none;"))
           (li (img (@ (src "http://dblp.uni-trier.de/img/waiting.anim.gif")))))
          (ul
           (@ (class "error") (style "display: none;"))
           (li (em "temporarily not available"))))
         (div
          (@ (class "refine-by venue"))
          (p (b "refine by venue"))
          (ul
           (@ (class "options"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:CoRR:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "CoRR"
             (& nbsp)
             "(8)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:IEEE_Trans._Automat._Contr.:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "IEEE Trans. Automat. Contr."
             (& nbsp)
             "(6)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:ICRA:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "ICRA"
             (& nbsp)
             "(4)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:HCI:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "HCI"
             (& nbsp)
             "(3)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:I._J._Robotic_Res.:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "I. J. Robotic Res."
             (& nbsp)
             "(3)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:MIE:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "MIE"
             (& nbsp)
             "(2)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:OTM_Conferences:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "OTM Conferences"
             (& nbsp)
             "(2)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:SEBD:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "SEBD"
             (& nbsp)
             "(2)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:VEE:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "VEE"
             (& nbsp)
             "(2)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "venue:Automatica:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Automatica"
             (& nbsp)
             "(2)")))
          (ul (@ (class "more-options")) (li (em "skipping 69 more options")))
          (ul
           (@ (class "no-options") (style "display: none;"))
           (li (em "no options")))
          (ul
           (@ (class "waiting") (style "display: none;"))
           (li (img (@ (src "http://dblp.uni-trier.de/img/waiting.anim.gif")))))
          (ul
           (@ (class "error") (style "display: none;"))
           (li (em "temporarily not available"))))
         (div
          (@ (class "refine-by type"))
          (p (b "refine by type"))
          (ul
           (@ (class "options"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "type:Conference_and_Workshop_Papers:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Conference and Workshop Papers"
             (& nbsp)
             "(53)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "type:Journal_Articles:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Journal Articles"
             (& nbsp)
             "(38)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "type:Informal_Publications:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Informal Publications"
             (& nbsp)
             "(8)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "type:Editorship:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Editorship"
             (& nbsp)
             "(3)"))
           (li
            (@ (class "add"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/add-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "add constraint")
              (value "type:Parts_in_Books_or_Collections:")
              (name "add")
              (class "text")
              (form "completesearch-form"))
             "Parts in Books or Collections"
             (& nbsp)
             "(1)")))
          (ul (@ (class "more-options") (style "display: none;")))
          (ul
           (@ (class "no-options") (style "display: none;"))
           (li (em "no options")))
          (ul
           (@ (class "waiting") (style "display: none;"))
           (li (img (@ (src "http://dblp.uni-trier.de/img/waiting.anim.gif")))))
          (ul
           (@ (class "error") (style "display: none;"))
           (li (em "temporarily not available"))))
         (div
          (@ (class "refine-by year"))
          (p (b "refine by year"))
          (ul
           (@ (class "options"))
           (li
            (@ (class "del"))
            (img
             (@
              (src "http://dblp.uni-trier.de/img/del-mark.12x12.png")
              (alt "")
              (class "mark")))
            (button
             (@
              (title "remove constraint")
              (value "year:2014:")
              (name "del")
              (class "text selected")
              (form "completesearch-form"))
             "2014"
             (& nbsp)
             "(103) "
             #\✓)))
          (ul (@ (class "more-options") (style "display: none;")))
          (ul
           (@ (class "no-options") (style "display: none;"))
           (li (em "no options")))
          (ul
           (@ (class "waiting") (style "display: none;"))
           (li (img (@ (src "http://dblp.uni-trier.de/img/waiting.anim.gif")))))
          (ul
           (@ (class "error") (style "display: none;"))
           (li (em "temporarily not available"))))))
       (div
        (@ (id "completesearch-input") (class "section"))
        (header (@ (class "hide-head")) (h2 "Query"))
        (div
         (@ (class "hide-body"))
         (p
          (@ (id "completesearch-info-total"))
          "searching in 2,841,294 publications")
         (form
          (@
           (id "completesearch-form")
           (action "http://dblp.uni-trier.de/search/publ"))
          (div
           (@ (id "completesearch-query"))
           (p "search for:")
           (span
            (input
             (@
              (type "text")
              (name "q")
              (value "author:Cali year:2014")
              (maxlength "127")))
            (img
             (@
              (class "clear js-only")
              (src "http://dblp.uni-trier.de/img/clear-mark.medium.16x16.png")
              (alt "")
              (title "clear"))))
           (div
            (@ (class "drop-down faq"))
            (div
             (@ (class "head"))
             (a
              (@ (href "http://dblp.uni-trier.de/faq"))
              (img
               (@
                (src "http://dblp.uni-trier.de/img/faq-mark.dark.12x12.png")
                (alt "")))))
            (div
             (@ (class "body"))
             (b "Quick help")
             (ul
              (li
               (em "prefix search:")
               " default "
               (br)
               (small
                "e.g., "
                (span (@ (class "eg")) (a (@ (href "?q=sig")) "sig"))
                " matches \"SIGIR\" as well as \"signal\""))
              (li
               (em "exact word search:")
               " append dollar ($)"
               (br)
               (small
                "e.g., "
                (span (@ (class "eg")) (a (@ (href "?q=graph$")) "graph$"))
                " does not match \"graphics\""))
              (li
               (em "phrase search:")
               " connect by dot (.)"
               (br)
               (small
                "e.g., "
                (span
                 (@ (class "eg"))
                 (a (@ (href "?q=inform.retriev.tech")) "inform.retriev.tech"))))
              (li
               (em "boolean and:")
               " separate by space"
               (br)
               (small
                "e.g., "
                (span
                 (@ (class "eg"))
                 (a (@ (href "?q=codd%20model")) "codd model"))))
              (li
               (em "boolean or:")
               " connect by pipe (|)"
               (br)
               (small
                "e.g., "
                (span
                 (@ (class "eg"))
                 (a (@ (href "?q=graph%7Cnetwork")) "graph|network"))))
              (li
               (em "boolean not:")
               " prepend minus (-)"
               (br)
               (small
                "e.g., "
                (span
                 (@ (class "eg"))
                 (a (@ (href "?q=knuth%20-don")) "knuth -don"))))))))
          (input (@ (type "hidden") (name "hits") (value "30"))))))
       (div
        (@ (id "completesearch-results") (class "section"))
        (header
         (@ (class "head"))
         (h2 "Matching publications")
         (nav
          (@ (class "header"))
          (ul
           (li
            (@ (class "drop-down"))
            (div
             (@ (class "head"))
             (a
              (@
               (href
                "http://dblp.uni-trier.de/search/publ/api?q=author%3ACali+year%3A2014&h=1000&c=0&rd=1a&format=xml"))
              (img
               (@
                (src "http://dblp.uni-trier.de/img/download.dark.16x16.png")
                (alt "")
                (class "icon")))))
            (div
             (@ (class "body"))
             (p (b "export search results as"))
             (ul
              (li
               (@ (class "xml"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/search/publ/api?q=author%3ACali+year%3A2014&h=1000&c=0&rd=1a&format=xml"))
                (img
                 (@
                  (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                  (alt "")
                  (class "icon")))
                "XML"))
              (li
               (@ (class "json"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/search/publ/api?q=author%3ACali+year%3A2014&h=1000&c=0&rd=1a&format=json"))
                (img
                 (@
                  (src "http://dblp.uni-trier.de/img/json.dark.16x16.png")
                  (alt "")
                  (class "icon")))
                "JSON"))
              (li
               (@ (class "jsonp"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/search/publ/api?q=author%3ACali+year%3A2014&h=1000&c=0&rd=1a&format=jsonp"))
                (img
                 (@
                  (src "http://dblp.uni-trier.de/img/json.dark.16x16.png")
                  (alt "")
                  (class "icon")))
                "JSONP")))
             (p (em "exporting car 1000 hits only")))))))
        (div
         (@ (class "body"))
         (p (@ (id "completesearch-info-matches")) "found 103 matches")
         (ul
          (@ (class "publ-list"))
          (li (@ (class "year")) "2014")
          (li
           (@ (class "entry article") (id "journals/adb/CaligioreTSB14"))
           (a (@ (name "CaligioreTSB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1177/1059712314539710"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1177/1059712314539710"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/adb/CaligioreTSB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/adb/CaligioreTSB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/adb/CaligioreTSB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/adb/CaligioreTSB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/adb/CaligioreTSB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Modular+and+hierarchical+brain+organization+to+understand+assimilation%2C+accommodation+and+their+relation+to+autism+in+reaching+tasks%3A+a+developmental+robotics+hypothesis."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Modular+and+hierarchical+brain+organization+to+understand+assimilation%2C+accommodation+and+their+relation+to+autism+in+reaching+tasks%3A+a+developmental+robotics+hypothesis."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Modular+and+hierarchical+brain+organization+to+understand+assimilation%2C+accommodation+and+their+relation+to+autism+in+reaching+tasks%3A+a+developmental+robotics+hypothesis."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Modular+and+hierarchical+brain+organization+to+understand+assimilation%2C+accommodation+and+their+relation+to+autism+in+reaching+tasks%3A+a+developmental+robotics+hypothesis."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/adb/CaligioreTSB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Caligiore:Daniele"))
             "Daniele Caligiore")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/t/Tommasino:Paolo"))
             "Paolo Tommasino")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Sperati:Valerio"))
             "Valerio Sperati")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Baldassarre:Gianluca"))
             "Gianluca Baldassarre")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Modular and hierarchical brain organization to understand assimilation, accommodation and their relation to autism in reaching tasks: a developmental robotics hypothesis.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/adb/adb22.html#CaligioreTSB14"))
             "Adaptive Behaviour 22(5)")
            ": 304-329 (2014)"))
          (li
           (@ (class "entry article") (id "journals/automatica/DingLB14"))
           (a (@ (name "DingLB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.automatica.2013.11.030"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.automatica.2013.11.030"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/automatica/DingLB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/automatica/DingLB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/automatica/DingLB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/automatica/DingLB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/automatica/DingLB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=LTL+receding+horizon+control+for+finite+deterministic+systems."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=LTL+receding+horizon+control+for+finite+deterministic+systems."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=LTL+receding+horizon+control+for+finite+deterministic+systems."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=LTL+receding+horizon+control+for+finite+deterministic+systems."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/automatica/DingLB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/d/Ding:Xu_Chu"))
             "Xu Chu Ding")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/l/Lazar:Mircea"))
             "Mircea Lazar")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Belta:Calin"))
             "Calin Belta")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "LTL receding horizon control for finite deterministic systems.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/automatica/automatica50.html#DingLB14"))
             "Automatica 50(2)")
            ": 399-408 (2014)"))
          (li
           (@ (class "entry article") (id "journals/automatica/CaliskanT14"))
           (a (@ (name "CaliskanT14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.automatica.2014.08.017"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.automatica.2014.08.017"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/automatica/CaliskanT14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/automatica/CaliskanT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/automatica/CaliskanT14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/automatica/CaliskanT14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/automatica/CaliskanT14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Towards+Kron+reduction+of+generalized+electrical+networks."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Towards+Kron+reduction+of+generalized+electrical+networks."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Towards+Kron+reduction+of+generalized+electrical+networks."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Towards+Kron+reduction+of+generalized+electrical+networks."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href
                    "http://pubzone.org/dblp/journals/automatica/CaliskanT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Caliskan:Sina_Yamac"))
             "Sina Yamac Caliskan")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/t/Tabuada:Paulo"))
             "Paulo Tabuada")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Towards Kron reduction of generalized electrical networks.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/automatica/automatica50.html#CaliskanT14"))
             "Automatica 50(10)")
            ": 2586-2590 (2014)"))
          (li
           (@ (class "entry article") (id "journals/bc/CohenBT14"))
           (a (@ (name "CohenBT14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1007/s00422-014-0602-x"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1007/s00422-014-0602-x"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/bc/CohenBT14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/bc/CohenBT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/bc/CohenBT14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/bc/CohenBT14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/bc/CohenBT14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Dependence+of+V2+illusory+contour+response+on+V1+cell+properties+and+topographic+organization."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Dependence+of+V2+illusory+contour+response+on+V1+cell+properties+and+topographic+organization."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Dependence+of+V2+illusory+contour+response+on+V1+cell+properties+and+topographic+organization."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Dependence+of+V2+illusory+contour+response+on+V1+cell+properties+and+topographic+organization."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/bc/CohenBT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Cohen:Amelia"))
             "Amelia Cohen")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Buia:Calin_I="))
             "Calin I. Buia")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/t/Tiesinga:Paul_H=_E="))
             "Paul H. E. Tiesinga")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Dependence of V2 illusory contour response on V1 cell properties and topographic organization.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/bc/bc108.html#CohenBT14"))
             "Biological Cybernetics 108(3)")
            ": 337-354 (2014)"))
          (li
           (@ (class "entry article") (id "journals/cii/RicoCCG14"))
           (a (@ (name "RicoCCG14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.compind.2014.07.010"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.compind.2014.07.010"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/cii/RicoCCG14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/cii/RicoCCG14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/cii/RicoCCG14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/cii/RicoCCG14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/cii/RicoCCG14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=OntoQualitas%3A+A+framework+for+ontology+quality+assessment+in+information+interchanges+between+heterogeneous+systems."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=OntoQualitas%3A+A+framework+for+ontology+quality+assessment+in+information+interchanges+between+heterogeneous+systems."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=OntoQualitas%3A+A+framework+for+ontology+quality+assessment+in+information+interchanges+between+heterogeneous+systems."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=OntoQualitas%3A+A+framework+for+ontology+quality+assessment+in+information+interchanges+between+heterogeneous+systems."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/cii/RicoCCG14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Rico:Mariela"))
             "Mariela Rico")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/c/Caliusco:Mar=iacute=a_Laura"))
             "Mar"
             #\í
             "a Laura Caliusco")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Chiotti:Omar"))
             "Omar Chiotti")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/g/Galli:Mar=iacute=a_Rosa"))
             "Mar"
             #\í
             "a Rosa Galli")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "OntoQualitas: A framework for ontology quality assessment in information interchanges between heterogeneous systems.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/cii/cii65.html#RicoCCG14"))
             "Computers in Industry 65(9)")
            ": 1291-1300 (2014)"))
          (li
           (@ (class "entry article") (id "journals/cj/ZhuZZ14"))
           (a (@ (name "ZhuZZ14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1093/comjnl/bxt021"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1093/comjnl/bxt021"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/cj/ZhuZZ14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/cj/ZhuZZ14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/cj/ZhuZZ14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/cj/ZhuZZ14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/cj/ZhuZZ14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Optimization+of+Monotonic+Linear+Progressive+Queries+Based+on+Dynamic+Materialized+Views."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Optimization+of+Monotonic+Linear+Progressive+Queries+Based+on+Dynamic+Materialized+Views."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Optimization+of+Monotonic+Linear+Progressive+Queries+Based+on+Dynamic+Materialized+Views."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Optimization+of+Monotonic+Linear+Progressive+Queries+Based+on+Dynamic+Materialized+Views."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/cj/ZhuZZ14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/z/Zhu:Chao"))
             "Chao Zhu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/z/Zhu:Qiang"))
             "Qiang Zhu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/z/Zuzarte:Calisto"))
             "Calisto Zuzarte")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Optimization of Monotonic Linear Progressive Queries Based on Dynamic Materialized Views.")
            " "
            (a
             (@
              (href "http://dblp.uni-trier.de/db/journals/cj/cj57.html#ZhuZZ14"))
             "Comput. J. 57(5)")
            ": 708-730 (2014)"))
          (li
           (@ (class "entry article") (id "journals/cmpb/CalinonBMNC14"))
           (a (@ (name "CalinonBMNC14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.cmpb.2013.12.015"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.cmpb.2013.12.015"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/cmpb/CalinonBMNC14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/cmpb/CalinonBMNC14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/cmpb/CalinonBMNC14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/cmpb/CalinonBMNC14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/cmpb/CalinonBMNC14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Human-robot+skills+transfer+interfaces+for+a+flexible+surgical+robot."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Human-robot+skills+transfer+interfaces+for+a+flexible+surgical+robot."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Human-robot+skills+transfer+interfaces+for+a+flexible+surgical+robot."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Human-robot+skills+transfer+interfaces+for+a+flexible+surgical+robot."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/cmpb/CalinonBMNC14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Calinon:Sylvain"))
             "Sylvain Calinon")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Bruno:Danilo"))
             "Danilo Bruno")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Malekzadeh:Milad_S="))
             "Milad S. Malekzadeh")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/n/Nanayakkara:Thrishantha"))
             "Thrishantha Nanayakkara")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Caldwell:Darwin_G="))
             "Darwin G. Caldwell")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Human-robot skills transfer interfaces for a flexible surgical robot.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/cmpb/cmpb116.html#CalinonBMNC14"))
             "Computer Methods and Programs in Biomedicine 116(2)")
            ": 81-96 (2014)"))
          (li
           (@ (class "entry article") (id "journals/comgeo/CalinescuK14"))
           (a (@ (name "CalinescuK14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.comgeo.2013.08.009"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.comgeo.2013.08.009"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/comgeo/CalinescuK14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/comgeo/CalinescuK14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/comgeo/CalinescuK14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/comgeo/CalinescuK14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/comgeo/CalinescuK14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Sequential+dependency+computation+via+geometric+data+structures."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Sequential+dependency+computation+via+geometric+data+structures."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Sequential+dependency+computation+via+geometric+data+structures."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Sequential+dependency+computation+via+geometric+data+structures."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/comgeo/CalinescuK14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Calinescu:Gruia"))
             "Gruia Calinescu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/k/Karloff:Howard_J="))
             "Howard J. Karloff")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Sequential dependency computation via geometric data structures.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/comgeo/comgeo47.html#CalinescuK14"))
             "Comput. Geom. 47(2)")
            ": 141-148 (2014)"))
          (li
           (@
            (class "entry article")
            (id "journals/cphysics/GrossuFJBSRRCEBT14"))
           (a (@ (name "GrossuFJBSRRCEBT14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.cpc.2014.06.014"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.cpc.2014.06.014"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/cphysics/GrossuFJBSRRCEBT14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/cphysics/GrossuFJBSRRCEBT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/cphysics/GrossuFJBSRRCEBT14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/cphysics/GrossuFJBSRRCEBT14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/cphysics/GrossuFJBSRRCEBT14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=CMBE+v05+-+Implementation+of+a+toy-model+for+chaos+analysis+of+relativistic+nuclear+collisions+at+the+present+BNL+energies."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=CMBE+v05+-+Implementation+of+a+toy-model+for+chaos+analysis+of+relativistic+nuclear+collisions+at+the+present+BNL+energies."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=CMBE+v05+-+Implementation+of+a+toy-model+for+chaos+analysis+of+relativistic+nuclear+collisions+at+the+present+BNL+energies."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=CMBE+v05+-+Implementation+of+a+toy-model+for+chaos+analysis+of+relativistic+nuclear+collisions+at+the+present+BNL+energies."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href
                    "http://pubzone.org/dblp/journals/cphysics/GrossuFJBSRRCEBT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/g/Grossu:I=_V="))
             "I. V. Grossu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/f/Felea:D="))
             "D. Felea")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/j/Jipa:Al="))
             "Al. Jipa")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Besliu:C="))
             "C. Besliu")
            ", "
            (a (@ (href "http://dblp.uni-trier.de/pers/hd/s/Stan:E=")) "E. Stan")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Ristea:O="))
             "O. Ristea")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Ristea:C="))
             "C. Ristea")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Calin:M="))
             "M. Calin")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/e/Esanu:T="))
             "T. Esanu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Bordeianu:C=_C="))
             "C. C. Bordeianu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/t/Tuturas:N="))
             "N. Tuturas")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "CMBE v05 - Implementation of a toy-model for chaos analysis of relativistic nuclear collisions at the present BNL energies.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/cphysics/cphysics185.html#GrossuFJBSRRCEBT14"))
             "Computer Physics Communications 185(11)")
            ": 3059-3061 (2014)"))
          (li
           (@ (class "entry article") (id "journals/disopt/Calinescu14"))
           (a (@ (name "Calinescu14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.disopt.2014.06.002"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.disopt.2014.06.002"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/disopt/Calinescu14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/disopt/Calinescu14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/disopt/Calinescu14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/disopt/Calinescu14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/disopt/Calinescu14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Relay+placement+for+two-connectivity."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Relay+placement+for+two-connectivity."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Relay+placement+for+two-connectivity."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Relay+placement+for+two-connectivity."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/disopt/Calinescu14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Calinescu:Gruia"))
             "Gruia Calinescu")
            ":"
            (br)
            " "
            (span (@ (class "title")) "Relay placement for two-connectivity.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/disopt/disopt14.html#Calinescu14"))
             "Discrete Optimization 14")
            ": 17-33 (2014)"))
          (li
           (@ (class "entry article") (id "journals/eor/XueCN14"))
           (a (@ (name "XueCN14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.ejor.2014.04.044"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.ejor.2014.04.044"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/eor/XueCN14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/eor/XueCN14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/eor/XueCN14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/eor/XueCN14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/eor/XueCN14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Supply+chain+performance+and+consumer+surplus+under+alternative+structures+of+channel+dominance."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Supply+chain+performance+and+consumer+surplus+under+alternative+structures+of+channel+dominance."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Supply+chain+performance+and+consumer+surplus+under+alternative+structures+of+channel+dominance."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Supply+chain+performance+and+consumer+surplus+under+alternative+structures+of+channel+dominance."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/eor/XueCN14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/x/Xue:Weili"))
             "Weili Xue")
            ", "
            (a
             (@
              (href "http://dblp.uni-trier.de/pers/hd/c/Caliskan=Demirag:Ozgun"))
             "Ozgun Caliskan-Demirag")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/n/Niu:Baozhuang"))
             "Baozhuang Niu")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Supply chain performance and consumer surplus under alternative structures of channel dominance.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/eor/eor239.html#XueCN14"))
             "European Journal of Operational Research 239(1)")
            ": 130-145 (2014)"))
          (li
           (@ (class "entry article") (id "journals/eswa/ReynaresCG14"))
           (a (@ (name "ReynaresCG14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.eswa.2013.08.054"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.eswa.2013.08.054"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/eswa/ReynaresCG14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/eswa/ReynaresCG14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/eswa/ReynaresCG14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/eswa/ReynaresCG14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/eswa/ReynaresCG14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Approaching+the+feasibility+of+SBVR+as+modeling+language+for+ontology+development%3A+An+exploratory+experiment."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Approaching+the+feasibility+of+SBVR+as+modeling+language+for+ontology+development%3A+An+exploratory+experiment."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Approaching+the+feasibility+of+SBVR+as+modeling+language+for+ontology+development%3A+An+exploratory+experiment."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Approaching+the+feasibility+of+SBVR+as+modeling+language+for+ontology+development%3A+An+exploratory+experiment."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/eswa/ReynaresCG14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Reynares:Emiliano"))
             "Emiliano Reynares")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/c/Caliusco:Mar=iacute=a_Laura"))
             "Mar"
             #\í
             "a Laura Caliusco")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/g/Galli:Mar=iacute=a_Rosa"))
             "Mar"
             #\í
             "a Rosa Galli")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Approaching the feasibility of SBVR as modeling language for ontology development: An exploratory experiment.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/eswa/eswa41.html#ReynaresCG14"))
             "Expert Syst. Appl. 41(4)")
            ": 1576-1583 (2014)"))
          (li
           (@ (class "entry article") (id "journals/et/MiryalaOPCMP14"))
           (a (@ (name "MiryalaOPCMP14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1007/s10836-014-5458-4"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1007/s10836-014-5458-4"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/et/MiryalaOPCMP14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/et/MiryalaOPCMP14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/et/MiryalaOPCMP14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/et/MiryalaOPCMP14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/et/MiryalaOPCMP14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Modeling+of+Physical+Defects+in+PN+Junction+Based+Graphene+Devices."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Modeling+of+Physical+Defects+in+PN+Junction+Based+Graphene+Devices."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Modeling+of+Physical+Defects+in+PN+Junction+Based+Graphene+Devices."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Modeling+of+Physical+Defects+in+PN+Junction+Based+Graphene+Devices."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/et/MiryalaOPCMP14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Miryala:Sandeep"))
             "Sandeep Miryala")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/o/Oleiro:Matheus"))
             "Matheus Oleiro")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/p/P=ouml=hls:Let=iacute=cia_Maria_Bolzani"))
             "Let"
             #\í
             "cia Maria Bolzani P"
             #\ö
             "hls")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Calimera:Andrea"))
             "Andrea Calimera")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Macii:Enrico"))
             "Enrico Macii")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/p/Poncino:Massimo"))
             "Massimo Poncino")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Modeling of Physical Defects in PN Junction Based Graphene Devices.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/et/et30.html#MiryalaOPCMP14"))
             "J. Electronic Testing 30(3)")
            ": 357-370 (2014)"))
          (li
           (@ (class "entry article") (id "journals/ijrr/CizeljB14"))
           (a (@ (name "CizeljB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1177/0278364914522312"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1177/0278364914522312"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/ijrr/CizeljB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/ijrr/CizeljB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/ijrr/CizeljB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/ijrr/CizeljB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/ijrr/CizeljB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Control+of+noisy+differential-drive+vehicles+from+time-bounded+temporal+logic+specifications."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Control+of+noisy+differential-drive+vehicles+from+time-bounded+temporal+logic+specifications."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Control+of+noisy+differential-drive+vehicles+from+time-bounded+temporal+logic+specifications."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Control+of+noisy+differential-drive+vehicles+from+time-bounded+temporal+logic+specifications."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/ijrr/CizeljB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Cizelj:Igor"))
             "Igor Cizelj")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Belta:Calin"))
             "Calin Belta")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Control of noisy differential-drive vehicles from time-bounded temporal logic specifications.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/ijrr/ijrr33.html#CizeljB14"))
             "I. J. Robotic Res. 33(8)")
            ": 1112-1129 (2014)"))
          (li
           (@ (class "entry article") (id "journals/ijrr/UlusoyWB14"))
           (a (@ (name "UlusoyWB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1177/0278364913519000"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1177/0278364913519000"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/ijrr/UlusoyWB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/ijrr/UlusoyWB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/ijrr/UlusoyWB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/ijrr/UlusoyWB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/ijrr/UlusoyWB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Incremental+controller+synthesis+in+probabilistic+environments+with+temporal+logic+constraints."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Incremental+controller+synthesis+in+probabilistic+environments+with+temporal+logic+constraints."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Incremental+controller+synthesis+in+probabilistic+environments+with+temporal+logic+constraints."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Incremental+controller+synthesis+in+probabilistic+environments+with+temporal+logic+constraints."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/ijrr/UlusoyWB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/u/Ulusoy:Alphan"))
             "Alphan Ulusoy")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/w/Wongpiromsarn:Tichakorn"))
             "Tichakorn Wongpiromsarn")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Belta:Calin"))
             "Calin Belta")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Incremental controller synthesis in probabilistic environments with temporal logic constraints.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/ijrr/ijrr33.html#UlusoyWB14"))
             "I. J. Robotic Res. 33(8)")
            ": 1130-1144 (2014)"))
          (li
           (@ (class "entry article") (id "journals/ijrr/UlusoyB14"))
           (a (@ (name "UlusoyB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1177/0278364914537008"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1177/0278364914537008"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/ijrr/UlusoyB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/ijrr/UlusoyB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/ijrr/UlusoyB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/ijrr/UlusoyB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/ijrr/UlusoyB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Receding+horizon+temporal+logic+control+in+dynamic+environments."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Receding+horizon+temporal+logic+control+in+dynamic+environments."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Receding+horizon+temporal+logic+control+in+dynamic+environments."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Receding+horizon+temporal+logic+control+in+dynamic+environments."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/ijrr/UlusoyB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/u/Ulusoy:Alphan"))
             "Alphan Ulusoy")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Belta:Calin"))
             "Calin Belta")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Receding horizon temporal logic control in dynamic environments.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/ijrr/ijrr33.html#UlusoyB14"))
             "I. J. Robotic Res. 33(12)")
            ": 1593-1607 (2014)"))
          (li
           (@ (class "entry article") (id "journals/isci/RanchhodGLT14"))
           (a (@ (name "RanchhodGLT14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.ins.2013.09.008"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.ins.2013.09.008"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/isci/RanchhodGLT14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/isci/RanchhodGLT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/isci/RanchhodGLT14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/isci/RanchhodGLT14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/isci/RanchhodGLT14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Evaluating+the+educational+effectiveness+of+simulation+games%3A+A+value+generation+model."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Evaluating+the+educational+effectiveness+of+simulation+games%3A+A+value+generation+model."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Evaluating+the+educational+effectiveness+of+simulation+games%3A+A+value+generation+model."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Evaluating+the+educational+effectiveness+of+simulation+games%3A+A+value+generation+model."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/isci/RanchhodGLT14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Ranchhod:Ashok"))
             "Ashok Ranchhod")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/g/Gurau:Calin"))
             "Calin Gurau")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/l/Loukis:Euripidis"))
             "Euripidis Loukis")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/t/Trivedi:Rohit"))
             "Rohit Trivedi")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Evaluating the educational effectiveness of simulation games: A value generation model.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/isci/isci264.html#RanchhodGLT14"))
             "Inf. Sci. 264")
            ": 75-90 (2014)"))
          (li
           (@ (class "entry article") (id "journals/jamia/FleurenceCCPSB14"))
           (a (@ (name "FleurenceCCPSB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1136/amiajnl-2014-002747"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1136/amiajnl-2014-002747"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/jamia/FleurenceCCPSB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/jamia/FleurenceCCPSB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/jamia/FleurenceCCPSB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/jamia/FleurenceCCPSB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/jamia/FleurenceCCPSB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Brief+communication%3A+Launching+PCORnet%2C+a+national+patient-centered+clinical+research+network."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Brief+communication%3A+Launching+PCORnet%2C+a+national+patient-centered+clinical+research+network."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Brief+communication%3A+Launching+PCORnet%2C+a+national+patient-centered+clinical+research+network."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Brief+communication%3A+Launching+PCORnet%2C+a+national+patient-centered+clinical+research+network."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href
                    "http://pubzone.org/dblp/journals/jamia/FleurenceCCPSB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/f/Fleurence:Rachael_L="))
             "Rachael L. Fleurence")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Curtis:Lesley_H="))
             "Lesley H. Curtis")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Califf:Robert_M="))
             "Robert M. Califf")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/p/Platt:Richard"))
             "Richard Platt")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Selby:Joe_V="))
             "Joe V. Selby")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Brown:Jeffrey_S="))
             "Jeffrey S. Brown")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Brief communication: Launching PCORnet, a national patient-centered clinical research network.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/jamia/jamia21.html#FleurenceCCPSB14"))
             "JAMIA 21(4)")
            ": 578-582 (2014)"))
          (li
           (@ (class "entry article") (id "journals/jcam/UyanCD14"))
           (a (@ (name "UyanCD14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.cam.2013.06.035"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.cam.2013.06.035"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/jcam/UyanCD14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/jcam/UyanCD14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/jcam/UyanCD14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/jcam/UyanCD14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/jcam/UyanCD14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Counting+Boolean+functions+with+specified+values+in+their+Walsh+spectrum."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Counting+Boolean+functions+with+specified+values+in+their+Walsh+spectrum."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Counting+Boolean+functions+with+specified+values+in+their+Walsh+spectrum."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Counting+Boolean+functions+with+specified+values+in+their+Walsh+spectrum."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/jcam/UyanCD14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/u/Uyan:Erdener"))
             "Erdener Uyan")
            ", "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/pers/hd/=/=Ccedil=alik:=Ccedil=agdas"))
             #\Ç
             "agdas "
             #\Ç
             "alik")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/d/Doganaksoy:Ali"))
             "Ali Doganaksoy")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Counting Boolean functions with specified values in their Walsh spectrum.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/jcam/jcam259.html#UyanCD14"))
             "J. Computational Applied Mathematics 259")
            ": 522-528 (2014)"))
          (li
           (@ (class "entry article") (id "journals/jcb/CalifanoKS14"))
           (a (@ (name "CalifanoKS14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1089/cmb.2014.009p"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1089/cmb.2014.009p"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/jcb/CalifanoKS14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/jcb/CalifanoKS14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/jcb/CalifanoKS14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/jcb/CalifanoKS14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/jcb/CalifanoKS14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=RECOMB%2FISCB+Systems+Biology%2C+Regulatory+Genomics%2C+and+DREAM+2013+Special+Issue."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=RECOMB%2FISCB+Systems+Biology%2C+Regulatory+Genomics%2C+and+DREAM+2013+Special+Issue."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=RECOMB%2FISCB+Systems+Biology%2C+Regulatory+Genomics%2C+and+DREAM+2013+Special+Issue."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=RECOMB%2FISCB+Systems+Biology%2C+Regulatory+Genomics%2C+and+DREAM+2013+Special+Issue."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/jcb/CalifanoKS14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Califano:Andrea"))
             "Andrea Califano")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/k/Kellis:Manolis"))
             "Manolis Kellis")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Stolovitzky:Gustavo"))
             "Gustavo Stolovitzky")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "RECOMB/ISCB Systems Biology, Regulatory Genomics, and DREAM 2013 Special Issue.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/jcb/jcb21.html#CalifanoKS14"))
             "Journal of Computational Biology 21(5)")
            ": 371-372 (2014)"))
          (li
           (@ (class "entry article") (id "journals/jmiv/BirteaCP14"))
           (a (@ (name "BirteaCP14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1007/s10851-013-0478-8"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1007/s10851-013-0478-8"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/jmiv/BirteaCP14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/jmiv/BirteaCP14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/jmiv/BirteaCP14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/jmiv/BirteaCP14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/jmiv/BirteaCP14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Averaging+on+Manifolds+by+Embedding+Algorithm."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Averaging+on+Manifolds+by+Embedding+Algorithm."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Averaging+on+Manifolds+by+Embedding+Algorithm."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Averaging+on+Manifolds+by+Embedding+Algorithm."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/jmiv/BirteaCP14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Birtea:Petre"))
             "Petre Birtea")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Comanescu:Dan"))
             "Dan Comanescu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/p/Popa:Calin=Adrian"))
             "Calin-Adrian Popa")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Averaging on Manifolds by Embedding Algorithm.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/jmiv/jmiv49.html#BirteaCP14"))
             "Journal of Mathematical Imaging and Vision 49(2)")
            ": 454-466 (2014)"))
          (li
           (@ (class "entry article") (id "journals/jns/MartinM14"))
           (a (@ (name "MartinM14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1007/s00332-014-9201-1"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1007/s00332-014-9201-1"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/jns/MartinM14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/jns/MartinM14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/jns/MartinM14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/jns/MartinM14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/jns/MartinM14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Steady+Periodic+Water+Waves+with+Unbounded+Vorticity%3A+Equivalent+Formulations+and+Existence+Results."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Steady+Periodic+Water+Waves+with+Unbounded+Vorticity%3A+Equivalent+Formulations+and+Existence+Results."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Steady+Periodic+Water+Waves+with+Unbounded+Vorticity%3A+Equivalent+Formulations+and+Existence+Results."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Steady+Periodic+Water+Waves+with+Unbounded+Vorticity%3A+Equivalent+Formulations+and+Existence+Results."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/jns/MartinM14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Martin:Calin_Iulian"))
             "Calin Iulian Martin")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Matioc:Bogdan=Vasile"))
             "Bogdan-Vasile Matioc")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Steady Periodic Water Waves with Unbounded Vorticity: Equivalent Formulations and Existence Results.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/jns/jns24.html#MartinM14"))
             "J. Nonlinear Science 24(4)")
            ": 633-659 (2014)"))
          (li
           (@ (class "entry article") (id "journals/jssc/YuZCDMGJYJIJ14"))
           (a (@ (name "YuZCDMGJYJIJ14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1109/JSSC.2014.2315650"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1109/JSSC.2014.2315650"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/jssc/YuZCDMGJYJIJ14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/jssc/YuZCDMGJYJIJ14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/jssc/YuZCDMGJYJIJ14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/jssc/YuZCDMGJYJIJ14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/jssc/YuZCDMGJYJIJ14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=An+X-Band+Radar+Transceiver+MMIC+with+Bandwidth+Reduction+in+0.13+%C2%B5m+SiGe+Technology."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=An+X-Band+Radar+Transceiver+MMIC+with+Bandwidth+Reduction+in+0.13+%C2%B5m+SiGe+Technology."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=An+X-Band+Radar+Transceiver+MMIC+with+Bandwidth+Reduction+in+0.13+%C2%B5m+SiGe+Technology."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=An+X-Band+Radar+Transceiver+MMIC+with+Bandwidth+Reduction+in+0.13+%C2%B5m+SiGe+Technology."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/jssc/YuZCDMGJYJIJ14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/y/Yu:Jianjun"))
             "Jianjun Yu")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/z/Zhao:Feng"))
             "Feng Zhao")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Cali:Joseph"))
             "Joseph Cali")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/d/Dai:Fa_Foster"))
             "Fa Foster Dai")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Ma:Desheng"))
             "Desheng Ma")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/g/Geng:Xueyang"))
             "Xueyang Geng")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/j/Jin:Yuehai"))
             "Yuehai Jin")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/y/Yao:Yuan"))
             "Yuan Yao")
            ", "
            (a (@ (href "http://dblp.uni-trier.de/pers/hd/j/Jin:Xin")) "Xin Jin")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/i/Irwin:J=_David"))
             "J. David Irwin")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/j/Jaeger:Richard_C="))
             "Richard C. Jaeger")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "An X-Band Radar Transceiver MMIC with Bandwidth Reduction in 0.13 "
             #\µ
             "m SiGe Technology.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/jssc/jssc49.html#YuZCDMGJYJIJ14"))
             "J. Solid-State Circuits 49(9)")
            ": 1905-1915 (2014)"))
          (li
           (@ (class "entry article") (id "journals/lgrs/PapaASPCCDFLFCCGS14"))
           (a (@ (name "PapaASPCCDFLFCCGS14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1109/LGRS.2013.2291237"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1109/LGRS.2013.2291237"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/lgrs/PapaASPCCDFLFCCGS14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/lgrs/PapaASPCCDFLFCCGS14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/lgrs/PapaASPCCDFLFCCGS14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/lgrs/PapaASPCCDFLFCCGS14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/lgrs/PapaASPCCDFLFCCGS14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Design+and+Validation+of+a+Multimode+Multifrequency+VHF%2FUHF+Airborne+Radar."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Design+and+Validation+of+a+Multimode+Multifrequency+VHF%2FUHF+Airborne+Radar."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Design+and+Validation+of+a+Multimode+Multifrequency+VHF%2FUHF+Airborne+Radar."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Design+and+Validation+of+a+Multimode+Multifrequency+VHF%2FUHF+Airborne+Radar."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href
                    "http://pubzone.org/dblp/journals/lgrs/PapaASPCCDFLFCCGS14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/p/Papa:Claudio"))
             "Claudio Papa")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/a/Alberti:Giovanni"))
             "Giovanni Alberti")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Salzillo:Giuseppe"))
             "Giuseppe Salzillo")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/p/Palmese:Gianfranco"))
             "Gianfranco Palmese")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Califano:Dario"))
             "Dario Califano")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Ciofaniello:L="))
             "L. Ciofaniello")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/d/Daniele:Maria"))
             "Maria Daniele")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/f/Facchinetti:Claudia"))
             "Claudia Facchinetti")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/l/Longo:Francesco"))
             "Francesco Longo")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/f/Formaro:Roberto"))
             "Roberto Formaro")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Catapano:Ilaria"))
             "Ilaria Catapano")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Crocco:Lorenzo"))
             "Lorenzo Crocco")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/g/Gennarelli:Gianluca"))
             "Gianluca Gennarelli")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Soldovieri:Francesco"))
             "Francesco Soldovieri")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Design and Validation of a Multimode Multifrequency VHF/UHF Airborne Radar.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/lgrs/lgrs11.html#PapaASPCCDFLFCCGS14"))
             "IEEE Geosci. Remote Sensing Lett. 11(7)")
            ": 1260-1264 (2014)"))
          (li
           (@ (class "entry article") (id "journals/mj/TenaceMCMMP14"))
           (a (@ (name "TenaceMCMMP14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.mejo.2013.11.013"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.mejo.2013.11.013"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/mj/TenaceMCMMP14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/mj/TenaceMCMMP14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/mj/TenaceMCMMP14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/mj/TenaceMCMMP14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/mj/TenaceMCMMP14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Row-based+body-bias+assignment+for+dynamic+thermal+clock-skew+compensation."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Row-based+body-bias+assignment+for+dynamic+thermal+clock-skew+compensation."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Row-based+body-bias+assignment+for+dynamic+thermal+clock-skew+compensation."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Row-based+body-bias+assignment+for+dynamic+thermal+clock-skew+compensation."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/mj/TenaceMCMMP14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/t/Tenace:Valerio"))
             "Valerio Tenace")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Miryala:Sandeep"))
             "Sandeep Miryala")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Calimera:Andrea"))
             "Andrea Calimera")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Macii:Alberto"))
             "Alberto Macii")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Macii:Enrico"))
             "Enrico Macii")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/p/Poncino:Massimo"))
             "Massimo Poncino")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Row-based body-bias assignment for dynamic thermal clock-skew compensation.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/mj/mj45.html#TenaceMCMMP14"))
             "Microelectronics Journal 45(5)")
            ": 530-538 (2014)"))
          (li
           (@ (class "entry article") (id "journals/pr/CalimanIR14"))
           (a (@ (name "CalimanIR14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.patcog.2013.08.021"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.patcog.2013.08.021"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/pr/CalimanIR14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/pr/CalimanIR14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/pr/CalimanIR14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/pr/CalimanIR14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/pr/CalimanIR14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Probabilistic+pseudo-morphology+for+grayscale+and+color+images."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Probabilistic+pseudo-morphology+for+grayscale+and+color+images."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Probabilistic+pseudo-morphology+for+grayscale+and+color+images."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Probabilistic+pseudo-morphology+for+grayscale+and+color+images."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/pr/CalimanIR14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Caliman:Alexandru"))
             "Alexandru Caliman")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/i/Ivanovici:Mihai"))
             "Mihai Ivanovici")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Richard:No=euml=l"))
             "No"
             #\ë
             "l Richard")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Probabilistic pseudo-morphology for grayscale and color images.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/pr/pr47.html#CalimanIR14"))
             "Pattern Recognition 47(2)")
            ": 721-735 (2014)"))
          (li
           (@ (class "entry article") (id "journals/ras/SorbelloCCGNI14"))
           (a (@ (name "SorbelloCCGNI14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1016/j.robot.2014.03.017"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1016/j.robot.2014.03.017"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/ras/SorbelloCCGNI14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/ras/SorbelloCCGNI14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/ras/SorbelloCCGNI14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/ras/SorbelloCCGNI14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/ras/SorbelloCCGNI14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Telenoid+android+robot+as+an+embodied+perceptual+social+regulation+medium+engaging+natural+human-humanoid+interaction."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Telenoid+android+robot+as+an+embodied+perceptual+social+regulation+medium+engaging+natural+human-humanoid+interaction."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Telenoid+android+robot+as+an+embodied+perceptual+social+regulation+medium+engaging+natural+human-humanoid+interaction."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Telenoid+android+robot+as+an+embodied+perceptual+social+regulation+medium+engaging+natural+human-humanoid+interaction."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@
                   (href "http://pubzone.org/dblp/journals/ras/SorbelloCCGNI14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Sorbello:Rosario"))
             "Rosario Sorbello")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Chella:Antonio"))
             "Antonio Chella")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Cal=iacute=:Carmelo"))
             "Carmelo Cal"
             #\í)
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/g/Giardina:Marcello"))
             "Marcello Giardina")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/n/Nishio:Shuichi"))
             "Shuichi Nishio")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/i/Ishiguro:Hiroshi"))
             "Hiroshi Ishiguro")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Telenoid android robot as an embodied perceptual social regulation medium engaging natural human-humanoid interaction.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/ras/ras62.html#SorbelloCCGNI14"))
             "Robotics and Autonomous Systems 62(9)")
            ": 1329-1341 (2014)"))
          (li
           (@ (class "entry article") (id "journals/tac/GolLB14"))
           (a (@ (name "GolLB14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1109/TAC.2013.2295664"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1109/TAC.2013.2295664"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/tac/GolLB14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/tac/GolLB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/tac/GolLB14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/tac/GolLB14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/tac/GolLB14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Language-Guided+Controller+Synthesis+for+Linear+Systems."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Language-Guided+Controller+Synthesis+for+Linear+Systems."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Language-Guided+Controller+Synthesis+for+Linear+Systems."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Language-Guided+Controller+Synthesis+for+Linear+Systems."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/tac/GolLB14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/g/Gol:Ebru_Aydin"))
             "Ebru Aydin Gol")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/l/Lazar:Mircea"))
             "Mircea Lazar")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Belta:Calin"))
             "Calin Belta")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Language-Guided Controller Synthesis for Linear Systems.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/tac/tac59.html#GolLB14"))
             "IEEE Trans. Automat. Contr. 59(5)")
            ": 1163-1176 (2014)"))
          (li
           (@ (class "entry article") (id "journals/tac/DingSBR14"))
           (a (@ (name "DingSBR14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1109/TAC.2014.2298143"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1109/TAC.2014.2298143"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/tac/DingSBR14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/tac/DingSBR14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/tac/DingSBR14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/tac/DingSBR14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/tac/DingSBR14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=Optimal+Control+of+Markov+Decision+Processes+With+Linear+Temporal+Logic+Constraints."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=Optimal+Control+of+Markov+Decision+Processes+With+Linear+Temporal+Logic+Constraints."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=Optimal+Control+of+Markov+Decision+Processes+With+Linear+Temporal+Logic+Constraints."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=Optimal+Control+of+Markov+Decision+Processes+With+Linear+Temporal+Logic+Constraints."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/tac/DingSBR14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/d/Ding:Xu_Chu"))
             "Xu Chu Ding")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/s/Smith:Stephen_L="))
             "Stephen L. Smith")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/b/Belta:Calin"))
             "Calin Belta")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/r/Rus:Daniela"))
             "Daniela Rus")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "Optimal Control of Markov Decision Processes With Linear Temporal Logic Constraints.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/tac/tac59.html#DingSBR14"))
             "IEEE Trans. Automat. Contr. 59(5)")
            ": 1244-1257 (2014)"))
          (li
           (@ (class "entry article") (id "journals/tac/CalifanoM14"))
           (a (@ (name "CalifanoM14")))
           (div
            (@ (class "box"))
            (img
             (@
              (alt "")
              (title "Journal Articles")
              (src "http://dblp.uni-trier.de/img/n.png"))))
           (nav
            (@ (class "publ"))
            (ul
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@ (href "http://dx.doi.org/10.1109/TAC.2014.2308606"))
                (img
                 (@
                  (alt "view")
                  (src
                   "http://dblp.uni-trier.de/img/paper.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "view"))
               (ul
                (li
                 (a
                  (@ (href "http://dx.doi.org/10.1109/TAC.2014.2308606"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/paper.dark.16x16.png")
                    (class "icon")))
                  "electronic edition via DOI")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://dblp.uni-trier.de/rec/bibtex/journals/tac/CalifanoM14"))
                (img
                 (@
                  (alt "export")
                  (src
                   "http://dblp.uni-trier.de/img/download.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "export record as"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/bibtex/journals/tac/CalifanoM14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/bibtex.dark.16x16.png")
                    (class "icon")))
                  "BibTeX"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/rdf/journals/tac/CalifanoM14.rdf"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/rdf.dark.16x16.png")
                    (class "icon")))
                  "RDF"))
                (li
                 (a
                  (@
                   (href
                    "http://dblp.uni-trier.de/rec/xml/journals/tac/CalifanoM14.xml"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/xml.dark.16x16.png")
                    (class "icon")))
                  "XML")))
               (p (em "dblp key:"))
               (ul
                (@ (class "bullets"))
                (li
                 (@ (class "select-on-click"))
                 (small "journals/tac/CalifanoM14")))))
             (li
              (@ (class "drop-down"))
              (div
               (@ (class "head"))
               (a
                (@
                 (href
                  "http://google.com/search?q=The+Observer+Error+Linearization+Problem+via+Dynamic+Compensation."))
                (img
                 (@
                  (alt "ask others")
                  (src
                   "http://dblp.uni-trier.de/img/search-external.dark.hollow.16x16.png")
                  (class "icon")))))
              (div
               (@ (class "body"))
               (p (b "ask others"))
               (ul
                (li
                 (a
                  (@
                   (href
                    "http://google.com/search?q=The+Observer+Error+Linearization+Problem+via+Dynamic+Compensation."))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/google.dark.16x16.png")
                    (class "icon")))
                  "Google"))
                (li
                 (a
                  (@
                   (href
                    "http://scholar.google.com/scholar?q=The+Observer+Error+Linearization+Problem+via+Dynamic+Compensation."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/google-scholar.dark.16x16.png")
                    (class "icon")))
                  "Google Scholar"))
                (li
                 (a
                  (@
                   (href
                    "http://academic.research.microsoft.com/search?query=The+Observer+Error+Linearization+Problem+via+Dynamic+Compensation."))
                  (img
                   (@
                    (alt "")
                    (src
                     "http://dblp.uni-trier.de/img/ms-academic.dark.16x16.png")
                    (class "icon")))
                  "MS Academic Search"))
                (li
                 (a
                  (@ (href "http://pubzone.org/dblp/journals/tac/CalifanoM14"))
                  (img
                   (@
                    (alt "")
                    (src "http://dblp.uni-trier.de/img/pubzone.dark.16x16.png")
                    (class "icon")))
                  "PubZone")))))))
           (div
            (@ (class "data"))
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/c/Califano:Claudia"))
             "Claudia Califano")
            ", "
            (a
             (@ (href "http://dblp.uni-trier.de/pers/hd/m/Moog:Claude_H="))
             "Claude H. Moog")
            ":"
            (br)
            " "
            (span
             (@ (class "title"))
             "The Observer Error Linearization Problem via Dynamic Compensation.")
            " "
            (a
             (@
              (href
               "http://dblp.uni-trier.de/db/journals/tac/tac59.html#CalifanoM14"))
             "IEEE Trans. Automat. Contr. 59(9)")
            ": 2502-2508 (2014)")))
         (p
          (@ (id "completesearch-info-skipping") (class "no-js-only"))
          (em "skipping 73 more matches")
          (a
           (@
            (href
             "http://dblp.uni-trier.de/search/publ/inc?q=author%3ACali+year%3A2014&f=30")
            (class "display-none"))))))
       (*COMMENT* " footer ")
       (div (@ (class "clear-both")))
       (div
        (@ (id "footer"))
        (div
         (@ (class "credit"))
         (a
          (@ (href "http://www.uni-trier.de/?L=2"))
          (img
           (@
            (alt "FAQ")
            (src "http://dblp.uni-trier.de/img/utr-logo-bottom.png")
            (title "University of Trier"))))
         " "
         (a
          (@ (href "http://www.dagstuhl.de/en/"))
          (img
           (@
            (alt "FAQ")
            (src "http://dblp.uni-trier.de/img/lzi-logo-bottom.png")
            (title "Schloss Dagstuhl - Leibniz Center for Informatics")))))
        (div
         (@ (class "info"))
         (p
          (a
           (@ (href "http://opendefinition.org/"))
           (img
            (@
             (alt "open data")
             (src "http://dblp.uni-trier.de/img/opendata.80x15.blue.png"))))
          " data released under the "
          (a
           (@ (href "http://opendatacommons.org/licenses/by/summary/"))
           "ODC-BY"
           (& nbsp)
           "1.0 license")
          ". See also our "
          (a
           (@ (href "http://dblp.uni-trier.de/db/copyright.html"))
           "legal information page"))
         (p
          "retrieved 2015-01-07 18:14 CET on data maintained by the "
          (a
           (@ (href "http://dblp.uni-trier.de/db/about/team.html"))
           "dblp team"))))
       "\n")
      "\n"
      (script
       (@
        (src "http://dblp.uni-trier.de/js/dblp-2014-10-23.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/dblp-search-2014-08-07.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/dblp-completesearch-2014-11-20.min.js")
        (type "application/javascript")))
      (script
       (@
        (src "http://dblp.uni-trier.de/js/dblp-ui-2014-06-16.min.js")
        (type "application/javascript")))

     "\n")   
 ))

)