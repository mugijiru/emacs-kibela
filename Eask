;; -*- mode: eask; lexical-binding: t -*-

(package "kibela"
         "2.0.0"
         "Kibela client")

(website-url "https://github.com/mugijiru/emacs-kibela")
(keywords "kibela" "tools")

(package-file "kibela.el")
(files "kibela-auth.el"
       "kibela-markdown-mode.el"
       "kibela-request.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "28.2")
(depends-on "graphql" "0.1.1")
(depends-on "request" "0.3.3")
(depends-on "markdown-mode" "2.5")
(depends-on "edit-indirect" "0.1.10")
(development
 (depends-on "ert-runner")
 (depends-on "noflet")
 (depends-on "undercover")
 (depends-on "with-simulated-input"))
