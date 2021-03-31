(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq url-proxy-services
  '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
    ("http" . "proxy.rd.francetelecom.fr:8080")
    ("https" . "proxy.rd.francetelecom.fr:8080")))
