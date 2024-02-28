;;; -*- coding: utf-8; lexical-binding: t; -*-

(setopt user-full-name    ""
        user-mail-address "")

(setq tg/user          (getenv "USER")
      tg/home-path     (getenv "HOME")
      tg/remote-user   tg/user
      tg/remote-hosts  '() ;"127.0.0.1")
      )

(setq tg/project-recentf-max-load 5
      tg/mvn-settings-file-path   nil ;"path-to-settings/settings.xml"
      tg/lsp-modes                '(java-mode
                                    js-mode
                                    nxml-mode
                                    sh-mode
                                    rustic-mode))
