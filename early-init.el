;;; -*- coding: utf-8; lexical-binding: t; -*-

(setopt user-full-name    ""
        user-mail-address "")

(setq tg/user          (getenv "USER")
      tg/home-path     (getenv "HOME")
      tg/remote-user   tg/user
      tg/remote-hosts  '("192.168.1.1"))

(setq tg/project-recentf-max-load 5
      tg/eclipse-jdtls-path       "eclipse-path"
      tg/lsp-jdk-path             ""
      tg/mvn-settings-file-path   nil ;"path-to-settings/settings.xml"
      tg/lsp-modes                '(java-mode
                                    js-mode
                                    nxml-mode
                                    shell-script-mode
                                    rustic-mode))
