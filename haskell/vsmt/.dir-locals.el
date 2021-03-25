((nil . ((dante-methods              . (nix-ghci))
         (dante-project-root         . "~/Research/VSmt/haskell/vsmt")
         (dante-repl-command-line    . ("nix-shell" "--pure" "--attr" "vsmt-shell" "release.nix" "--run" "cabal new-repl"))
         (inferior-haskell-root-dir  . "~/Research/VSmt/haskell/vsmt")
         ;; (haskell-process-type       . cabal-repl)
         (haskell-process-wrapper-function . (lambda (args)
                                               (append
                                                (append (list "nix-shell" "--command" )
                                                        (list (mapconcat 'identity args " ")))
                                                (list (nix-current-sandbox))))
                                           )


         )))
;; ((nil
;;   (indent-tabs-mode . nil)
;;   (fill-column . 80)
;;   (buffer-file-coding-system . utf-8-unix))

;;  (haskell-mode
;;   (lsp-haskell-process-args-hie . ("--cwd /home/doyougnu/Research/VSmt/haskell/vsmt .")))
;;  )
