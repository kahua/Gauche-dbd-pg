;;
;; Package Gauche-dbd-pg
;;

(define-gauche-package "Gauche-dbd-pg"
  ;;
  :version "0.4_pre1"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "PostgreSQL binding with dbd interface"

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.11")))

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Kahua project <info@kahua.org>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ("Shiro Kawai <shiro@acm.org>")

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ("BSD")

  ;; Homepage URL, if any.
  :homepage "https://github.com/kahua/Gauche-dbd-pg"

  ;; Repository URL, e.g. github
  :repository "https://github.com/kahua/Gauche-dbd-pg.git"

  :providing-modules (dbd.pg)
  )
