;;; terraform-lang.el --- Terraform / HCL support -*- lexical-binding: t; -*-
;;
;; LSP binary:
;;   brew install hashicorp/tap/terraform-ls
;;   or: download from https://releases.hashicorp.com/terraform-ls/
;;
;; terraform-mode is third-party.

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'"      . terraform-mode)
         ("\\.tfvars\\'"  . terraform-mode))
  :hook (terraform-mode . eglot-ensure)
  :custom
  (terraform-indent-level 2))

(provide 'terraform-lang)
;;; terraform-lang.el ends here
